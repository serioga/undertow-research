(ns spin.impl-v2.adapter
  (:require [spin.impl-v2.handler :as handler]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerAdapter
  ""
  (result-context [adapter context])
  (result-error [adapter context throwable])
  (nio? [adapter])
  (blocking-call [adapter f])
  (async-call [adapter f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private ^ThreadLocal thread-nio!
  (ThreadLocal.))

(defn thread-nio?
  ""
  []
  (.get thread-nio!))

(defmacro ^:private in
  [expr context]
  `(try ~expr (catch Throwable t#
                (throw (ex-info "Context handler error" {::context ~context ::throwable t#})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-handlers
  [adapter context handlers]
  (letfn [(reduce* [context prev-context chain]
            (try
              (let [nio (nio? adapter)]
                (.set thread-nio! nio)
                (loop [value context, prev prev-context, chain (seq chain)]
                  (cond
                    value
                    (if-let [context (-> (handler/get-context value) (in prev))]
                      (if chain
                        (if-let [handler (.first chain)]
                          (let [result (-> (handler context) (in context))
                                is-reduced (reduced? result)
                                result (cond-> result is-reduced (deref))
                                chain (when-not is-reduced (.next chain))]
                            (if-let [instant (-> (handler/instant-result result) (in context))]
                              (recur (-> (instant) (in context)) context chain)
                              (if-let [blocking (-> (handler/blocking-result result) (in context))]
                                (if nio
                                  (-> (blocking-call adapter (^:once fn* []
                                                               (reduce* (try (blocking)
                                                                             (catch Throwable t t))
                                                                        context chain)))
                                      (in context))
                                  (recur (-> (blocking) (in context)) context chain))
                                (if-let [async (-> (handler/async-result result) (in context))]
                                  (-> (async-call adapter (^:once fn* []
                                                            (try (async (fn [value] (reduce* value context chain)))
                                                                 (catch Throwable t
                                                                   (reduce* t context chain)))))
                                      (in context))
                                  (-> (throw (ex-info (str "Cannot handle result: " result) {}))
                                      (in context))))))
                          ;; handler is falsy, skip
                          (recur value prev (.next chain)))
                        ;; chain is empty, complete
                        (if-let [chain+ (:spin/response-handlers context)]
                          (recur (dissoc context :spin/response-handlers) prev (seq chain+))
                          (result-context adapter context)))
                      ;; threat value as handler(s), or fail
                      (recur prev nil (-> (handler/prepend-handlers value chain) (in prev))))
                    prev
                    (recur prev nil chain)
                    :else
                    (throw (ex-info "Handle empty context" {::chain chain})))))
              (catch Throwable t
                (let [{::keys [context throwable]
                       :or {context context, throwable t}} (ex-data t)]
                  (if-let [error-handlers (seq (:spin/error-handlers context))]
                    (try
                      ;; remove `:spin/error-handlers` from context just in case if error handlers fail
                      (let [context* (dissoc context :spin/error-handlers)]
                        (->> (concat error-handlers (handler/handler-seq (constantly throwable)))
                             (map #(fn as-handler [ctx]
                                     ;; check if prev handler returns new context
                                     (if (identical? ctx context*)
                                       (% ctx throwable)
                                       (reduced ctx))))
                             (reduce* context* nil)))
                      (catch Throwable t
                        (result-error adapter context t)))
                    (result-error adapter context throwable)))))
            ;; always return nil, provide result to `adapter`
            nil)]
    (assert (map? context) (str "Requires context map to apply handlers "
                                {:context context :handlers handlers}))
    (reduce* context nil (some-> handlers (handler/handler-seq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
