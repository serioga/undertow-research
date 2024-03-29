(ns spin.impl-v3.adapter
  (:require [spin.impl-v3.handler :as handler]))

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
                (when nio (.set thread-nio! nio))
                (loop [result context, prev prev-context, chain (seq chain)]
                  (cond
                    result
                    (if-let [context (-> (handler/result-context result) (in prev))]
                      (if chain
                        (if-let [handler (.first chain)]
                          (let [handler-type (handler/handler-type handler)]
                            (cond
                              ;; non-blocking handler function
                              (not handler-type)
                              (recur (-> (handler context) (in context)) context (.next chain))
                              ;; blocking handler
                              (.equals :blocking-handler handler-type)
                              (if nio
                                (-> (blocking-call adapter (^:once fn* []
                                                             (reduce* (try (handler/invoke-blocking handler context)
                                                                           (catch Throwable t t))
                                                                      context (.next chain))))
                                    (in context))
                                (recur (-> (handler/invoke-blocking handler context) (in context)) context (.next chain)))
                              ;; async handler
                              (.equals :async-handler handler-type)
                              (-> (async-call adapter (^:once fn* []
                                                        (try (handler/invoke-async handler context (fn [value] (reduce* value context (.next chain))))
                                                             (catch Throwable t
                                                               (reduce* t context (.next chain))))))
                                  (in context))
                              :else
                              (-> (throw (ex-info (str "Invalid handler type: " handler-type) {}))
                                  (in context))))
                          ;; handler is falsy, skip
                          (recur result prev (.next chain)))
                        ;; chain is empty, complete
                        (if-let [chain+ (:spin/response-handlers context)]
                          (recur (dissoc context :spin/response-handlers) prev (seq chain+))
                          (result-context adapter context)))
                      ;; threat value as handler chain, or fail
                      (recur prev nil (-> (handler/result-chain result chain) (in prev))))
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
