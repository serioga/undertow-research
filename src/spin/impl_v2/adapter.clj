(ns spin.impl-v2.adapter
  (:require [spin.impl-v2.handler :as handler]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerAdapter
  ""
  (result-context [adapter context])
  (result-error [adapter throwable])
  (nio? [adapter])
  (blocking-call [adapter f])
  (async-call [adapter f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ^:private throw*
  [expr context]
  `(try ~expr (catch Throwable t#
                (throw (ex-info "Context handler error" {::context ~context ::throwable t#})))))

(defn run-handlers
  [adapter context handlers]
  (letfn [(reduce* [context prev-context chain]
            (try
              (loop [value context, prev prev-context, chain (seq chain)]
                (cond
                  value
                  (if-let [context (-> (handler/value-context value) (throw* prev))]
                    (if chain
                      (if-let [handler (first chain)]
                        (let [result (-> (handler context) (throw* context))
                              is-reduced (reduced? result)
                              result (cond-> result is-reduced (deref))
                              chain (when-not is-reduced (next chain))]
                          (if-let [instant (-> (handler/instant-result result) (throw* context))]
                            (recur (-> (instant) (throw* context)) context chain)
                            (if-let [blocking (-> (handler/blocking-result result) (throw* context))]
                              (if (-> (nio? adapter) (throw* context))
                                (-> (blocking-call adapter (^:once fn* []
                                                             (reduce* (try (blocking)
                                                                           (catch Throwable t t))
                                                                      context chain)))
                                    (throw* context))
                                (recur (-> (blocking) (throw* context)) context chain))
                              (if-let [async (-> (handler/async-result result) (throw* context))]
                                (-> (async-call adapter (^:once fn* []
                                                          (try (async (fn [value] (reduce* value context chain)))
                                                               (catch Throwable t
                                                                 (reduce* t context chain)))))
                                    (throw* context))
                                (-> (throw (ex-info (str "Cannot handle result: " result) {}))
                                    (throw* context))))))
                        ;; handler is falsy, skip
                        (recur value prev (next chain)))
                      ;; chain is empty, complete
                      (result-context adapter context))
                    (if-let [chain+ (-> (handler/value-handlers value) (throw* prev))]
                      (recur prev nil (concat chain+ chain))
                      (-> (throw (ex-info (str "Handler result value is not context or handlers: " value)
                                          {::value value ::chain chain}))
                          (throw* prev))))
                  prev
                  (recur prev nil chain)
                  :else
                  (throw (ex-info "Handle empty context" {::chain chain}))))
              (catch Throwable t
                (let [{::keys [context throwable]} (ex-data t)]
                  (if-let [error-handlers (seq (:spin/error-handlers context))]
                    ;; remove `:spin/error-handlers` from context just in case if error handlers fail
                    (let [context* (dissoc context :spin/error-handlers)
                          as-handler (fn [error-handler] (fn as-handler [ctx]
                                                           ;; check if prev handler returns new context
                                                           (if (identical? ctx context*)
                                                             (error-handler ctx throwable)
                                                             (reduced ctx))))]
                      (reduce* context* nil (concat (map as-handler error-handlers)
                                                    ;; error if none of error handlers returns new context
                                                    (handler/value-handlers (constantly throwable)))))
                    (result-error adapter (or throwable t))))))
            ;; always return nil, provide result to `adapter`
            nil)]
    (assert (map? context) (str "Requires context map to apply handlers "
                                {:context context :handlers handlers}))
    (reduce* context nil (some-> handlers (handler/value-handlers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
