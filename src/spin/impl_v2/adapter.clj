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
  (letfn [(reduce* [context chain prev-context]
            (try
              (loop [value context, chain (seq chain), prev prev-context]
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
                            (recur (-> (instant) (throw* context)) chain context)
                            (if-let [blocking (-> (handler/blocking-result result) (throw* context))]
                              (if (-> (nio? adapter) (throw* context))
                                (-> (blocking-call adapter (^:once fn* []
                                                             (reduce* (try (blocking)
                                                                           (catch Throwable t t))
                                                                      chain context)))
                                    (throw* context))
                                (recur (-> (blocking) (throw* context)) chain context))
                              (if-let [async (-> (handler/async-result result) (throw* context))]
                                (-> (async-call adapter (^:once fn* []
                                                          (try (async (fn [value] (reduce* value chain context)))
                                                               (catch Throwable t
                                                                 (reduce* t chain context)))))
                                    (throw* context))
                                (-> (throw (ex-info (str "Cannot handle result: " result) {}))
                                    (throw* context))))))
                        ;; handler is falsy, skip
                        (recur value (next chain) prev))
                      ;; chain is empty, complete
                      (result-context adapter context))
                    (if-let [chain+ (-> (handler/value-handlers value) (throw* prev))]
                      (recur prev (concat chain+ chain) nil)
                      (-> (throw (ex-info (str "Handler result value is not context or handlers: " value)
                                          {::value value ::chain chain}))
                          (throw* prev))))
                  prev
                  (recur prev chain nil)
                  :else
                  (throw (ex-info "Handle empty context" {::chain chain}))))
              (catch Throwable t
                (let [{::keys [context throwable]} (ex-data t)
                      handlers (:spin/error-handlers context)]
                  (try
                    (if-let [context (and handlers (as-> (dissoc context :spin/error-handlers) context
                                                         (some (fn [handler] (handler context throwable)) handlers)))]
                      (result-context adapter context)
                      (result-error adapter (or throwable t)))
                    (catch Throwable t
                      (result-error adapter t))))))
            ;; always return nil, provide result to `adapter`
            nil)]
    (assert (map? context) (str "Requires context map to apply handlers "
                                {:context context :handlers handlers}))
    (reduce* context (some-> handlers (handler/value-handlers)) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
