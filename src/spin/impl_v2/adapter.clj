(ns spin.impl-v2.adapter
  (:require [spin.impl-v2.handler :as handler]))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerAdapter
  ""
  (complete-context [adapter context])
  (complete-error [adapter throwable])
  (nio-thread? [adapter])
  (dispatch-blocking [adapter f])
  (dispatch-async [adapter f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ^:private throw*
  [expr context]
  `(try ~expr (catch Throwable t#
                (throw (ex-info "Context handler error" {::context ~context ::throwable t#})))))

(defn run-handlers
  [adapter context handlers]
  (letfn [(reduce* [prev context chain]
            (try
              (loop [prev prev, value context, chain (seq chain)]
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
                            (recur context (-> (instant) (throw* context)) chain)
                            (if-let [blocking (-> (handler/blocking-result result) (throw* context))]
                              (if (nio-thread? adapter)
                                (dispatch-blocking adapter (^:once fn* [] (reduce* context (blocking) chain)))
                                (recur context (-> (blocking) (throw* context)) chain))
                              (if-let [async (-> (handler/async-result result) (throw* context))]
                                (dispatch-async adapter (^:once fn* [] (async (fn [result] (reduce* context result chain)))))
                                (-> (throw (ex-info (str "Cannot handle result: " result) {}))
                                    (throw* context))))))
                        ;; handler is falsy, skip
                        (recur prev value (next chain)))
                      ;; chain is empty, complete
                      (complete-context adapter context))
                    (if-let [chain+ (-> (handler/value-handlers value) (throw* prev))]
                      (recur nil prev (concat chain+ chain))
                      (-> (throw (ex-info (str "Handler result value is not context or handlers: " value)
                                          {::value value ::chain chain}))
                          (throw* prev))))
                  prev
                  (recur nil prev chain)
                  :else
                  (throw (ex-info "Handle empty context" {::chain chain}))))
              (catch Throwable t
                (let [{::keys [context throwable]} (ex-data t)
                      handlers (:spin/error-handlers context)]
                  (try
                    (if-let [context (and handlers (as-> (dissoc context :spin/error-handlers) context
                                                         (some (fn [handler] (handler context throwable)) handlers)))]
                      (complete-context adapter context)
                      (complete-error adapter (or throwable t)))
                    (catch Throwable t
                      (complete-error adapter t))))))
            ;; always return nil, provide result to `impl`
            nil)]
    (assert (map? context) (str "Requires context map to apply handlers "
                                {:context context :handlers handlers}))
    (reduce* nil context (some-> handlers (handler/value-handlers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
