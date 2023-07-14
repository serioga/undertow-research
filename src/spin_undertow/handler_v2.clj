(ns spin-undertow.handler-v2
  (:require [spin-undertow.request :as request]
            [spin-undertow.response :as response]
            [spin.handler-v2 :as handler]
            [spin.impl-v2.adapter :as adapter]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang IPersistentMap)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util SameThreadExecutor)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-result-context
  [^IPersistentMap context, ^HttpServerExchange e]
  ;; TODO: apply prepending context transformations
  (when context
    ;; Add prepending response headers from context.
    (some->> (.valAt context :response/headers)
             ;; TODO: not-empty here?
             (response/put-headers! e))
    (when-some [status (.valAt context :response/status)] (.setStatusCode e status))
    (some-> (.valAt context :response) (response/handle-response e))

    #_(let [end-time (System/nanoTime)]
        #p (- end-time (:start-time context))))

  (.endExchange e))

(extend-protocol adapter/HandlerAdapter HttpServerExchange
  (result-context,,, [e context] (handle-result-context context e))
  (result-error,,,,, [e context throwable] (exchange/throw* e throwable))
  (nio?,,,,,,,,,,,,, [e] (.isInIoThread e))
  (blocking-call,,,, [e f] (.dispatch e ^Runnable f))
  (async-call,,,,,,, [e f] (.dispatch e SameThreadExecutor/INSTANCE ^Runnable f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn http-handler
  ""
  ([handlers] (http-handler handlers nil))
  ([handlers {:keys [error-handler response-handler]}]
   (let [context (cond-> {}
                   error-handler (handler/set-error-handler error-handler)
                   response-handler (handler/set-response-handler response-handler))]
     (reify HttpHandler
       (handleRequest [_ exchange]
         (let [context (assoc context :request (request/create-request exchange) #_#_:start-time (System/nanoTime))]
           (adapter/run-handlers exchange context handlers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
