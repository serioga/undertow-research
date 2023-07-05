(ns spin-undertow.handler-v2
  (:require [spin-undertow.request :as request]
            [spin-undertow.response :as response]
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
  (complete-context,,,, [e context] (handle-result-context context e))
  (complete-error,,,,,, [e throwable] (exchange/throw* e throwable))
  (nio-thread?,,,,,,,,, [e] (.isInIoThread e))
  (dispatch-blocking,,, [e f] (.dispatch e ^Runnable f))
  (dispatch-async,,,,,, [e f] (.dispatch e SameThreadExecutor/INSTANCE ^Runnable f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn http-handler
  ""
  [handlers]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (let [context {:request (request/create-request exchange) #_#_:start-time (System/nanoTime)}]
        (adapter/run-handlers exchange context handlers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
