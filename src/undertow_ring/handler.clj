(ns undertow-ring.handler
  (:require [undertow-ring.impl.ring-request :as ring-request]
            [undertow-ring.impl.ring-response :as ring-response]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn sync-ring-handler
  "Returns HttpHandler for **synchronous** ring handler function.

  The function `handler-fn` takes one argument, a map representing a HTTP
  request, and return a map representing the HTTP response."
  [handler-fn]
  (handler/force-dispatch
    (reify HttpHandler
      (handleRequest [_ exchange]
        (-> (ring-request/build-request exchange)
            (handler-fn)
            (ring-response/handle-response exchange))))))

(defn async-ring-handler
  "Returns HttpHandler for **asynchronous** ring handler function.

  The function `handler-fn` takes three arguments: the request map, a response
  callback and an exception callback."
  [handler-fn]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (exchange/async-dispatch exchange
        (handler-fn (ring-request/build-request exchange)
                    (fn handle-async [response] (ring-response/handle-response response exchange))
                    (partial exchange/throw* exchange))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
