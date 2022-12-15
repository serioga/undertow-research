(ns undertow-ring.core
  (:require [undertow-ring.impl.request :as request]
            [undertow-ring.impl.response :as response]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti fn-as-handler (comp ::handler-type meta))

;; *Synchronous* handlers take one argument, a map representing a HTTP request,
;; and return a map representing the HTTP response.

(defmethod fn-as-handler nil
  [ring-handler]
  (handler/force-dispatch
    (reify HttpHandler
      (handleRequest [_ exchange]
        (-> (request/build-request exchange)
            (ring-handler)
            (response/handle-response exchange))))))

;; Handlers may also be *asynchronous*. Handlers of this type take three
;; arguments: the request map, a response callback and an exception callback.

(defmethod fn-as-handler ::async-handler
  [ring-handler]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (exchange/async-dispatch exchange
        (ring-handler (request/build-request exchange)
                      (fn handle-async [response] (response/handle-response response exchange))
                      (partial exchange/throw* exchange))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-async-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::async-handler))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn websocket?
  [req]
  (some-> req :headers ^String (get "upgrade")
          (.equalsIgnoreCase "websocket")))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
