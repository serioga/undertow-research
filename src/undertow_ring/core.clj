(ns undertow-ring.core
  (:require [undertow-ring.impl.request :as request]
            [undertow-ring.impl.response :as response]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti fn-as-handler (comp ::handler-type meta))

(defmethod fn-as-handler nil
  [handler]
  (-> (reify HttpHandler
        (handleRequest [_ exchange]
          (-> (request/build-request-map exchange)
              (handler)
              (response/handle-response exchange))))
      (handler/dispatch)))

(defmethod fn-as-handler ::async-handler
  [handler]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (exchange/dispatch-async exchange
        (handler (request/build-request-map exchange)
                 (fn handle-async-response [response]
                   (response/handle-response response exchange))
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
