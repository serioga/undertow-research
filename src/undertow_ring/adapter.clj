(ns undertow-ring.adapter
  (:require [undertow-ring.impl.ring-request :as ring-request]
            [undertow-ring.impl.ring-response :as ring-response]
            [undertow.adapter :as adapter]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti fn-as-handler (comp ::handler-type meta))

;; **Synchronous** handlers take one argument, a map representing a HTTP
;; request, and return a map representing the HTTP response.

(defmethod fn-as-handler nil
  [ring-handler]
  (handler/force-dispatch
    (reify HttpHandler
      (handleRequest [_ exchange]
        (-> (ring-request/build-request exchange)
            (ring-handler)
            (ring-response/handle-response exchange))))))

;; Handlers may also be **asynchronous**. Handlers of this type take three
;; arguments: the request map, a response callback and an exception callback.

(defmethod fn-as-handler ::async-handler
  [ring-handler]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (exchange/async-dispatch exchange
        (ring-handler (ring-request/build-request exchange)
                      (fn handle-async [response] (ring-response/handle-response response exchange))
                      (partial exchange/throw* exchange))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn enable-ring-handler
  []
  (adapter/set-fn-as-handler fn-as-handler))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-async-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::async-handler))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
