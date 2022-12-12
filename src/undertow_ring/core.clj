(ns undertow-ring.core
  (:require [undertow-ring.impl.request :as request]
            [undertow-ring.impl.response :as response]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-async-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::async))

(defn as-non-blocking-sync-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::sync-non-blocking))

(defmulti fn-as-handler (comp ::handler-type meta))

(defmethod fn-as-handler nil
  [handler]
  (-> (reify HttpHandler
        (handleRequest [_ exchange]
          (-> (request/build-request-map exchange)
              (handler)
              (response/handle-response exchange))))
      (handler/dispatch)))

(defmethod fn-as-handler ::async
  [handler]
  ;; TODO: Async exception handler
  (letfn [(exception-callback [e] (throw e))]
    (reify HttpHandler
      (handleRequest [_ e]
        (exchange/dispatch-async e
          (handler (request/build-request-map e)
                   (fn response-callback [response]
                     (response/handle-response response e))
                   exception-callback))))))

(defmethod fn-as-handler ::sync-non-blocking
  [handler]
  (reify HttpHandler
    (handleRequest [this e]
      (if (and (.isInIoThread e)
               (not (.isRequestComplete e)))
        ;; Dispatch incomplete request to worker thread
        (.dispatch e this)
        ;; Execute handler on IO thread
        (-> (request/build-request-map e)
            (handler)
            (response/handle-response e))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn websocket?
  [req]
  (some-> req :headers ^String (get "upgrade")
          (.equalsIgnoreCase "websocket")))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
