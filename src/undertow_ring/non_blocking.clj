(ns undertow-ring.non-blocking
  (:require [undertow-ring.core :as ring]
            [undertow-ring.impl.request :as request]
            [undertow-ring.impl.response :as response])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-non-blocking-sync-handler
  [handler]
  (vary-meta handler assoc ::ring/handler-type ::sync-non-blocking-handler))

(defmethod ring/fn-as-handler ::sync-non-blocking-handler
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
