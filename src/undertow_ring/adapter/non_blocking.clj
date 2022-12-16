(ns undertow-ring.adapter.non-blocking
  (:require [undertow-ring.adapter :as adapter]
            [undertow-ring.impl.request :as request]
            [undertow-ring.impl.response :as response])
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-non-blocking-sync-handler
  [handler]
  (vary-meta handler assoc ::adapter/handler-type ::sync-non-blocking-handler))

(defmethod adapter/fn-as-handler ::sync-non-blocking-handler
  [handler]
  (reify HttpHandler
    (handleRequest [this e]
      (if (and (.isInIoThread e)
               (not (.isRequestComplete e)))
        ;; Dispatch incomplete request to worker thread
        (.dispatch e this)
        ;; Execute handler on IO thread
        (-> (request/build-request e)
            (handler)
            (response/handle-response e))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
