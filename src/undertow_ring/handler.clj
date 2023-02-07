(ns undertow-ring.handler
  (:require [undertow-ring.impl.ring-request :as ring-request]
            [undertow-ring.impl.ring-response :as ring-response]
            [undertow.api.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler)
           (io.undertow.util WorkerUtils)
           (java.util.concurrent TimeUnit TimeoutException)))

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
  [handler-fn timeout-ms]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (let [timeout-task (^:once fn* [] (->> (str "Async ring response timeout: " timeout-ms " ms")
                                             (TimeoutException.)
                                             (exchange/throw* exchange)))
            timeout-key (-> (.getIoThread exchange)
                            (WorkerUtils/executeAfter timeout-task timeout-ms
                                                      TimeUnit/MILLISECONDS))]
        (exchange/async-dispatch exchange
          (handler-fn (ring-request/build-request exchange)
                      (fn handle-async [response]
                        ;; Handle response only before timeout
                        (when (.remove timeout-key)
                          (ring-response/handle-response response exchange)))
                      (fn handle-error [throwable]
                        (.remove timeout-key)
                        (exchange/throw* exchange throwable))))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
