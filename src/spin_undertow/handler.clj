(ns spin-undertow.handler
  (:require [spin-undertow.request :as request]
            [spin-undertow.response :as response]
            [spin.handler :as handler]
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

(defn- handle-instant-result
  [instant-result-fn, ^HttpServerExchange e]
  (try
    (handle-result-context (instant-result-fn) e)
    (catch Throwable t (exchange/throw* e t)))
  'handle-instant-result)

(declare handle-result)

(defn- handle-blocking-result
  [blocking-result-fn, ^HttpServerExchange e]
  (if (.isInIoThread e)
    (->> ^Runnable
         (^:once fn* [] (try (handle-result (blocking-result-fn) e)
                             (catch Throwable t (exchange/throw* e t))))
         (.dispatch e))
    (handle-result (blocking-result-fn) e))
  'handle-blocking-result)

(defn- handle-async-result
  [async-result-fn, ^HttpServerExchange e]
  (letfn [(async-callback [result]
            (handle-result result e))]
    (if (.isDispatched e)
      (async-result-fn (async-callback e))
      (->> ^Runnable
           (^:once fn* []
             (try (async-result-fn (async-callback e))
                  (catch Throwable t (exchange/throw* e t))))
           (.dispatch e SameThreadExecutor/INSTANCE)))
    'handle-async-result))

(defn- handle-result
  [result, exchange]
  (or (some-> (handler/instant-result-fn result),,,, (handle-instant-result exchange))
      (some-> (handler/blocking-result-fn result),,, (handle-blocking-result exchange))
      (some-> (handler/async-result-fn result),,,,,, (handle-async-result exchange))
      (throw (ex-info (str "Missing handler for " (pr-str result)) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn http-handler
  ""
  [handler-fn]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (-> {:request (request/create-request exchange) #_#_:start-time (System/nanoTime)}
          (handler-fn)
          (handle-result exchange)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
