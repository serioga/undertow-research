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
  [^IPersistentMap context, ^HttpServerExchange exchange]
  ;; TODO: apply prepending context transformations
  (when context
    ;; Add prepending response headers from context.
    (some->> (.valAt context :response/headers)
             ;; TODO: not-empty here?
             (response/put-headers! exchange))
    (when-some [status (.valAt context :response/status)] (.setStatusCode exchange status))
    (some-> (.valAt context :response) (response/handle-response exchange))

    #_(let [end-time (System/nanoTime)]
        #p (- end-time (:start-time context))))

  (.endExchange exchange))

(defn- handle-instant-result
  [instant-result-fn, ^HttpServerExchange exchange]
  (try
    (handle-result-context (instant-result-fn) exchange)
    (catch Throwable t (exchange/throw* exchange t)))
  'handle-instant-result)

(declare handle-result)

(defn- handle-blocking-result
  [blocking-result-fn, ^HttpServerExchange exchange]
  (if (.isInIoThread exchange)
    (->> ^Runnable
         (^:once fn* [] (try (handle-result (blocking-result-fn) exchange)
                             (catch Throwable t (exchange/throw* exchange t))))
         (.dispatch exchange))
    (handle-result (blocking-result-fn) exchange))
  'handle-blocking-result)

(defn- handle-async-result
  [async-result-fn, ^HttpServerExchange exchange]
  (letfn [(async-callback [result]
            (handle-result result exchange))]
    (if (.isDispatched exchange)
      (async-result-fn (async-callback exchange))
      (->> ^Runnable
           (^:once fn* []
             (try (async-result-fn (async-callback exchange))
                  (catch Throwable t (exchange/throw* exchange t))))
           (.dispatch exchange SameThreadExecutor/INSTANCE)))
    'handle-async-result))

(defn- handle-result
  [result, exchange]
  (or (some-> (handler/instant-result-fn result), (handle-instant-result exchange))
      (some-> (handler/blocking-result-fn result) (handle-blocking-result exchange))
      (some-> (handler/async-result-fn result),,, (handle-async-result exchange))
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
