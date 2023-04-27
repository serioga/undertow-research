(ns spin-undertow.handler
  (:require [spin.request :as req]
            [spin.handler :as handler]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang IPersistentMap)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util HeaderMap HttpString SameThreadExecutor)
           (java.nio.charset Charset)
           (java.util Collection)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn method-keyword
  [s]
  (case s "GET" :get "POST" :post "PUT" :put "DELETE" :delete "HEAD" :head "OPTIONS" :options
          (keyword (.toLowerCase ^String s))))

(defn scheme-keyword
  [s]
  (case s "http" :http "https" :https
          (keyword (.toLowerCase ^String s))))

(extend-protocol req/ISpinRequest HttpServerExchange
  (internal [e] e)
  ;; TODO: exchange request as-map
  (as-map [e] {})
  (server-port [e], (.getPort (.getDestinationAddress e)))
  (server-name [e], (.getHostName e))
  (remote-addr [e], (.getHostAddress (.getAddress (.getSourceAddress e))))
  (uri [e],,,,,,,,, (.getRequestURI e))
  ;; TODO: nil uri for empty string
  (query-string [e] (.getQueryString e))
  (scheme [e],,,,,, (scheme-keyword (.getRequestScheme e)))
  (method [e],,,,,, (method-keyword (.toString (.getRequestMethod e))))
  ;; TODO: dispatch blocking when work with input stream
  (body [e],,,,,,,, (exchange/get-input-stream e))
  ;; TODO: path-info
  (path-info [e],,,)
  ;; TODO: protocol
  (protocol [e],,,,)
  (header
    [e n] (.getFirst (.getRequestHeaders e) ^String n))
  (header*
    [e n] (.get (.getRequestHeaders e) ^String n))
  (cookie
    [e n] (some-> (.getRequestCookie e n) (.getValue)))
  (cookie*
    [e n] (when-let [c (.getRequestCookie e n)]
            ;; TODO: cookie map fields
            ;; TODO: skip empty fields?
            {:name (.getName c)
             :value (.getValue c)
             :path (.getPath c)})))

(defn- put-headers!
  [exchange headers]
  (reduce (fn [^HeaderMap hm [k v]]
            (cond (sequential? v)
                  (-> hm (.putAll (HttpString. (str k))
                                  ^Collection (map str v)))
                  (some? v)
                  (-> hm (.put (HttpString. (str k)) (str v)))
                  :else
                  (-> hm (.remove (HttpString. (str k))))))
          (.getResponseHeaders ^HttpServerExchange exchange)
          headers))

(defprotocol SpinResponse
  (handle-response [response exchange]))

(defprotocol SpinResponseBody
  (handle-response-body [body exchange]))

(extend-protocol SpinResponse IPersistentMap
  (handle-response
    [response ^HttpServerExchange exchange]
    (when-some [headers,, (.valAt response :headers)] (doto exchange (put-headers! headers)))
    (when-some [status,,, (.valAt response :status)], (.setStatusCode exchange status))
    ;(when-some [session (.entryAt response :session)] (doto exchange (session/update-values (val session))))
    (when-some [body,,,,, (.valAt response :body)],,, (handle-response-body body exchange))
    nil))

;; Allow just a body in response
(extend-protocol SpinResponse Object
  (handle-response
    [object, exchange]
    (handle-response-body object exchange)))

(extend-protocol SpinResponseBody String
  (handle-response-body
    [string, ^HttpServerExchange exchange]
    (-> (.getResponseSender exchange)
        (.send string (Charset/forName (.getResponseCharset exchange))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-result-context
  [^IPersistentMap context, ^HttpServerExchange exchange]
  ;; TODO: apply prepending context transformations
  (when context
    ;; Add prepending response headers from context.
    (some->> (.valAt context :response/headers)
             ;; TODO: not-empty here?
             (put-headers! exchange))
    (when-some [status (.valAt context :response/status)] (.setStatusCode exchange status))
    (some-> (.valAt context :response) (handle-response exchange))

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
      (-> {:request exchange #_#_:start-time (System/nanoTime)}
          (handler-fn)
          (handle-result exchange)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
