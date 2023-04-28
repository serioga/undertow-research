(ns spin-undertow.handler
  (:refer-clojure :exclude [get])
  (:require [spin.request :as req]
            [spin.handler :as handler]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang IPersistentMap MultiFn)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util HeaderMap HttpString Methods SameThreadExecutor)
           (java.nio.charset Charset)
           (java.util ArrayDeque Collection)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti exchange-get
  ""
  req/get-dispatch-key)

(defn extend-request
  ""
  [f & ks]
  (doseq [k ks]
    (.addMethod ^MultiFn exchange-get k f)))

(defn- get-request-server-port
  [^HttpServerExchange e _] (.getPort (.getDestinationAddress e)))

(defn- get-request-server-name
  [^HttpServerExchange e _] (.getHostName e))

(defn- get-request-remote-addr
  [^HttpServerExchange e _] (.getHostAddress (.getAddress (.getSourceAddress e))))

(defn- get-request-uri
  [^HttpServerExchange e _] (.getRequestURI e))

(defn- get-request-query-string
  [^HttpServerExchange e _] (as-> (.getQueryString e) s
                                  (when-not (.isEmpty s) s)))

(defn- get-request-scheme
  [^HttpServerExchange e _] (as-> (.getRequestScheme e) s
                                  (case s "http" :http "https" :https
                                          (keyword (.toLowerCase ^String s)))))

(defn- get-request-method
  [^HttpServerExchange e _] (as-> (.toString (.getRequestMethod e)) s
                                  (case s "GET" :get "POST" :post "PUT" :put
                                          "DELETE" :delete "HEAD" :head "OPTIONS" :options
                                          (keyword (.toLowerCase ^String s)))))

(defn- get-request-method-get?
  [^HttpServerExchange e _] (.equals Methods/GET (.getRequestMethod e)))

(defn- get-request-method-post?
  [^HttpServerExchange e _] (.equals Methods/POST (.getRequestMethod e)))

;; TODO: dispatch blocking when work with input stream
(defn- get-request-body
  [^HttpServerExchange e _] (exchange/get-input-stream e))

(defn- get-request-header
  [^HttpServerExchange e _ x] (.getFirst (.getRequestHeaders e) ^String x))

(defn- get-request-header*
  [^HttpServerExchange e _ x] (.get (.getRequestHeaders e) ^String x))

(defn- get-request-query-param
  [^HttpServerExchange e _ x] (some-> ^ArrayDeque (.get (.getQueryParameters e) x)
                                      (.peekFirst)))

(defn- get-request-query-param*
  [^HttpServerExchange e _ x] (seq (.get (.getQueryParameters e) x)))

(defn- get-request-cookie
  [^HttpServerExchange e _ x] (some-> (.getRequestCookie e x) (.getValue)))

(defn- get-request-cookie-info
  [^HttpServerExchange e _ x] (when-let [c (.getRequestCookie e x)]
                                ;; TODO: cookie map fields
                                ;; TODO: skip empty fields?
                                {:name (.getName c)
                                 :value (.getValue c)
                                 :path (.getPath c)}))

(extend-request get-request-server-port, :server-port)
(extend-request get-request-server-name, :server-name :server-host)
(extend-request get-request-remote-addr, :remote-addr)
(extend-request get-request-uri,,,,,,,,, :uri)
(extend-request get-request-query-string :query-string)
(extend-request get-request-scheme,,,,,, :scheme)
(extend-request get-request-method,,,,,, :method :request-method)
;; TODO: more predicates for request method
(extend-request get-request-method-get?, :method-get?)
(extend-request get-request-method-post? :method-post?)
(extend-request get-request-body,,,,,,,, :body :input-stream)
(extend-request get-request-header,,,,,, :header)
(extend-request get-request-header*,,,,, :header*)
(extend-request get-request-query-param, :query-param)
(extend-request get-request-query-param* :query-param*)
(extend-request get-request-cookie,,,,,, :cookie)
(extend-request get-request-cookie-info, :cookie-info)

;; TODO: protocol, path-info

(extend-protocol req/IRequestView HttpServerExchange
  (get-fn
    [_ k]
    (get-method exchange-get k))
  (get-methods*
    [_]
    (methods exchange-get)))

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
