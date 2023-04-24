(ns undertow-spin.handler
  (:require [spin.request :as req]
            [spin.response :as resp]
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
    [e h] (.getFirst (.getRequestHeaders e) ^String h))
  (header-seq
    [e h] (.get (.getRequestHeaders e) ^String h)))

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

(extend-protocol SpinResponseBody String
  (handle-response-body
    [string, ^HttpServerExchange exchange]
    (-> (.getResponseSender exchange)
        (.send string (Charset/forName (.getResponseCharset exchange))))))

(defn handle-context
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

(defn handle-instant
  [result, ^HttpServerExchange exchange]
  (when-let [instant (resp/instant result)]
    #_#p (.getName (Thread/currentThread))
    #_#p (instant)
    (handle-context (instant) exchange)
    'handle-instant))

(defn handle-blocking
  [result, ^HttpServerExchange exchange]
  (when-let [blocking (resp/blocking result)]
    (if (.isInIoThread exchange)
      (->> ^Runnable
           (^:once fn* [] (handle-context (blocking) exchange))
           (.dispatch exchange))
      (handle-context (blocking) exchange))
    'handle-blocking))

(defn async-callback [exchange] #(handle-context % exchange))

(defn handle-async
  [result, ^HttpServerExchange exchange]
  (when-let [async (resp/async result)]
    (if (.isDispatched exchange)
      (async (async-callback exchange))
      (->> ^Runnable
           (^:once fn* [] (async (async-callback exchange)))
           (.dispatch exchange SameThreadExecutor/INSTANCE)))
    'handle-async))

(defn handle-result
  [result, exchange]
  (or (handle-instant result exchange)
      (handle-blocking result exchange)
      (handle-async result exchange)
      (throw (ex-info (str "Missing handler for " (pr-str result)) {}))))

(defn http-handler
  ""
  [handler-fn]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (-> {:request exchange :start-time (System/nanoTime)}
          (handler-fn)
          (handle-result exchange)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
