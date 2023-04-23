(ns undertow-spin.handler
  (:require [spin.request :as req]
            [spin.response :as resp]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang IPersistentMap)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util SameThreadExecutor)
           (java.nio.charset Charset)))

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

(defprotocol ResponseBody
  (handle-response-body [body exchange]))

(extend-protocol ResponseBody String
  (handle-response-body
    [string, ^HttpServerExchange exchange]
    (-> (.getResponseSender exchange)
        (.send string (Charset/forName (.getResponseCharset exchange))))))

(declare handle-context)

(defn handle-instant
  [context, ^HttpServerExchange exchange]
  (when-let [instant (resp/instant context)]
    #_#p (.getName (Thread/currentThread))
    #_#p (instant)
    ;; TODO: apply prepending context transformations
    ;; TODO: add prepending response headers
    (let [^IPersistentMap context (instant)]
      (when-some [status (.valAt context :response/status)]
        (doto exchange (.setStatusCode status)))
      ;; TODO: put headers
      (when-some [body (.valAt context :response/body)]
        (handle-response-body body exchange))

      #_(let [end-time (System/nanoTime)]
        #p (- end-time (:start-time context)))

      (.endExchange exchange))
    'handle-instant))

(defn handle-blocking
  [context, ^HttpServerExchange exchange]
  (when-let [blocking (resp/blocking context)]
    (if (.isInIoThread exchange)
      (->> ^Runnable
           (^:once fn* [] (handle-context (blocking) exchange))
           (.dispatch exchange))
      (handle-context (blocking) exchange))
    'handle-blocking))

(defn async-callback
  [exchange]
  (fn [context]
    (handle-context context exchange)))

(defn handle-async
  [context, ^HttpServerExchange exchange]
  (when-let [async (resp/async context)]
    (if (.isDispatched exchange)
      (async (async-callback exchange))
      (->> ^Runnable
           (^:once fn* [] (async (async-callback exchange)))
           (.dispatch exchange SameThreadExecutor/INSTANCE)))
    'handle-async))

(defn handle-context
  [context, exchange]
  (or (handle-instant context exchange)
      (handle-blocking context exchange)
      (handle-async context exchange)
      (throw (ex-info (str "Missing handler for context: " (pr-str context)) {}))))

(defn http-handler
  ""
  [handler-fn]
  (reify HttpHandler
    (handleRequest [_ exchange]
      (-> {:request exchange :start-time (System/nanoTime)}
          (handler-fn)
          (handle-context exchange)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
