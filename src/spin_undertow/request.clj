(ns spin-undertow.request
  (:require [spin.request :as request]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang MapEntry)
           (io.undertow.server HttpServerExchange)
           (io.undertow.server.handlers Cookie)
           (io.undertow.util AttachmentKey HeaderValues)
           (java.util ArrayDeque)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -server-exchange
  [^HttpServerExchange e _]
  e)

(defn -server-port
  [^HttpServerExchange e _]
  (.getPort (.getDestinationAddress e)))

(defn -server-name
  [^HttpServerExchange e _]
  (.getHostName e))

(defn -remote-addr
  [^HttpServerExchange e _]
  (.getHostAddress (.getAddress (.getSourceAddress e))))

(defn -uri
  [^HttpServerExchange e _]
  (.getRequestURI e))

(defn -query-string
  [^HttpServerExchange e _]
  (as-> (.getQueryString e) s
        (when-not (.isEmpty s) s)))

(defn -scheme
  ([^HttpServerExchange e _]
   (as-> (.getRequestScheme e) s
         (case s "http" :http "https" :https
                 (keyword (.toLowerCase ^String s)))))
  ([^HttpServerExchange e _ _raw?]
   (.getRequestScheme e)))

(defn -method
  ([^HttpServerExchange e _]
   (as-> (.toString (.getRequestMethod e)) s
         (case s "GET" :get "POST" :post "PUT" :put
                 "DELETE" :delete "HEAD" :head "OPTIONS" :options
                 (keyword (.toLowerCase ^String s)))))
  ([^HttpServerExchange e _ _raw?]
   (.toString (.getRequestMethod e))))

;; TODO: dispatch blocking when work with input stream
(defn -body
  [^HttpServerExchange e _]
  (exchange/get-input-stream e))

(defn -headers
  ([^HttpServerExchange e _]
   (some->> (.getRequestHeaders e)
            (.iterator)
            (iterator-seq)
            (map (fn [^HeaderValues h]
                   (MapEntry. (.toString (.getHeaderName h))
                              (iterator-seq (.iterator h)))))))
  ([^HttpServerExchange e _ x]
   (.getFirst (.getRequestHeaders e) ^String x))
  ([^HttpServerExchange e _ ^String x many?]
   (as-> (.getRequestHeaders e) header-map
         (if many?
           (.get header-map x)
           (.getFirst header-map x)))))

(defn -query-params
  ([^HttpServerExchange e _]
   (update-vals (.getQueryParameters e) seq))
  ([^HttpServerExchange e _ x]
   (some-> ^ArrayDeque (.get (.getQueryParameters e) x)
           (.peekFirst)))
  ([^HttpServerExchange e _ x many?]
   (if many?
     (seq (.get (.getQueryParameters e) x))
     (-query-params e _ x))))

(defn- cookie-map
  [^Cookie c]
  ;; TODO: cookie map fields
  ;; TODO: skip empty fields?
  {:name (.getName c)
   :value (.getValue c)
   :path (.getPath c)})

(defn -cookies
  ([^HttpServerExchange e _]
   (->> (.requestCookies e)
        (map cookie-map)))
  ([^HttpServerExchange e _ x]
   (some-> (.getRequestCookie e x)
           (.getValue)))
  ([^HttpServerExchange e _ x data?]
   (some-> (.getRequestCookie e x)
           (as-> c (if data? (cookie-map c)
                             (.getValue c))))))

(defonce ^{:doc ""}
  STATE_KEY (AttachmentKey/create Object))

(defn -state
  ([^HttpServerExchange e _]
   (.getAttachment e STATE_KEY))
  ([^HttpServerExchange e _ k]
   (-> (.getAttachment e STATE_KEY)
       (get k)))
  ([^HttpServerExchange e _ k _set! v]
   (.putAttachment e STATE_KEY (as-> (.getAttachment e STATE_KEY) state
                                     (if (some? v) (assoc state k v)
                                                   (dissoc state k))))
   nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def exchange-methods (request/create-methods))

(def add-method (partial request/add-method* exchange-methods))

(add-method -server-exchange,, :server-exchange)
(add-method -server-port,,,,,, :server-port)
(add-method -server-name,,,,,, :server-name :server-host)
(add-method -remote-addr,,,,,, :remote-addr)
(add-method -uri,,,,,,,,,,,,,, :uri)
(add-method -query-string,,,,, :query-string)
(add-method -query-params,,,,, :query-params)
(add-method -scheme,,,,,,,,,,, :scheme)
(add-method -method,,,,,,,,,,, :method :request-method)
(add-method -body,,,,,,,,,,,,, :body :input-stream)
(add-method -headers,,,,,,,,,, :headers)
(add-method -cookies,,,,,,,,,, :cookies)
(add-method -state,,,,,,,,,,,, :state)

;; TODO: protocol, path-info

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-request
  [exchange]
  (request/create-request exchange exchange-methods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
