(ns spin-undertow.request
  (:require [spin.request :as request]
            [undertow.api.exchange :as exchange])
  (:import (io.undertow.server HttpServerExchange)
           (io.undertow.util AttachmentKey)
           (java.util ArrayDeque)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -server-exchange
  [^HttpServerExchange e _] e)

(defn -server-port
  [^HttpServerExchange e _] (.getPort (.getDestinationAddress e)))

(defn -server-name
  [^HttpServerExchange e _] (.getHostName e))

(defn -remote-addr
  [^HttpServerExchange e _] (.getHostAddress (.getAddress (.getSourceAddress e))))

(defn -uri
  [^HttpServerExchange e _] (.getRequestURI e))

(defn -query-string
  [^HttpServerExchange e _] (as-> (.getQueryString e) s
                                  (when-not (.isEmpty s) s)))

(defn -scheme
  [^HttpServerExchange e _] (as-> (.getRequestScheme e) s
                                  (case s "http" :http "https" :https
                                          (keyword (.toLowerCase ^String s)))))

(defn -method
  ([^HttpServerExchange e _] (as-> (.toString (.getRequestMethod e)) s
                                   (case s "GET" :get "POST" :post "PUT" :put
                                           "DELETE" :delete "HEAD" :head "OPTIONS" :options
                                           (keyword (.toLowerCase ^String s)))))
  ([e _ x] (.equals ^Object (-method e _) x)))

;; TODO: dispatch blocking when work with input stream
(defn -body
  [^HttpServerExchange e _] (exchange/get-input-stream e))

(defn -header
  [^HttpServerExchange e _ x] (.getFirst (.getRequestHeaders e) ^String x))

(defn -header*
  [^HttpServerExchange e _ x] (.get (.getRequestHeaders e) ^String x))

(defn -query-param
  [^HttpServerExchange e _ x] (some-> ^ArrayDeque (.get (.getQueryParameters e) x)
                                      (.peekFirst)))

(defn -query-param*
  [^HttpServerExchange e _ x] (seq (.get (.getQueryParameters e) x)))

(defn -cookie
  [^HttpServerExchange e _ x] (some-> (.getRequestCookie e x) (.getValue)))

(defn -cookie-info
  [^HttpServerExchange e _ x] (when-let [c (.getRequestCookie e x)]
                                ;; TODO: cookie map fields
                                ;; TODO: skip empty fields?
                                {:name (.getName c)
                                 :value (.getValue c)
                                 :path (.getPath c)}))

(defonce ^{:doc ""}
         request-state-attachment-key (AttachmentKey/create Object))

(defn -state
  ([^HttpServerExchange e _ k]
   (-> (.getAttachment e request-state-attachment-key)
       (get k)))
  ([^HttpServerExchange e _ k v]
   (.putAttachment e request-state-attachment-key
                   (-> (.getAttachment e request-state-attachment-key)
                       (assoc k v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def exchange-methods (request/create-methods))

(def add-method (partial request/add-method* exchange-methods))

(add-method -server-exchange :server-exchange)
(add-method -server-port,,,, :server-port)
(add-method -server-name,,,, :server-name :server-host)
(add-method -remote-addr,,,, :remote-addr)
(add-method -uri,,,,,,,,,,,, :uri)
(add-method -query-string,,, :query-string)
(add-method -scheme,,,,,,,,, :scheme)
(add-method -method,,,,,,,,, :method :request-method)
(add-method -body,,,,,,,,,,, :body :input-stream)
(add-method -header,,,,,,,,, :header)
(add-method -header*,,,,,,,, :header*)
(add-method -query-param,,,, :query-param)
(add-method -query-param*,,, :query-param*)
(add-method -cookie,,,,,,,,, :cookie)
(add-method -cookie-info,,,, :cookie-info)
(add-method -state,,,,,,,,,, :state!)

;; TODO: protocol, path-info

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-request
  [exchange]
  (request/create-request exchange exchange-methods))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
