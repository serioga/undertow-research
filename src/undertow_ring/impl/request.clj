(ns undertow-ring.impl.request
  (:require [strojure.zizzmap.core :as zizz]
            [undertow-ring.impl.headers :as headers]
            [undertow-ring.impl.session :as session]
            [undertow.api.exchange :as exchange])
  (:import (io.undertow.server HttpServerExchange)
           (io.undertow.util Headers)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn method-keyword
  [s]
  (case s "OPTIONS" :options "GET" :get "HEAD" :head "POST" :post "PUT" :put "DELETE" :delete
          (keyword (.toLowerCase ^String s))))

(comment
  (method-keyword (.toString (.getRequestMethod -exchange)))
  #_=> :get
  ;             Execution time mean : 5,780530 ns
  ;    Execution time std-deviation : 0,555488 ns
  ;   Execution time lower quantile : 5,332538 ns ( 2,5%)
  ;   Execution time upper quantile : 6,680023 ns (97,5%)
  )

(defn scheme-keyword
  [s]
  (case s "http" :http "https" :https
          (keyword (.toLowerCase ^String s))))

(comment
  (scheme-keyword (.getRequestScheme -exchange))
  #_=> :http
  ;             Execution time mean : 2,649399 ns
  ;    Execution time std-deviation : 1,004296 ns
  ;   Execution time lower quantile : 1,863488 ns ( 2,5%)
  ;   Execution time upper quantile : 4,057465 ns (97,5%)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn build-request-map
  [^HttpServerExchange exchange]
  ;; TODO: `path-info` in request (see immutant)
  (let [header-map (.getRequestHeaders exchange)
        query-string (.getQueryString exchange)
        query-string (when-not (.isEmpty query-string) query-string)
        content-type (.getFirst header-map Headers/CONTENT_TYPE)
        content-length (.getRequestContentLength exchange)
        content-length (when-not (neg? content-length) content-length)
        body (exchange/get-input-stream exchange)
        ;; TODO: Delayed getting of the session?
        session (session/get-session exchange)]
    (cond-> {:undertow/exchange exchange
             :server-port (.getPort (.getDestinationAddress exchange))
             :server-name (.getHostName exchange)
             ;; TODO: remote addr
             :remote-addr (.getHostString (.getSourceAddress exchange)) #_(-> exchange .getSourceAddress .getAddress .getHostAddress)
             :uri (.getRequestURI exchange)
             :scheme (scheme-keyword (.getRequestScheme exchange))
             :request-method (method-keyword (.toString (.getRequestMethod exchange)))
             :character-encoding (.getRequestCharset exchange)
             :headers (headers/ring-headers header-map)
             ;; TODO: Don't put empty context in request?
             :context (.getResolvedPath exchange)}
      query-string,, (assoc :query-string query-string)
      content-type,, (assoc :content-type content-type)
      content-length (assoc :content-length content-length)
      body,,,,,,,,,, (assoc :body body)
      session,,,,,,, (assoc :session session))))

(defn exchange->lazy-request
  [^HttpServerExchange exchange]
  (let [headers (.getRequestHeaders exchange)]
    (zizz/init {:undertow/exchange exchange
                :server-port (.getPort (.getDestinationAddress exchange))
                :server-name (.getHostName exchange)
                ;; TODO: remote addr
                :remote-addr (.getHostString (.getSourceAddress exchange)) #_(-> exchange .getSourceAddress .getAddress .getHostAddress)
                :uri (.getRequestURI exchange)
                :query-string (let [s (.getQueryString exchange)] (when-not (.isEmpty s) s))
                :scheme (scheme-keyword (.getRequestScheme exchange))
                :request-method (method-keyword (.toString (.getRequestMethod exchange)))
                :content-type (.getFirst headers Headers/CONTENT_TYPE)
                :content-length (.getRequestContentLength exchange)
                :character-encoding (.getRequestCharset exchange)
                :headers (headers/persistent-map headers)
                :body (when (.isBlocking exchange) (.getInputStream exchange))
                :context (.getResolvedPath exchange)})))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
