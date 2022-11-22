(ns user.undertow
  (:require [strojure.zizzmap.core :as zizz])
  (:import (io.undertow Undertow)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.server.handlers NameVirtualHostHandler RequestDumpingHandler SetHeaderHandler)
           (io.undertow.util HeaderMap HeaderValues Headers)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(declare ^HttpServerExchange -exchange)

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

;; TODO: multiple header values?
(defn headers-map
  [^HeaderMap h]
  (loop [m! (transient {})
         it (.iterator h)]
    (if (.hasNext it)
      (let [^HeaderValues x (.next it)]
        (recur (assoc! m! (.toLowerCase (.toString (.getHeaderName x)))
                       (.getFirst x))
               it))
      (persistent! m!))))

(comment
  (headers-map (.getRequestHeaders -exchange))
  #_=> {"sec-fetch-site" "same-origin",
        "host" "localhost:8080",
        "user-agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0",
        "cookie" "secret=dfe83f04-2d13-4914-88dd-5005ac317936",
        "referer" "http://localhost:8080/",
        "connection" "keep-alive",
        "accept" "image/avif,image/webp,*/*",
        "accept-language" "ru,en;q=0.8,de;q=0.6,uk;q=0.4,be;q=0.2",
        "sec-fetch-dest" "image",
        "accept-encoding" "gzip, deflate, br",
        "sec-fetch-mode" "no-cors",
        "sec-gpc" "1"}
  ;             Execution time mean : 4,183899 µs
  ;    Execution time std-deviation : 121,940808 ns
  ;   Execution time lower quantile : 4,047818 µs ( 2,5%)
  ;   Execution time upper quantile : 4,362875 µs (97,5%)
  )

(defn exchange->request
  [^HttpServerExchange exchange]
  {:undertow/exchange exchange
   :server-port (.getHostPort exchange)
   :server-name (.getHostName exchange)
   ;; TODO: remote addr
   :remote-addr (.getHostString (.getSourceAddress exchange))
   :uri (.getRequestURI exchange)
   :query-string (.getQueryString exchange)
   :scheme (scheme-keyword (.getRequestScheme exchange))
   :request-method (method-keyword (.toString (.getRequestMethod exchange)))
   ;; TODO: header conversion is slow
   :headers (headers-map (.getRequestHeaders exchange))
   :body (when (.isBlocking exchange) (.getInputStream exchange))})

(defn exchange->lazy-request
  [^HttpServerExchange exchange]
  (zizz/init {:undertow/exchange exchange
              :server-port (.getHostPort exchange)
              :server-name (.getHostName exchange)
              ;; TODO: remote addr
              :remote-addr (.getHostString (.getSourceAddress exchange))
              :uri (.getRequestURI exchange)
              :query-string (.getQueryString exchange)
              :scheme (scheme-keyword (.getRequestScheme exchange))
              :request-method (method-keyword (.toString (.getRequestMethod exchange)))
              :headers (headers-map (.getRequestHeaders exchange))
              :body (when (.isBlocking exchange) (.getInputStream exchange))}))

(comment
  -exchange
  (exchange->request -exchange)
  (exchange->lazy-request -exchange)
  (.getRequestHeaders ^HttpServerExchange -exchange)
  (.getHostName ^HttpServerExchange -exchange)
  (.getHostAddress (.getAddress (.getSourceAddress ^HttpServerExchange -exchange)))
  (.getHostString (.getSourceAddress ^HttpServerExchange -exchange))
  (.getRequestScheme ^HttpServerExchange -exchange)

  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start
  []
  (doto (-> (Undertow/builder)
            (.addHttpListener 8080 nil (-> (NameVirtualHostHandler.)
                                           (.addHost "localhost" (-> (reify HttpHandler (handleRequest [_ exchange]
                                                                                          (def -exchange exchange)
                                                                                          (doto exchange
                                                                                            (-> (.getResponseHeaders)
                                                                                                (.put Headers/CONTENT_TYPE "text/plain"))
                                                                                            (-> (.getResponseSender)
                                                                                                (.send "Hello World (localhost)")))))
                                                                     (SetHeaderHandler. "Content-Type" "text/plain")))
                                           (.addHost "127.0.0.1" (-> (reify HttpHandler (handleRequest [_ exchange]
                                                                                          (doto exchange
                                                                                            (-> (.getResponseHeaders)
                                                                                                (.put Headers/CONTENT_TYPE "text/plain"))
                                                                                            (-> (.getResponseSender)
                                                                                                (.send "Hello World (127.0.0.1)")))))
                                                                     (SetHeaderHandler. "Content-Type" "text/plain")))
                                           (RequestDumpingHandler.)))
            (.build))
    (.start)))

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! (fn [server] (some-> ^Undertow server (.stop)))))

(defn init-server []
  (stop-server)
  (reset! server! (start)))

(comment
  ; exchange -> request -> response -> exchange
  ; chain handlers

  {:name-virtual-host-handler {"localhost" (fn [])}}
  (init-server)
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
