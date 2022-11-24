(ns user.undertow
  (:require [ring.adapter.undertow.headers :as adapter.headers]
            [ring.adapter.undertow.request :as adapter.request]
            [ring.util.response :as ring.response]
            [strojure.zizzmap.core :as zizz]
            [user.headers :as headers])
  (:import (io.undertow Undertow)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.server.handlers NameVirtualHostHandler RequestDumpingHandler SetHeaderHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.util HeaderMap Headers HttpString)
           (java.util Collection)))

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

(comment
  (headers/->HeaderMapProxy (.getRequestHeaders -exchange))
  ;             Execution time mean : 9,855630 ns
  ;    Execution time std-deviation : 0,413126 ns
  ;   Execution time lower quantile : 9,354131 ns ( 2,5%)
  ;   Execution time upper quantile : 10,413843 ns (97,5%)

  (def -headers (headers/->HeaderMapProxy (.getRequestHeaders -exchange)))

  (get -headers "Host")
  #_=> "localhost:8080"
  ;             Execution time mean : 33,529314 ns
  ;    Execution time std-deviation : 1,880358 ns
  ;   Execution time lower quantile : 31,427326 ns ( 2,5%)
  ;   Execution time upper quantile : 36,320641 ns (97,5%)

  (headers/persistent-map (.getRequestHeaders -exchange))
  #_=> {"sec-fetch-site" "none",
        "host" "localhost:8080",
        "user-agent" "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0",
        "cookie" "secret=dfe83f04-2d13-4914-88dd-5005ac317936",
        "sec-fetch-user" "?1",
        "x-a" "a1,a2",
        "connection" "keep-alive",
        "upgrade-insecure-requests" "1",
        "accept" "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
        "accept-language" "ru,en;q=0.8,de;q=0.6,uk;q=0.4,be;q=0.2",
        "sec-fetch-dest" "document",
        "accept-encoding" "gzip, deflate, br",
        "sec-fetch-mode" "navigate",
        "sec-gpc" "1"}
  ;             Execution time mean : 5,081815 µs
  ;    Execution time std-deviation : 143,452140 ns
  ;   Execution time lower quantile : 4,926556 µs ( 2,5%)
  ;   Execution time upper quantile : 5,258276 µs (97,5%)
  (adapter.headers/get-headers (.getRequestHeaders -exchange))
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
   #_#_:headers (headers/persistent-map (.getRequestHeaders exchange))
   :headers (headers/->HeaderMapProxy (.getRequestHeaders exchange))
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
              :headers (headers/persistent-map (.getRequestHeaders exchange))
              :body (when (.isBlocking exchange) (.getInputStream exchange))}))

(comment
  -exchange
  (exchange->request -exchange)
  (exchange->lazy-request -exchange)
  (adapter.request/build-exchange-map -exchange)
  (.get (.getRequestHeaders ^HttpServerExchange -exchange) "Host")
  (.get (.getRequestHeaders ^HttpServerExchange -exchange) "host")
  (.get (.getRequestHeaders ^HttpServerExchange -exchange) "HOST")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "Content-Type")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "content-type")
  (.getHostName ^HttpServerExchange -exchange)
  (.getHostAddress (.getAddress (.getSourceAddress ^HttpServerExchange -exchange)))
  (.getHostString (.getSourceAddress ^HttpServerExchange -exchange))
  (.getRequestScheme ^HttpServerExchange -exchange)

  )

(defn set-response-headers
  [exchange headers]
  (reduce-kv (fn [^HeaderMap hs k v]
               (cond (sequential? v)
                     (-> hs (.putAll (HttpString. (str k))
                                     ^Collection (map str v)))
                     (some? v)
                     (-> hs (.put (HttpString. (str k)) (str v)))
                     :else
                     (-> hs (.remove (HttpString. (str k))))))
             (.getResponseHeaders ^HttpServerExchange exchange)
             headers))

(defn ring->http-handler
  ^HttpHandler [f]
  (reify HttpHandler (handleRequest [_ exchange]
                       ;; TODO: Remove inline def
                       (def -exchange exchange)
                       (let [resp (f (exchange->request exchange))]
                         (some->> (:headers resp) (set-response-headers exchange))
                         (some->> (:status resp) (.setStatusCode exchange))
                         ;; TODO: Handle other body types
                         ;; - ISeq Each element of the seq is sent to the client as a string.
                         ;; - File The contents of the referenced file is sent to the client.
                         ;; - InputStream The contents of the stream is sent to the client. When the stream is exhausted, the stream is closed.
                         (some->> ^String (:body resp) (.send (.getResponseSender exchange)))))))

(defn http-handler
  ^HttpHandler [x]
  (cond (fn? x) (ring->http-handler x)
        (instance? HttpHandler x) x
        :else (throw (ex-info (str "Cannot create undertow HttpHandler for " (pr-str x)) {}))))

(comment
  (str "123")
  (string? "123")
  (instance? Collection (range 2))
  (fn? (RequestDumpingHandler. nil))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn test-ring-handler
  [req]
  {:body (str "Hello World " req)
   :headers {"x-a" "1"
             "x-b" "2"
             "x-c" [3 4]
             #_#_"content-type" "xxx"}
   #_#_:status 404})

(defn start
  []
  (doto (-> (Undertow/builder)
            (.addHttpListener 8080 nil (-> (NameVirtualHostHandler.)
                                           (.addHost "localhost" (-> (ring->http-handler test-ring-handler)
                                                                     (SetHeaderHandler. "Content-Type" "text/plain")))
                                           (.addHost "127.0.0.1" (-> (reify HttpHandler (handleRequest [_ exchange]
                                                                                          (doto exchange
                                                                                            (-> (.getResponseHeaders)
                                                                                                (.put Headers/CONTENT_TYPE "text/plain"))
                                                                                            (-> (.getResponseSender)
                                                                                                (.send "Hello World (127.0.0.1)")))))
                                                                     (SetHeaderHandler. "Content-Type" "text/plain")))
                                           (RequestDumpingHandler.)))
            (.addHttpListener 8081 "localhost"
                              (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                                                           "public")))
            (.build))
    (.start)))

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! (fn [server] (some-> ^Undertow server (.stop)))))

(defn init-server []
  (stop-server)
  (reset! server! (start)))

(init-server)

(comment
  ; exchange -> request -> response -> exchange
  ; chain handlers

  {:name-virtual-host-handler {"localhost" (fn [])}}
  (init-server)
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
