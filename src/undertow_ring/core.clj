(ns undertow-ring.core
  (:require [immutant.web.internal.ring :as immutant.ring]
            [ring.adapter.undertow.headers :as adapter.headers]
            [ring.adapter.undertow.request :as adapter.request]
            [ring.util.response :as ring.response]
            [strojure.zizzmap.core :as zizz]
            [undertow-ring.headers :as headers]
            [undertow-ring.response :as response]
            [undertow-ring.session :as session]
            [undertow.exchange :as exchange]
            [undertow.handler :as handler])
  (:import (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util Headers)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(declare ^:deprecated ^HttpServerExchange -exchange)

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

(comment
  (headers/ring-headers (.getRequestHeaders -exchange))
  ;             Execution time mean : 9,855630 ns
  ;    Execution time std-deviation : 0,413126 ns
  ;   Execution time lower quantile : 9,354131 ns ( 2,5%)
  ;   Execution time upper quantile : 10,413843 ns (97,5%)

  (def -headers (headers/ring-headers (.getRequestHeaders -exchange)))

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

(comment
  -exchange
  (build-request-map -exchange)
  (exchange->lazy-request -exchange)
  (require 'immutant.web.internal.undertow)
  (immutant.ring/ring-request-map -exchange)
  (def -ireq (immutant.ring/ring-request-map -exchange))
  (assoc (immutant.ring/ring-request-map -exchange) :test 0)
  ((immutant.ring/ring-request-map -exchange) :headers)
  (adapter.request/build-exchange-map -exchange)
  (.getRequestHeaders -exchange)
  (.get (.getRequestHeaders -exchange) "Host")
  (.get (.getRequestHeaders -exchange) "host")
  (.get (.getRequestHeaders -exchange) "HOST")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "Content-Type")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "content-type")
  (.getHostPort -exchange)
  (-> -exchange .getDestinationAddress .getPort)
  (-> -exchange .getDestinationAddress)
  (.getHostName -exchange)
  (.getHostAddress (.getAddress (.getSourceAddress -exchange)))
  (.getHostString (.getSourceAddress -exchange))
  (-> -exchange .getSourceAddress .getAddress .getHostAddress)
  (.getRequestScheme -exchange)
  (let [s (.getQueryString -exchange)]
    (when-not (.equals "" s) s))
  (let [s (.getQueryString -exchange)]
    (when-not (.isEmpty s) s))
  (-> -exchange .getRequestHeaders (.getFirst Headers/CONTENT_TYPE))
  (.getRequestCharset -exchange)
  (.getResolvedPath -exchange)
  (.getRequestURI -exchange)
  (exchange/get-session-manager -exchange)
  (exchange/get-existing-session -exchange)
  (delay (exchange/get-existing-session -exchange))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn as-async-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::async))

(defn as-non-blocking-sync-handler
  [handler]
  (vary-meta handler assoc ::handler-type ::sync-non-blocking))

(defn- execute-sync
  [handler exchange]
  (-> (build-request-map exchange)
      (handler)
      (response/handle-response exchange)))

(defmulti fn-as-handler (comp ::handler-type meta))

(defmethod fn-as-handler nil
  [handler]
  (-> (reify HttpHandler
        (handleRequest [_ exchange]
          ;; TODO: Remove inline def
          (def -exchange exchange)
          (execute-sync handler exchange)))
      (handler/dispatch)))

(defmethod fn-as-handler ::async
  [handler]
  ;; TODO: Async exception handler
  (letfn [(exception-callback [e] (throw e))]
    (reify HttpHandler
      (handleRequest [_ e]
        (exchange/dispatch-async e
          (handler (build-request-map e)
                   (fn response-callback [response]
                     (response/handle-response response e))
                   exception-callback))))))

(defmethod fn-as-handler ::sync-non-blocking
  [handler]
  (reify HttpHandler
    (handleRequest [this e]
      (if (and (.isInIoThread e)
               (not (.isRequestComplete e)))
        ;; Dispatch incomplete request to worker thread
        (.dispatch e this)
        ;; Execute handler on IO thread
        (execute-sync handler e)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
