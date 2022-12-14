(ns undertow-ring.impl.request
  (:require [immutant.web.internal.ring :as immutant.ring]
            [ring.adapter.undertow.headers :as adapter.headers]
            [ring.adapter.undertow.request :as adapter.request]
            [ring.util.response :as ring.response]
            [strojure.zizzmap.impl :as zizz*]
            [undertow-ring.impl.headers :as headers]
            [undertow-ring.impl.session :as session]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang PersistentHashMap)
           (io.undertow.server HttpServerExchange)
           (io.undertow.util Headers)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(declare ^:deprecated ^HttpServerExchange -exchange)

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
  ;; TODO: Refer to https://github.com/ring-clojure/ring/wiki/Concepts#requests
  [^HttpServerExchange e]
  ;; TODO: Remove inline def
  #_ (def ^HttpServerExchange -exchange e)
  (let [query-string (.getQueryString e)
        query-string (when-not (.isEmpty query-string) query-string)
        context (.getResolvedPath e)
        context (when-not (.isEmpty context) context)
        session (session/get-session e)
        body (exchange/get-input-stream e)]
    (-> (.asTransient PersistentHashMap/EMPTY)
        (.assoc :undertow/exchange e)
        (.assoc :server-port,,, ^Object (.getPort (.getDestinationAddress e)))
        (.assoc :server-name,,, (.getHostName e))
        (.assoc :remote-addr,,, (.getHostAddress (.getAddress (.getSourceAddress e))))
        (.assoc :uri,,,,,,,,,,, (.getRequestURI e))
        (.assoc :scheme,,,,,,,, (scheme-keyword (.getRequestScheme e)))
        (.assoc :request-method (method-keyword (.toString (.getRequestMethod e))))
        (.assoc :headers,,,,,,, (headers/ring-headers (.getRequestHeaders e)))
        (cond->
          query-string,,, (.assoc :query-string query-string)
          context,,,,,,,, (.assoc :context context)
          session,,,,,,,, (.assoc :session session)
          body,,,,,,,,,,, (.assoc :body body))
        (.persistent))))

(defn build-request-map-zizz
  [^HttpServerExchange e]
  (let [query-string (.getQueryString e)
        query-string (when-not (.isEmpty query-string) query-string)
        context (.getResolvedPath e)
        context (when-not (.isEmpty context) context)
        session (session/get-session e)
        body (exchange/get-input-stream e)]
    (-> (.asTransient PersistentHashMap/EMPTY)
        (.assoc :undertow/exchange e)
        (.assoc :server-port,,, (zizz*/boxed-value (.getPort (.getDestinationAddress e))))
        (.assoc :server-name,,, (zizz*/boxed-value (.getHostName e)))
        (.assoc :remote-addr,,, (zizz*/boxed-value (.getHostAddress (.getAddress (.getSourceAddress e)))))
        (.assoc :uri,,,,,,,,,,, (.getRequestURI e))
        (.assoc :scheme,,,,,,,, (scheme-keyword (.getRequestScheme e)))
        (.assoc :request-method (method-keyword (.toString (.getRequestMethod e))))
        (.assoc :headers,,,,,,, (headers/ring-headers (.getRequestHeaders e)))
        (cond->
          query-string,,, (.assoc :query-string query-string)
          context,,,,,,,, (.assoc :context context)
          session,,,,,,,, (.assoc :session session)
          body,,,,,,,,,,, (.assoc :body body))
        (.persistent)
        (zizz*/persistent-map))))

(comment
  -exchange
  (build-request-map -exchange)
  (build-request-map-zizz -exchange)
  ;;; immutant
  (require 'immutant.web.internal.undertow)
  (immutant.ring/ring-request-map -exchange)
  (def -ireq (immutant.ring/ring-request-map -exchange))
  (assoc (immutant.ring/ring-request-map -exchange) :test 0)
  ;;; luminus ring adapter
  ((immutant.ring/ring-request-map -exchange) :headers)
  (adapter.request/build-exchange-map -exchange)
  ;;; exchange methods
  (.getRequestHeaders -exchange)
  (.get (.getRequestHeaders -exchange) "Host")
  (.get (.getRequestHeaders -exchange) "host")
  (.get (.getRequestHeaders -exchange) "HOST")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "Content-Type")
  (ring.response/get-header {:headers {"Content-Type" "xxx"}} "content-type")
  (.getHostPort -exchange)
  (-> -exchange .getDestinationAddress .getPort)
  (-> -exchange .getDestinationAddress)
  (zizz*/boxed-value (.getPort (.getDestinationAddress -exchange)))
  (.getHostName -exchange)
  (zizz*/boxed-value (.getHostName -exchange))
  (.getHostAddress (.getAddress (.getSourceAddress -exchange))) ; Execution time mean : 126,172175 ns
  (-> -exchange .getSourceAddress .getAddress .getHostAddress)
  (.getRequestScheme -exchange)
  (scheme-keyword (.getRequestScheme -exchange))
  (method-keyword (.toString (.getRequestMethod -exchange)))
  (let [s (.getQueryString -exchange)]
    (when-not (.equals "" s) s))
  (let [s (.getQueryString -exchange)]
    (when-not (.isEmpty s) s))
  (-> -exchange .getRequestHeaders (.getFirst Headers/CONTENT_TYPE))
  (-> -exchange .getRequestHeaders headers/ring-headers)
  (.getRequestCharset -exchange)
  (.getResolvedPath -exchange)
  (.isEmpty (.getResolvedPath -exchange))
  (.getRequestURI -exchange)
  (exchange/get-session-manager -exchange)
  (exchange/get-existing-session -exchange)
  (delay (exchange/get-existing-session -exchange))
  (exchange/get-input-stream -exchange)
  (session/get-session -exchange)
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

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
