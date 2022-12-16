(ns undertow-ring.impl.ring-request
  (:require [immutant.web.internal.ring :as immutant.ring]
            [ring.adapter.undertow.headers :as adapter.headers]
            [ring.adapter.undertow.request :as adapter.request]
            [ring.util.response :as ring.response]
            [strojure.zizzmap.core :as zizz]
            [undertow-ring.impl.request-headers :as headers]
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
  (case s "GET" :get "POST" :post "PUT" :put "DELETE" :delete "HEAD" :head "OPTIONS" :options
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
  ;             Execution time mean : 14,033375 ns
  ;    Execution time std-deviation : 0,509849 ns
  ;   Execution time lower quantile : 13,565789 ns ( 2,5%)
  ;   Execution time upper quantile : 14,883741 ns (97,5%)

  (def -headers (headers/ring-headers (.getRequestHeaders -exchange)))

  (get -headers "Host")
  #_=> "localhost:8080"
  ;             Execution time mean : 33,529314 ns
  ;    Execution time std-deviation : 1,880358 ns
  ;   Execution time lower quantile : 31,427326 ns ( 2,5%)
  ;   Execution time upper quantile : 36,320641 ns (97,5%)

  (headers/as-persistent-map (.getRequestHeaders -exchange))
  #_=> {"connection" "Keep-Alive",
        "accept-encoding" "br,deflate,gzip,x-gzip",
        "cookie" "JSESSIONID=YXnPYqFOpP3kLAb-f8aLZ4SnJ2WGdyVV7TedaYQK",
        "user-agent" "Apache-HttpClient/4.5.13 (Java/17.0.5)",
        "host" "localhost:8080"}
  ;             Execution time mean : 1,074338 µs
  ;    Execution time std-deviation : 24,671600 ns
  ;   Execution time lower quantile : 1,053665 µs ( 2,5%)
  ;   Execution time upper quantile : 1,102043 µs (97,5%)

  (adapter.headers/get-headers (.getRequestHeaders -exchange))
  ;             Execution time mean : 1,157751 µs
  ;    Execution time std-deviation : 104,759240 ns
  ;   Execution time lower quantile : 1,052633 µs ( 2,5%)
  ;   Execution time upper quantile : 1,273790 µs (97,5%)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn build-request
  ;; TODO: Refer to https://github.com/ring-clojure/ring/wiki/Concepts#requests
  [^HttpServerExchange e]
  ;; TODO: Remove inline def
  #_(def ^HttpServerExchange -exchange e)
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

(defn build-request-zizz
  [^HttpServerExchange e]
  (let [query-string (.getQueryString e)
        query-string (when-not (.isEmpty query-string) query-string)
        context (.getResolvedPath e)
        context (when-not (.isEmpty context) context)
        session (session/get-session e)
        body (exchange/get-input-stream e)]
    (-> (.asTransient PersistentHashMap/EMPTY)
        (.assoc :undertow/exchange e)
        (.assoc :server-port,,, (zizz/delay* (.getPort (.getDestinationAddress e))))
        (.assoc :server-name,,, (zizz/delay* (.getHostName e)))
        (.assoc :remote-addr,,, (zizz/delay* (.getHostAddress (.getAddress (.getSourceAddress e)))))
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
        (zizz/convert-map))))

(comment
  -exchange
  (build-request -exchange)
  (build-request-zizz -exchange)
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
  (zizz/delay* (.getPort (.getDestinationAddress -exchange)))
  (.getHostName -exchange)
  (zizz/delay* (.getHostName -exchange))
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
