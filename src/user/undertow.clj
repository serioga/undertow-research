(ns user.undertow
  (:require [immutant.web.internal.ring :as immutant.ring]
            [ring.adapter.undertow.headers :as adapter.headers]
            [ring.adapter.undertow.request :as adapter.request]
            [ring.util.response :as ring.response]
            [strojure.zizzmap.core :as zizz]
            [user.headers :as headers])
  (:import (clojure.lang Fn IPersistentMap MultiFn)
           (io.undertow Undertow Undertow$Builder Undertow$ListenerBuilder Undertow$ListenerType)
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
  (headers/as-persistent-map (.getRequestHeaders -exchange))
  ;             Execution time mean : 9,855630 ns
  ;    Execution time std-deviation : 0,413126 ns
  ;   Execution time lower quantile : 9,354131 ns ( 2,5%)
  ;   Execution time upper quantile : 10,413843 ns (97,5%)

  (def -headers (headers/as-persistent-map (.getRequestHeaders -exchange)))

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
  ;; TODO: `path-info` in request (see immutant)
  (let [headers (.getRequestHeaders exchange)
        query-string (.getQueryString exchange)
        query-string (when-not (.isEmpty query-string) query-string)
        content-type (.getFirst headers Headers/CONTENT_TYPE)
        content-length (.getRequestContentLength exchange)
        content-length (when-not (neg? content-length) content-length)
        body (when (.isBlocking exchange) (.getInputStream exchange))]
    (cond-> {:undertow/exchange exchange
             :server-port (.getPort (.getDestinationAddress exchange))
             :server-name (.getHostName exchange)
             ;; TODO: remote addr
             :remote-addr (.getHostString (.getSourceAddress exchange)) #_(-> exchange .getSourceAddress .getAddress .getHostAddress)
             :uri (.getRequestURI exchange)
             :scheme (scheme-keyword (.getRequestScheme exchange))
             :request-method (method-keyword (.toString (.getRequestMethod exchange)))
             :character-encoding (.getRequestCharset exchange)
             ;; TODO: header conversion is slow
             #_#_:headers (headers/persistent-map headers)
             :headers (headers/as-persistent-map headers)
             ;; TODO: Don't put empty context in request?
             :context (.getResolvedPath exchange)}
      query-string,, (assoc :query-string query-string)
      content-type,, (assoc :content-type content-type)
      content-length (assoc :content-length content-length)
      body,,,,,,,,,, (assoc :body body))))

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
  (exchange->request -exchange)
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

(defn ring-handler-adapter
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

(comment
  (str "123")
  (string? "123")
  (instance? Collection (range 2))
  (fn? (RequestDumpingHandler. nil))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol HttpListenerBuilder
  (new-listener-builder [opts port]))

(extend-protocol HttpListenerBuilder
  IPersistentMap
  ;; TODO: Document, that it covers only HTTP/HTTPS but not AJP
  (new-listener-builder
    [{:keys [host https handler socket-options use-proxy-protocol] :or {host "localhost"}}
     port]
    (-> (Undertow$ListenerBuilder.)
        (.setType (if https Undertow$ListenerType/HTTPS
                            Undertow$ListenerType/HTTP))
        (.setPort port)
        (.setHost host)
        (.setRootHandler handler)
        (.setKeyManagers,, (:key-managers https))
        (.setTrustManagers (:trust-managers https))
        (.setSslContext,,, (:ssl-context https))
        ;; TODO: Set OptionMap
        #_(.setOverrideSocketOptions nil)
        (.setUseProxyProtocol (boolean use-proxy-protocol))))
  Undertow$ListenerBuilder
  (new-listener-builder [builder port] (.setPort builder port))
  HttpHandler
  (new-listener-builder [handler port] (new-listener-builder {:handler handler} port)))

(defn add-listener
  ^Undertow$Builder
  [^Undertow$Builder builder, [port opts]]
  (doto builder
    (.addListener (new-listener-builder opts port))))

(defn add-port-listeners
  ^Undertow$Builder
  [builder ports]
  (reduce add-listener builder ports))

(defn- wrap-with
  "Applies wrap function or a sequence of wrap functions to the `x`."
  [x fs]
  (if (sequential? fs)
    (->> (reverse fs)
         (reduce (fn [obj f] (f obj)) x))
    (fs x)))

(def ^:dynamic *handler-fn-adapter* identity)

#_(def as-http-handler nil)
(defmulti ^HttpHandler as-http-handler (some-fn :type type))
(.addMethod ^MultiFn as-http-handler HttpHandler identity)

(defmethod as-http-handler Fn
  [handler-fn] (*handler-fn-adapter* handler-fn))

(defmethod as-http-handler :undertow/named-virtual-host-handler
  [{:keys [hosts, default-handler]}]
  (cond-> ^NameVirtualHostHandler
          (reduce (fn [handler [host opts]]
                    (.addHost ^NameVirtualHostHandler handler host (as-http-handler opts)))
                  (NameVirtualHostHandler.)
                  hosts)
    default-handler (.setDefaultHandler (as-http-handler default-handler))))

(defmethod as-http-handler :undertow/resource-handler
  [{:keys [path-prefix, next-handler]
    :or {path-prefix "public"}}]
  (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                               ^String path-prefix)
                    (some-> next-handler as-http-handler)))

(defn wrap-resource-handler
  [opts]
  (fn [handler]
    (as-http-handler (assoc opts :type :undertow/resource-handler
                                 :next-handler handler))))

(defn build-server
  ^Undertow [{:keys [ports, handler, wrap-handler, wrap-builder]}]
  (-> (Undertow/builder)
      (add-port-listeners ports)
      (cond-> handler (.setHandler (cond-> (as-http-handler handler)
                                     wrap-handler (wrap-with wrap-handler)))
              wrap-builder ^Undertow$Builder (wrap-with wrap-builder))
      (.build)))

(defn start
  ^Undertow [options]
  (doto (build-server options) .start))

(defn test-ring-handler
  [req]
  {:body (str "Hello World " req)
   :headers {"x-a" "1"
             "x-b" "2"
             "x-c" [3 4]
             #_#_"content-type" "xxx"}
   #_#_:status 404})

(defn test-ring-handler-fn
  ([] (test-ring-handler-fn "Hello World"))
  ([greet]
   (fn [req]
     {:body (str greet "\n\n" req)
      #_#_:headers {"x-a" "1"
                    "x-b" "2"
                    "x-c" [3 4]
                    #_#_"content-type" "xxx"}
      #_#_:status 200})))

(defn start-test-server
  []
  (binding [*handler-fn-adapter* ring-handler-adapter]
    (start {:ports {8080 {:host "localhost"}}
            :handler (test-ring-handler-fn "2")
            #_#_:handler {:type :undertow/resource-handler
                          :next-handler {:type :undertow/named-virtual-host-handler
                                         :hosts {"localhost" (test-ring-handler-fn "1")
                                                 "127.0.0.1" (test-ring-handler-fn "2")}}}
            :wrap-handler [(fn [h] (RequestDumpingHandler. h))
                           (wrap-resource-handler {})]
            :wrap-builder [(fn [^Undertow$Builder builder] (.setIoThreads builder 2))
                           (fn [^Undertow$Builder builder] (.setIoThreads builder 1))]}))
  #_(doto (-> (Undertow/builder)
              #_(.addHttpListener 8080 nil (-> (NameVirtualHostHandler.)
                                               (.addHost "localhost" (-> (ring-handler-adapter test-ring-handler)
                                                                         (SetHeaderHandler. "Content-Type" "text/plain")))
                                               (.addHost "127.0.0.1" (-> (reify HttpHandler (handleRequest [_ exchange]
                                                                                              (doto exchange
                                                                                                (-> (.getResponseHeaders)
                                                                                                    (.put Headers/CONTENT_TYPE "text/plain"))
                                                                                                (-> (.getResponseSender)
                                                                                                    (.send "Hello World (127.0.0.1)")))))
                                                                         (SetHeaderHandler. "Content-Type" "text/plain")))
                                               (RequestDumpingHandler.)))
              #_(.addHttpListener 8081 "localhost"
                                  (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                                                               "public")))
              #_(add-listener [8080 (-> (ring-handler-adapter (test-ring-handler-fn "1"))
                                        (RequestDumpingHandler.))])
              (add-listener [8080 {}])
              #_(.addHttpListener 8080 nil (comment (-> (ring-handler-adapter (test-ring-handler-fn "1"))
                                                        (RequestDumpingHandler.))))
              (.setHandler (-> (ring-handler-adapter (test-ring-handler-fn "2"))
                               (RequestDumpingHandler.)))
              (.build))
      (.start)))

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! (fn [server] (some-> ^Undertow server (.stop)))))

(defn init-server []
  (stop-server)
  (reset! server! (start-test-server)))

(init-server)

(comment
  ; exchange -> request -> response -> exchange
  ; chain handlers

  {:ports {8080 {:host "localhost"
                 :https {:key-managers [] :trust-managers []}
                 #_#_:https {:ssl-context nil}
                 :handler nil
                 :socket-options {} :use-proxy-protocol false}}
   :handler nil
   :wrap-handler nil}


  {:name-virtual-host-handler {"localhost" (fn [])}}
  (init-server)
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
