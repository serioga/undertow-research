(ns user.server-spin-v2
  (:require [spin-undertow.handler-v2 :as spin-handler]
            [undertow.server :as server])
  (:import (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util Methods)
           (java.io InputStream)
           (java.util.concurrent CompletableFuture)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(server/set-handler-fn-adapter spin-handler/http-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -test-middleware
  [context]
  (update context :response/headers
          conj ["X-Test-Middleware" "was here"] ["X-Test" "middleware"]))

(defn- t-name [] (.getName (Thread/currentThread)))

(declare -request)

(defn -test-handler
  [{:keys [request] :as context}]
  (def -request request)
  (assoc context :response {:status 226 :body (str "instant - " (t-name)) :headers {"x-test" "handler"}}
                 :response/status 404)
  #_(if (= :post (request :method))
      (delay (assoc context :response (str "blocking - " (t-name) "\n\n"
                                           (some-> ^InputStream (request :body) (.readAllBytes) (String.)))))
      (assoc context :response (str "non-blocking - " (t-name))))
  #_(throw (ex-info "oops" {}))
  #_(assoc context :response {:body (str "non-blocking - " (t-name))})
  #_(delay (assoc context :response {:body (str "blocking - " (t-name))}))
  #_(CompletableFuture/supplyAsync
      (reify Supplier (get [_] (throw (ex-info "oops" {})))))
  #_(CompletableFuture/supplyAsync
      (reify Supplier (get [_] (assoc context :response {:body (str "async - " (t-name))})))))

(declare ^HttpServerExchange -e)

(def http-handler (reify HttpHandler (handleRequest [_ e]
                                       (def ^HttpServerExchange -e e)
                                       (-> (.getResponseSender e)
                                           (.send "OK")))))
(comment
  (-request)
  (meta (-request))
  (-request :server-exchange)
  (def ^HttpServerExchange -e (-request :server-exchange))
  (-request :headers)
  (-request :headers "Accept-Encoding")
  (-request :headers "x-test")
  (-request :headers "x-test" :many)
  (-request :headers "x-test" false)
  (.getFirst (.getRequestHeaders -e) "Accept-Encoding")
  (.getRequestCookies -e)
  (some-> (.getRequestCookie -e "JSESSIONID") (.getValue))
  (-request :cookies)
  (-request :cookies "JSESSIONID")
  (-request :cookies "JSESSIONID" :data)
  (first (.get (.getQueryParameters -e) "a"))
  (.getQueryString -e)
  (.peekFirst ^java.util.ArrayDeque (.get (.getQueryParameters -e) "a"))
  (-request :query-string)
  (-request :query-params)
  (-request :query-params "a")
  (-request :query-params "a" :many)
  (-request :uri)
  (-request :method)
  (-request :method :raw)
  (-request :request-method)
  (.getRequestMethod -e)
  (.equals Methods/GET (.getRequestMethod -e))
  (-request :state)
  (-request :state :k)
  (-request :state :k :set! :v)
  (-request :state :k :set! nil)
  )

(defn start-test-server
  []
  (-> {:port 8086
       :handler (spin-handler/http-handler [-test-middleware -test-handler])
       #_#_:handler http-handler}
      (server/start)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! (fn [instance] (some-> instance server/stop))))

(defn init-server []
  (stop-server)
  (reset! server! (start-test-server)))

(init-server)

(comment
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
