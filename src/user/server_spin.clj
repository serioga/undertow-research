(ns user.server-spin
  (:require [spin-undertow.handler :as spin-handler]
            [undertow.server :as server])
  (:import (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.util Methods)
           (java.util.concurrent CompletableFuture)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(server/set-handler-fn-adapter spin-handler/http-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -test-middleware
  [handler]
  (fn [context]
    (handler (update context :response/headers
                     conj ["X-Test-Middleware" "was here"] ["X-Test" "middleware"]))))

(defn- t-name [] (.getName (Thread/currentThread)))

(declare -request)

(defn -test-handler
  [{:keys [request] :as context}]
  (def -request request)
  (assoc context :response {:status 226 :body "instant" :headers {"x-test" "handler"}}
                 :response/status 404)
  #_(if (request :method-post?)
      (delay (assoc context :response (str "blocking - " (t-name) "\n\n"
                                           (String. (.readAllBytes (request :body))))))
      (assoc context :response (str "non-blocking - " (t-name))))
  #_(throw (ex-info "oops" {}))
  #_context
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
  (-request :server-exchange)
  (def ^HttpServerExchange -e (-request :server-exchange))
  (-request :header "Accept-Encoding")
  (-request :header "x-test")
  (-request :header "x-test" :many)
  (-request :header "x-test" false)
  (.getFirst (.getRequestHeaders -e) "Accept-Encoding")
  (.getRequestCookies -e)
  (some-> (.getRequestCookie -e "JSESSIONID") (.getValue))
  (-request :cookie "JSESSIONID")
  (-request :cookie-info "JSESSIONID")
  (first (.get (.getQueryParameters -e) "a"))
  (.getQueryString -e)
  (.peekFirst ^java.util.ArrayDeque (.get (.getQueryParameters -e) "a"))
  (-request :query-string)
  (-request :query-param "a")
  (-request :query-param "a" :many)
  (-request :uri)
  (-request :method)
  (-request :method :get)
  (-request :request-method)
  (.getRequestMethod -e)
  (.equals Methods/GET (.getRequestMethod -e))
  (-request :state! :k)
  (-request :state! :k :v)
  )

(defn start-test-server
  []
  (-> {:port 8086
       :handler (-> -test-handler -test-middleware)
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
