(ns user.server-spin
  (:require [spin-undertow.handler :as spin-handler]
            [spin.request :as req]
            [undertow.server :as server])
  (:import (io.undertow.server HttpHandler HttpServerExchange)
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

(defn -test-handler
  [{:keys [request] :as context}]
  (assoc context :response {:status 226 :body "instant" :headers {"x-test" "handler"}}
                 :response/status 404)
  #_(if (req/method-post? request)
      (delay (assoc context :response (str "blocking - " (t-name) "\n\n"
                                           (String. (.readAllBytes (req/body request))))))
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
  (req/header -e "Accept-Encoding")
  (req/header* -e "x-test")
  (.getRequestCookies -e)
  (some-> (.getRequestCookie -e "JSESSIONID") (.getValue))
  (req/cookie -e "JSESSIONID")
  (req/cookie* -e "JSESSIONID")
  (first (.get (.getQueryParameters -e) "a"))
  (.getQueryString -e)
  (.peekFirst ^java.util.ArrayDeque (.get (.getQueryParameters -e) "a"))
  (req/query-param -e "a")
  (req/query-param* -e "a")
  )

(defn start-test-server
  []
  (-> {:port 8086
       #_#_:handler (-> -test-handler -test-middleware)
       :handler http-handler}
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
