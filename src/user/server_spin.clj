(ns user.server-spin
  (:require [spin-undertow.handler :as spin-handler]
            [undertow.server :as server])
  (:import (io.undertow.server HttpHandler)
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
  [context]
  #_(assoc context :response {:body "instant" :headers {"x-test" "handler"}
                              #_#_:status 226}
                   :response/status 404)
  #_(throw (ex-info "oops" {}))
  #_context
  (assoc context :response {:body (str "non-blocking - " (t-name))})
  #_(delay (assoc context :response {:body (str "blocking - " (t-name))}))
  #_(CompletableFuture/supplyAsync
    (reify Supplier (get [_] (throw (ex-info "oops" {})))))
  #_(CompletableFuture/supplyAsync
      (reify Supplier (get [_] (assoc context :response {:body (str "async - " (t-name))})))))

#_(def http-handler (reify HttpHandler (handleRequest [_ e]
                                       (-> (.getResponseSender e)
                                           (.send "OK")))))

(defn start-test-server
  []
  (-> {:port 8086 :handler (-> -test-handler #_-test-middleware)}
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
