(ns user.server-spin
  (:require [spin-undertow.handler :as spin-handler]
            [undertow.server :as server])
  (:import (io.undertow.server HttpHandler)
           (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(server/set-handler-fn-adapter spin-handler/http-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -test-middleware
  [handler]
  (fn [context]
    (handler (update context :response/headers
                     conj ["X-Test-Middleware" "was here"] ["X-Test" "middleware"]))))

(defn -test-handler
  [context]
  #_(throw (ex-info "oops" {}))
  context
  #_(assoc context :response {:body "instant" :headers {"x-test" "handler"}
                            #_#_:status 226}
                 :response/status 404)
  #_(delay context)
  #_(delay (assoc context :response {:body "delay"}))
  #_(doto (CompletableFuture.) (as-> ft (future (.complete ft context))))
  #_(doto (CompletableFuture.) (as-> ft (future (.completeExceptionally ft (ex-info "oops" {})))))
  #_(doto (CompletableFuture.) (as-> ft (future
                                        #_(Thread/sleep 100)
                                        (.complete ft (assoc context :response {:body "async"}))))))

#_(def http-handler (reify HttpHandler (handleRequest [_ e]
                                       (-> (.getResponseSender e)
                                           (.send "OK")))))

(defn start-test-server
  []
  (-> {:port 8080 :handler (-> -test-handler #_-test-middleware)}
      (server/start)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! (fn [instance] (some-> instance server/stop))))

#_(defn stop-server []
    (swap! server! (fn [instance] (some-> ^Undertow instance .stop))))

(defn init-server []
  (stop-server)
  (reset! server! (start-test-server)))

(init-server)

(comment
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
