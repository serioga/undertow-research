(ns user.server-spin
  (:require [undertow-spin.handler :as spin-handler]
            [undertow.server :as server]
            [spin.response :as resp])
  (:import (java.util.concurrent CompletableFuture)))

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
  #_context
  (assoc context :response {:body "instant" :headers {"x-test" "handler"}
                            #_#_:status 226}
                 :response/status 404)
  #_(delay context)
  #_(delay (assoc context :response {:body "delay"}))
  #_(doto (CompletableFuture.) (as-> ft (future (.complete ft context))))
  #_(doto (CompletableFuture.) (as-> ft (future
                                        #_(Thread/sleep 100)
                                        (.complete ft (assoc context :response {:body "async"}))))))

(defn start-test-server
  []
  (-> {:port 8080 :handler (-> -test-handler -test-middleware)}
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
