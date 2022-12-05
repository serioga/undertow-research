(ns user.server-adapter
  (:require [ring.adapter.undertow :as undertow]
            [ring.adapter.undertow.middleware.session :as session]
            [user.main-handler :as main])
  (:import (io.undertow Undertow)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start-test-server []
  (undertow/run-undertow (-> (main/ring-handler-fn "adapter привет")
                             (session/wrap-session))
                         {:port 8082}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! #(some-> ^Undertow % .stop)))

(defn start-server []
  (stop-server)
  (reset! server! (start-test-server)))

(start-server)

(comment
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
