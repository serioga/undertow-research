(ns user.server-jetty
  (:require [clojure.tools.logging :as log]
            [clojure.tools.logging.impl :as impl]
            [ring.adapter.jetty :as jetty])
  (:import (org.eclipse.jetty.server Server)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handler [request]
  {:status 200 :body "OK"})

(defn start-test-server []
  (jetty/run-jetty handler {:port 8088 :join? false}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! #(some-> ^Server % .stop)))

(defn start-server []
  (stop-server)
  (reset! server! (start-test-server)))

(start-server)

(comment
  (stop-server)
  (-> ^ch.qos.logback.classic.Logger (impl/get-logger log/*logger-factory* "org.eclipse.jetty")
      (.setLevel ch.qos.logback.classic.Level/INFO))
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
