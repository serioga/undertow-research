(ns user.server-immutant
  (:require [immutant.web :as web]
            [immutant.web.middleware :as middleware]
            [user.main-handler :as main]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start-test-server []
  (web/run (-> (main/ring-handler-fn "immutant")
               (middleware/wrap-session))
           {:port 8084 :dispatch? false}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defonce server! (atom nil))

(defn stop-server []
  (swap! server! #(some-> % web/stop)))

(defn start-server []
  (stop-server)
  (reset! server! (start-test-server)))

(start-server)

(comment
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
