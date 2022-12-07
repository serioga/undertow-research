(ns user.server-adapter
  (:require [ring.adapter.undertow :as undertow]
            [ring.adapter.undertow.middleware.session :as session]
            [ring.adapter.undertow.websocket :as ws]
            [user.main-handler :as main])
  (:import (io.undertow Undertow)
           (io.undertow.websockets.core WebSocketChannel)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn websocket-response
  [response request]
  (assoc response :undertow/websocket {:on-message (fn [{:keys [channel data]}]
                                                     (if (= "bye" data)
                                                       (.sendClose ^WebSocketChannel channel)
                                                       (ws/send (str "OK: " data) channel)))}))

(defn start-test-server []
  (undertow/run-undertow (-> (main/ring-handler-fn "adapter привет" {:websocket-response-fn websocket-response})
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
