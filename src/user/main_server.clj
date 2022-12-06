(ns user.main_server
  (:require [undertow-ring.core :as ring]
            [undertow.adapter :as adapter]
            [undertow.handler :as handler]
            [undertow.server :as server]
            [undertow.websocket :as websocket]
            [user.main-handler :as main])
  (:import (io.undertow Handlers Undertow Undertow$Builder)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers BlockingHandler NameVirtualHostHandler RequestDumpingHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.util Headers)
           (io.undertow.websockets WebSocketConnectionCallback)
           (io.undertow.websockets.core AbstractReceiveListener WebSocketChannel)
           (io.undertow.websockets.spi WebSocketHttpExchange)
           (org.xnio ChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(adapter/set-handler-fn-adapter ring/handler-fn-adapter)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start-test-server
  []
  (-> {:ports {8080 {}}
       :handler (-> (Handlers/websocket (reify WebSocketConnectionCallback
                                          (^void onConnect [_ ^WebSocketHttpExchange exchange ^WebSocketChannel channel]
                                            #_[exchange channel (.getName (Thread/currentThread))]
                                            #_(on-open {:channel channel})
                                            (-> (.getReceiveSetter channel)
                                                (.set (websocket/channel-listener
                                                        {:on-message (fn [{:keys [channel message]}] #_[:on-message message]
                                                                       (websocket/send-text (str "What " message "?") channel {}))
                                                         :on-close (fn [params] #_[:on-close params])
                                                         :on-error (fn [params] #_[:on-error params])})))
                                            (.resumeReceives channel))))
                    (BlockingHandler.)
                    (handler/path-prefix {:prefixes {"static" {:type handler/resource-handler :prefix "public/static"}}}))
       #_#_:handler [{:type handler/dispatch}
                     {:type handler/path-prefix :prefixes {"static" {:type handler/resource-handler :prefix "public/static"}}}]
       #_#_:handler [{:type handler/graceful-shutdown}
                     {:type handler/proxy-peer-address}
                     {:type handler/simple-error-page}
                     {:type handler/virtual-host :hosts {"webapi.localtest.me" [{:type handler/simple-error-page}
                                                                                (main/ring-handler-fn "webapi")]}}
                     {:type handler/path-prefix :prefixes {"static" [{:type handler/request-dump}
                                                                     {:type handler/resource-handler :prefix "public/static"}]}}
                     {:type handler/session-attachment}
                     {:type handler/virtual-host :hosts {"localhost" [{:type handler/simple-error-page}
                                                                      {:type handler/request-dump}
                                                                      (-> (main/ring-handler-fn "localhost привет")
                                                                          (ring/as-non-blocking-handler)
                                                                          #_(ring/as-async-handler))]
                                                         "127.0.0.1" (main/ring-handler-fn "127.0.0.1")}}
                     (main/ring-handler-fn "localhost")]
       #_#_:handler (-> (test-ring-handler-fn "default")
                        (handler/virtual-host {:hosts {"localhost" (-> (test-ring-handler-fn "localhost")
                                                                       (handler/request-dump))
                                                       "127.0.0.1" (test-ring-handler-fn "127.0.0.1")}})
                        (handler/session-attachment {})
                        (handler/path-prefix {:prefixes {"static" (handler/resource-handler {:prefix "public/static"})}})
                        (handler/virtual-host {:hosts {"webapi.localtest.me" (test-ring-handler-fn "webapi")}})
                        (handler/simple-error-page)
                        (handler/proxy-peer-address)
                        (handler/graceful-shutdown))
       :instance-data {:source `start-test-server}}
      (server/start))
  #_(server/start {:ports {8080 {:host "localhost"}}
                   :handler (-> (test-ring-handler-fn "2")
                                (handler/session-attachment {})
                                #_(handler/resource-handler {})
                                (RequestDumpingHandler.))
                   :io-threads 6
                   #_#_:handler {:type :undertow/resource-handler
                                 :next-handler {:type :undertow/name-virtual-host-handler
                                                :hosts {"localhost" (test-ring-handler-fn "1")
                                                        "127.0.0.1" (test-ring-handler-fn "2")}}}
                   :wrap-builder-fn (fn [builder-fn]
                                      (fn [builder options]
                                        (-> ^Undertow$Builder (builder-fn builder options)
                                            (.setIoThreads 4))))
                   :server-options {:undertow/enable-http2 true}
                   #_#_:worker-options {:xnio/worker-io-threads 2}})
  #_(doto (-> (Undertow/builder)
              #_(.addHttpListener 8080 nil (-> (NameVirtualHostHandler.)
                                               (.addHost "localhost" (-> (ring-handler-adapter (test-ring-handler-fn "localhost"))
                                                                         (SetHeaderHandler. "Content-Type" "text/plain")))
                                               (.addHost "127.0.0.1" (-> (reify HttpHandler (handleRequest [_ exchange]
                                                                                              (doto exchange
                                                                                                (-> (.getResponseHeaders)
                                                                                                    (.put Headers/CONTENT_TYPE "text/plain"))
                                                                                                (-> (.getResponseSender)
                                                                                                    (.send "Hello World (127.0.0.1)")))))
                                                                         (SetHeaderHandler. "Content-Type" "text/plain")))
                                               (RequestDumpingHandler.)))
              #_(.addHttpListener 8081 "localhost"
                                  (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                                                               "public")))
              #_(add-listener [8080 (-> (ring-handler-adapter (test-ring-handler-fn "1"))
                                        (RequestDumpingHandler.))])
              (add-listener [8080 {}])
              #_(.addHttpListener 8080 nil (comment (-> (ring-handler-adapter (test-ring-handler-fn "1"))
                                                        (RequestDumpingHandler.))))
              (.setHandler (-> (ring-handler-adapter (test-ring-handler-fn "2"))
                               (RequestDumpingHandler.)))
              (.build))
      (.start)))

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
  ; exchange -> request -> response -> exchange
  ; chain handlers

  {:ports {8080 {:host "localhost"
                 :https {:key-managers [] :trust-managers []}
                 #_#_:https {:ssl-context nil}
                 :handler nil
                 :socket-options {} :use-proxy-protocol false}}
   :handler nil
   :wrap-handler nil}


  {:name-virtual-host-handler {"localhost" (fn [])}}
  (init-server)
  (stop-server)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,