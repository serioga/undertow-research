(ns user.main_server
  (:require [undertow-ring.core :as ring]
            [undertow.adapter :as adapter]
            [undertow.handler :as handler]
            [undertow.server :as server]
            [undertow.websockets :as websocket]
            [user.main-handler :as main])
  (:import (io.undertow Undertow Undertow$Builder)
           (io.undertow.server Connectors HttpHandler)
           (io.undertow.server.handlers BlockingHandler NameVirtualHostHandler RequestDumpingHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.util Headers HttpString)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(adapter/set-fn-as-handler ring/fn-as-handler)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def -test-handler
  (reify HttpHandler
    (handleRequest [_ e]
      (-> (.getResponseHeaders e)
          (.add (HttpString. "Content-Type") "text/plain; charset=utf-8"))
      (.dispatch e ^Runnable (fn []
                               (prn [:start (.getName (Thread/currentThread))])
                               (Thread/sleep 20000)
                               (prn [:end (.getName (Thread/currentThread))])
                               (.endExchange e)))
      #_(.dispatch e ^Runnable
                 (fn []
                   (future
                     #p [:future (.getName (Thread/currentThread))]
                     (Thread/sleep 2000)
                     (Connectors/executeRootHandler
                       (reify HttpHandler
                         (handleRequest [_ ee]
                           #p [:handler (.getName (Thread/currentThread))]
                           (-> (.getResponseSender ee)
                               (.send "Hello, привет"))))
                       e))))
      #_(.dispatch e ^Runnable
                   (fn []
                     (future
                       #p [:future (.getName (Thread/currentThread))]
                       (Thread/sleep 2000)
                       (-> (.getResponseSender e)
                           (.send "Hello, привет"))))))))

(defn start-test-server
  []
  (-> {:ports {8080 {#_#_:socket-options {:xnio/worker-io-threads 2}}}
       #_#_:handler (websocket/handler {:on-open (fn [{:keys [channel context]}]
                                                   #p [:on-open context]
                                                   (websocket/send-text "What's up!" channel {}))
                                        :on-message (fn [{:keys [channel message context]}]
                                                      #p [:on-message message context]
                                                      (websocket/send-text (str "What " message "?") channel {}))
                                        :on-close (fn [params] #p [:on-close params])
                                        :on-error (fn [params] #p [:on-error params])})
       #_#_:handler [{:type handler/dispatch}
                     {:type handler/path-prefix :prefixes {"static" {:type handler/resource-handler :prefix "public/static"}}}]
       #_#_:handler (-> -test-handler #_(BlockingHandler.))
       :handler [{:type handler/graceful-shutdown}
                 {:type handler/proxy-peer-address}
                 {:type handler/simple-error-page}
                 {:type handler/virtual-host :hosts {"webapi.localtest.me" [{:type handler/simple-error-page}
                                                                            (main/ring-handler-fn "webapi")]}}
                 {:type handler/path-prefix
                  :prefixes {"static" [{:type handler/request-dump}
                                       {:type handler/resource-handler :prefix "public/static"}]}
                  :exacts {"ws" {:type websocket/handler
                                 :on-open (fn [{:keys [channel] :as event}]
                                            (prn event)
                                            (prn [:exchange (websocket/get-exchange channel)])
                                            (websocket/send-text "What's up!" channel {}))
                                 :on-message (fn [{:keys [channel text] :as event}]
                                               (prn event)
                                               (websocket/send-text (str "What " text "?") channel {}))
                                 :on-close (fn [event] (prn event))
                                 :on-error (fn [event] (prn event))}}}
                 {:type handler/session-attachment}
                 {:type handler/virtual-host :hosts {"localhost" [{:type handler/simple-error-page}
                                                                  {:type handler/request-dump}
                                                                  (-> (main/ring-handler-fn "localhost привет")
                                                                      (ring/as-non-blocking-sync-handler)
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