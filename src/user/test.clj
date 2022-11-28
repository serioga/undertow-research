(ns user.test
  (:require [undertow-ring.core :as ring]
            [undertow.core :as undertow])
  (:import (io.undertow Undertow Undertow$Builder)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers NameVirtualHostHandler RequestDumpingHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.util Headers)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(undertow/set-handler-fn-adapter ring/handler-fn-adapter)

(defn test-ring-handler
  [req]
  {:body (str "Hello World " req)
   :headers {"x-a" "1"
             "x-b" "2"
             "x-c" [3 4]
             #_#_"content-type" "xxx"}
   #_#_:status 404})

(defn test-ring-handler-fn
  ([] (test-ring-handler-fn "Hello World"))
  ([greet]
   (fn [req]
     {:body (str greet "\n\n" req)
      :session {:test "Test session value"}
      #_#_:headers {"x-a" "1"
                    "x-b" "2"
                    "x-c" [3 4]
                    #_#_"content-type" "xxx"}
      #_#_:status 200})))

(defn start-test-server
  []
  (undertow/start {:ports {8080 {:host "localhost"}}
                   :handler (test-ring-handler-fn "2")
                   :io-threads 6
                   #_#_:handler {:type :undertow/resource-handler
                                 :next-handler {:type :undertow/named-virtual-host-handler
                                                :hosts {"localhost" (test-ring-handler-fn "1")
                                                        "127.0.0.1" (test-ring-handler-fn "2")}}}
                   :wrap-handler [(fn [h] (RequestDumpingHandler. h))
                                  (undertow/wrap-session-attachment-handler {})
                                  #_(undertow/wrap-resource-handler {})]
                   #_#_:wrap-builder [(fn [^Undertow$Builder builder] (.setIoThreads builder 1))
                                  (fn [^Undertow$Builder builder] (.setIoThreads builder 4))]
                   :server-options {:undertow/enable-http2 true}
                   #_#_:worker-options {:xnio/worker-io-threads 2}})
  #_(doto (-> (Undertow/builder)
              #_(.addHttpListener 8080 nil (-> (NameVirtualHostHandler.)
                                               (.addHost "localhost" (-> (ring-handler-adapter test-ring-handler)
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
  (swap! server! undertow/stop))

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
