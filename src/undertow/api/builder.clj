(ns undertow.api.builder
  (:require [undertow.api.types :as types])
  (:import (clojure.lang IPersistentMap)
           (io.undertow Undertow Undertow$Builder Undertow$ListenerBuilder Undertow$ListenerType)
           (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol HttpListenerBuilder
  (new-listener-builder [opts port]))

(extend-protocol HttpListenerBuilder
  IPersistentMap
  ;; TODO: Document, that it covers only HTTP/HTTPS but not AJP
  (new-listener-builder
    [{:keys [host https handler socket-options use-proxy-protocol] :or {host "localhost"}}
     port]
    (-> (Undertow$ListenerBuilder.)
        (.setType (if https Undertow$ListenerType/HTTPS
                            Undertow$ListenerType/HTTP))
        (.setPort port)
        (.setHost host)
        (.setRootHandler handler)
        (.setKeyManagers,, (:key-managers https))
        (.setTrustManagers (:trust-managers https))
        (.setSslContext,,, (:ssl-context https))
        (.setOverrideSocketOptions (types/as-option-map socket-options))
        (.setUseProxyProtocol (boolean use-proxy-protocol))))
  Undertow$ListenerBuilder
  (new-listener-builder [builder port] (.setPort builder port))
  HttpHandler
  (new-listener-builder [handler port] (new-listener-builder {:handler handler} port)))

(defn add-listener
  (^Undertow$Builder
   [builder [port opts]] (add-listener builder port opts))
  (^Undertow$Builder
   [builder port opts]
   (.addListener ^Undertow$Builder builder (new-listener-builder opts port))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn set-server-option
  (^Undertow$Builder
   [builder [option value]] (set-server-option builder option value))
  (^Undertow$Builder
   [builder option value]
   (let [[option value] (types/as-option option value)]
     (.setServerOption ^Undertow$Builder builder option value))))

(defn set-socket-option
  (^Undertow$Builder
   [builder [option value]] (set-socket-option builder option value))
  (^Undertow$Builder
   [builder option value]
   (let [[option value] (types/as-option option value)]
     (.setSocketOption ^Undertow$Builder builder option value))))

(defn set-worker-option
  (^Undertow$Builder
   [builder [option value]] (set-worker-option builder option value))
  (^Undertow$Builder
   [builder option value]
   (let [[option value] (types/as-option option value)]
     (.setWorkerOption ^Undertow$Builder builder option value))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- apply-map
  ^Undertow$Builder
  [builder set-fn entries]
  (reduce set-fn builder entries))

(defn configure
  [builder {:keys [ports, handler, buffer-size, io-threads, worker-threads, direct-buffers
                   server-options, socket-options, worker-options]}]
  (-> builder
      (apply-map add-listener ports)
      (apply-map set-server-option server-options)
      (apply-map set-socket-option socket-options)
      (apply-map set-worker-option worker-options)
      (cond->
        handler,,,,,,, (.setHandler (types/as-handler handler))
        buffer-size,,, (.setBufferSize buffer-size)
        io-threads,,,, (.setIoThreads io-threads)
        worker-threads (.setWorkerThreads worker-threads)
        direct-buffers (.setDirectBuffers direct-buffers))))

(defn build
  ^Undertow
  [builder]
  (.build ^Undertow$Builder builder))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,