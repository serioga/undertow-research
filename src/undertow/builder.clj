(ns undertow.builder
  (:require [undertow.types :as types])
  (:import (clojure.lang IPersistentMap)
           (io.undertow Undertow Undertow$Builder Undertow$ListenerBuilder Undertow$ListenerType UndertowOptions)
           (io.undertow.server HttpHandler)
           (org.xnio Option OptionMap Options)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti as-option (fn [k _] k))

(defn define-option
  ([alias option] (define-option alias option identity))
  ([alias option value-fn]
   (defmethod as-option alias [_ v] [option (value-fn v)])))

;; TODO: Complete set of known undertow options
(define-option :undertow/max-header-size UndertowOptions/MAX_HEADER_SIZE)
(define-option :undertow/max-entity-size UndertowOptions/MAX_ENTITY_SIZE)
(define-option :undertow/multipart-max-entity-size UndertowOptions/MULTIPART_MAX_ENTITY_SIZE)
(define-option :undertow/max-parameters UndertowOptions/MAX_PARAMETERS)
(define-option :undertow/max-headers UndertowOptions/MAX_HEADERS)
(define-option :undertow/enable-http2 UndertowOptions/ENABLE_HTTP2)

(define-option :xnio/worker-io-threads Options/WORKER_IO_THREADS int)

(defmethod as-option :default
  [option value]
  (if (instance? Option option)
    [option value]
    (throw (ex-info (str "Unknown undertow option: " option "\n"
                         "Use `define-option` to define new options") {}))))

(comment
  (as-option :undertow/enable-http2 true)
  (as-option UndertowOptions/ENABLE_HTTP2 true)
  (as-option :xnio/worker-io-threads 4)
  )

(defn as-option-map
  ^OptionMap
  [m]
  (if (seq m)
    (-> (OptionMap/builder)
        (.add (->> m (into {} (map (fn [[k v]] (as-option k v))))))
        (.getMap))
    OptionMap/EMPTY))

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
        (.setOverrideSocketOptions (as-option-map socket-options))
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
   (let [[option value] (as-option option value)]
     (.setServerOption ^Undertow$Builder builder option value))))

(defn set-socket-option
  (^Undertow$Builder
   [builder [option value]] (set-socket-option builder option value))
  (^Undertow$Builder
   [builder option value]
   (let [[option value] (as-option option value)]
     (.setSocketOption ^Undertow$Builder builder option value))))

(defn set-worker-option
  (^Undertow$Builder
   [builder [option value]] (set-worker-option builder option value))
  (^Undertow$Builder
   [builder option value]
   (let [[option value] (as-option option value)]
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
