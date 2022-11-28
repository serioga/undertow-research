(ns undertow.builder
  (:import (clojure.lang IPersistentMap)
           (io.undertow Undertow$Builder Undertow$ListenerBuilder Undertow$ListenerType UndertowOptions)
           (io.undertow.server HttpHandler)
           (org.xnio Option Options)))

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
        ;; TODO: Set OptionMap
        #_(.setOverrideSocketOptions nil)
        (.setUseProxyProtocol (boolean use-proxy-protocol))))
  Undertow$ListenerBuilder
  (new-listener-builder [builder port] (.setPort builder port))
  HttpHandler
  (new-listener-builder [handler port] (new-listener-builder {:handler handler} port)))

(defn add-listener
  ^Undertow$Builder
  [^Undertow$Builder builder, [port opts]]
  (.addListener builder (new-listener-builder opts port)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn set-server-option
  ^Undertow$Builder
  [builder [k v]]
  (let [[k v] (as-option k v)]
    (.setServerOption ^Undertow$Builder builder k v)))

(defn set-socket-option
  ^Undertow$Builder
  [builder [k v]]
  (let [[k v] (as-option k v)]
    (.setSocketOption ^Undertow$Builder builder k v)))

(defn set-worker-option
  ^Undertow$Builder
  [builder [k v]]
  (let [[k v] (as-option k v)]
    (.setWorkerOption ^Undertow$Builder builder k v)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
