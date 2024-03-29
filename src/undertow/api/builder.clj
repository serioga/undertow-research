(ns undertow.api.builder
  (:require [undertow.api.types :as types])
  (:import (io.undertow Undertow Undertow$Builder Undertow$ListenerBuilder Undertow$ListenerType)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmethod types/as-listener-builder :default
  [{:keys [port, host, https, handler, socket-options, use-proxy-protocol]
    {:keys [key-managers, trust-managers, ssl-context]} :https}]
  (-> (Undertow$ListenerBuilder.)
      (.setType (if https Undertow$ListenerType/HTTPS
                          Undertow$ListenerType/HTTP))
      (cond-> port (.setPort port))
      (.setHost (or host "localhost"))
      (.setRootHandler handler)
      (cond-> https (-> (.setKeyManagers key-managers)
                        (.setTrustManagers trust-managers)
                        (.setSslContext ssl-context)))
      (.setOverrideSocketOptions (types/as-option-map socket-options))
      (.setUseProxyProtocol (boolean use-proxy-protocol))))

(defn add-listener
  "Adds listener given builder instance or configuration map.

  Configuration options:

  - `:host`  The host name string, default \"localhost\".

  - `:https` HTTPS configuration map with options:
      - `:key-managers`   The instance of `javax.net.ssl.KeyManager[]`.
      - `:trust-managers` The instance of `javax.net.ssl.TrustManager[]`.
      - `:ssl-context`    The instance of `javax.net.ssl.SSLContext`.

  - `:handler` The listener HttpHandler to be used on the port.
               See [[server/start]] for details.

  - `:socket-options` The map of socket options for the listener.
      - `:undertow/enable-http2`. If HTTP2 protocol enabled, boolean.
      + Other option keywords can be found in `server` namespace.
      + Undertow option constants.

  - `:use-proxy-protocol` boolean.

  + The `:https` enables HTTPS protocol for the listener.
  + Declaration of AJP protocol is not supported.
  "
  (^Undertow$Builder
   [builder [port config]] (add-listener builder port config))
  (^Undertow$Builder
   [builder port config]
   (.addListener ^Undertow$Builder builder (-> (types/as-listener-builder config)
                                               (.setPort (int port))))))

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
  [builder {:keys [port, handler, buffer-size, io-threads, worker-threads, direct-buffers
                   server-options, socket-options, worker-options]}]
  (-> builder
      (apply-map add-listener (if (int? port) {port {}} port))
      (apply-map set-server-option server-options)
      (apply-map set-socket-option socket-options)
      (apply-map set-worker-option worker-options)
      (cond->
        handler,,,,,,,,,,,,,,,,, (.setHandler (types/as-handler handler))
        buffer-size,,,,,,,,,,,,, (.setBufferSize buffer-size)
        io-threads,,,,,,,,,,,,,, (.setIoThreads io-threads)
        worker-threads,,,,,,,,,, (.setWorkerThreads worker-threads)
        (some? direct-buffers),, (.setDirectBuffers (boolean direct-buffers)))))

(defn build
  ^Undertow
  [builder]
  (.build ^Undertow$Builder builder))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
