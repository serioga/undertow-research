(ns undertow.handler
  (:require [undertow.adapter :as adapter])
  (:import (clojure.lang Fn IPersistentMap MultiFn)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler ResourceManager)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionConfig SessionCookieConfig SessionManager)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti ^HttpHandler as-http-handler (some-fn :type type))

#_(def as-http-handler nil)
(.addMethod ^MultiFn as-http-handler HttpHandler identity)

(defmethod as-http-handler Fn
  [handler-fn] (adapter/*handler-fn-adapter* handler-fn))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn path-prefix
  (^PathHandler
   [opts] (path-prefix nil opts))
  (^PathHandler
   [default-handler {:keys [paths cache-size]}]
   (reduce (fn [^PathHandler this [path {:keys [handler exact?]}]]
             (if exact?
               (.addExactPath this path (as-http-handler handler))
               (.addPrefixPath this path (as-http-handler handler))))
           (cond
             (and default-handler cache-size)
             (PathHandler. (as-http-handler default-handler) cache-size)
             default-handler
             (PathHandler. (as-http-handler default-handler))
             cache-size
             (PathHandler. (int cache-size))
             :else
             (PathHandler.))
           paths)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(defmethod as-http-handler :undertow/name-virtual-host-handler
    [{:keys [hosts, default-handler]}]
    (cond-> ^NameVirtualHostHandler
            (reduce (fn [handler [host opts]]
                      (.addHost ^NameVirtualHostHandler handler host (as-http-handler opts)))
                    (NameVirtualHostHandler.)
                    hosts)
      default-handler (.setDefaultHandler (as-http-handler default-handler))))

(defn virtual-host
  (^NameVirtualHostHandler
   [{:keys [hosts]}]
   (reduce (fn [this [host {:keys [handler]}]]
             (.addHost ^NameVirtualHostHandler this host (as-http-handler handler)))
           (NameVirtualHostHandler.)
           hosts))
  (^NameVirtualHostHandler
   [default-handler opts]
   (-> (virtual-host opts)
       (.setDefaultHandler (as-http-handler default-handler)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(defmethod as-http-handler :undertow/resource-handler
    [{:keys [prefix, next-handler] :or {prefix "public"}}]
    (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                                 ^String prefix)
                      (some-> next-handler as-http-handler)))

(defn- resource-manager
  ^ResourceManager
  [{:keys [prefix] :or {prefix "public"}}]
  (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                             ^String prefix))

(defn resource-handler
  (^ResourceHandler
   [opts]
   (ResourceHandler. (resource-manager opts)))
  (^ResourceHandler
   [next-handler opts]
   (ResourceHandler. (resource-manager opts) (as-http-handler next-handler))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti as-session-manager (some-fn :type type))
(.addMethod ^MultiFn as-session-manager SessionManager identity)

(defn in-memory-session-manager
  [{:keys [session-id-generator
           deployment-name
           max-sessions
           expire-oldest-unused-session-on-max
           statistics-enabled]
    :or {max-sessions 0, expire-oldest-unused-session-on-max true}}]
  (InMemorySessionManager. (or session-id-generator (SecureRandomSessionIdGenerator.)),
                           deployment-name
                           max-sessions
                           expire-oldest-unused-session-on-max
                           (boolean statistics-enabled)))

(.addMethod ^MultiFn as-session-manager IPersistentMap in-memory-session-manager)

(defmulti as-session-config (some-fn :type type))
(.addMethod ^MultiFn as-session-config SessionConfig identity)

(defn session-cookie-config
  [{:keys [cookie-name path domain discard secure http-only max-age comment]}]
  (cond-> (SessionCookieConfig.)
    cookie-name (.setCookieName cookie-name)
    path (.setPath path)
    domain (.setDomain domain)
    (some? discard) (.setDiscard (boolean discard))
    (some? secure) (.setSecure (boolean secure))
    (some? http-only) (.setHttpOnly (boolean http-only))
    max-age (.setMaxAge max-age)
    comment (.setComment comment)))

(.addMethod ^MultiFn as-session-config IPersistentMap session-cookie-config)

#_(defmethod as-http-handler :undertow/session-attachment-handler
    [{:keys [session-manager, session-config, next-handler]
      :or {session-manager {} session-config {}}}]
    (if next-handler
      (SessionAttachmentHandler. (as-http-handler next-handler)
                                 (as-session-manager session-manager)
                                 (as-session-config session-config))
      (SessionAttachmentHandler. (as-session-manager session-manager)
                                 (as-session-config session-config))))

(defn session-attachment
  ^SessionAttachmentHandler
  [next-handler {:keys [session-manager, session-config]
                 :or {session-manager {} session-config {}}}]
  (SessionAttachmentHandler. (as-http-handler next-handler)
                             (as-session-manager session-manager)
                             (as-session-config session-config)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn proxy-peer-address
  "Handler that sets the peer address to the value of the X-Forwarded-For
  header.

  This should only be used behind a proxy that always sets this header,
  otherwise it is possible for an attacker to forge their peer address."
  ^ProxyPeerAddressHandler
  [next-handler]
  (ProxyPeerAddressHandler. next-handler))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-error-page
  "Handler that generates an extremely simple no frills error page."
  ^SimpleErrorPageHandler
  [next-handler]
  (SimpleErrorPageHandler. (as-http-handler next-handler)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn graceful-shutdown
  ^HttpHandler
  [next-handler]
  (GracefulShutdownHandler. (as-http-handler next-handler)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  ^HttpHandler
  [next-handler]
  (RequestDumpingHandler. (as-http-handler next-handler)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
