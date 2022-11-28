(ns undertow.core
  (:require [undertow.builder :as builder])
  (:import (clojure.lang Fn IPersistentMap MultiFn)
           (io.undertow Undertow Undertow$Builder)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers NameVirtualHostHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionConfig SessionCookieConfig SessionManager)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:dynamic *handler-fn-adapter* identity)

(defn set-handler-fn-adapter
  [f]
  (alter-var-root #'*handler-fn-adapter* (constantly f)))

(defmulti ^HttpHandler as-http-handler (some-fn :type type))

#_(def as-http-handler nil)
(.addMethod ^MultiFn as-http-handler HttpHandler identity)

(defmethod as-http-handler Fn
  [handler-fn] (*handler-fn-adapter* handler-fn))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Separate namespace for built-in adapters

(defmethod as-http-handler :undertow/named-virtual-host-handler
  [{:keys [hosts, default-handler]}]
  (cond-> ^NameVirtualHostHandler
          (reduce (fn [handler [host opts]]
                    (.addHost ^NameVirtualHostHandler handler host (as-http-handler opts)))
                  (NameVirtualHostHandler.)
                  hosts)
    default-handler (.setDefaultHandler (as-http-handler default-handler))))

(defmethod as-http-handler :undertow/resource-handler
  [{:keys [path-prefix, next-handler] :or {path-prefix "public"}}]
  (ResourceHandler. (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                                               ^String path-prefix)
                    (some-> next-handler as-http-handler)))

(defn wrap-resource-handler
  [opts]
  (fn [handler]
    (as-http-handler (merge opts {:type :undertow/resource-handler
                                  :next-handler handler}))))

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

(defmethod as-http-handler :undertow/session-attachment-handler
  [{:keys [session-manager, session-config, next-handler]
    :or {session-manager {} session-config {}}}]
  (if next-handler
    (SessionAttachmentHandler. (as-http-handler next-handler)
                               (as-session-manager session-manager)
                               (as-session-config session-config))
    (SessionAttachmentHandler. (as-session-manager session-manager)
                               (as-session-config session-config))))

(defn wrap-session-attachment-handler
  [opts]
  (fn [handler]
    (as-http-handler (merge opts {:type :undertow/session-attachment-handler
                                  :next-handler handler}))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- wrap-with
  "Applies wrap function or a sequence of wrap functions to the `x`."
  [x fs]
  (if (sequential? fs)
    (->> (reverse fs)
         (reduce (fn [obj f] (f obj)) x))
    (fs x)))

(defn- apply-map
  ^Undertow$Builder
  [builder set-fn entries]
  (reduce set-fn builder entries))

(defn build-server
  ^Undertow [{:keys [ports, handler, wrap-handler, wrap-builder
                     server-options, socket-options, worker-options]}]
  (-> (Undertow/builder)
      (apply-map builder/add-listener ports)
      (apply-map builder/set-server-option server-options)
      (apply-map builder/set-socket-option socket-options)
      (apply-map builder/set-worker-option worker-options)
      (cond-> handler (.setHandler (cond-> (as-http-handler handler)
                                     wrap-handler (wrap-with wrap-handler)))
              wrap-builder ^Undertow$Builder (wrap-with wrap-builder))
      (.build)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start
  ^Undertow [options]
  (doto (build-server options) .start))

(defn stop
  [^Undertow server]
  (some-> server .stop))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
