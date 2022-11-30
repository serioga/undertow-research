(ns undertow.handler
  (:require [undertow.adapter :as adapter])
  (:import (clojure.lang Fn IPersistentMap MultiFn Sequential)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler ResourceManager)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionConfig SessionCookieConfig SessionManager)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def handler-type (some-fn :type type))

#_(def as-http-handler nil)
;; TODO: Decide about order arguments
(defmulti as-http-handler {:tag HttpHandler :arglists '([handler] [next-handler, handler])}
          (fn
            ([handler] (handler-type handler))
            ([_ handler] (handler-type handler))))

;; TODO: Add 2-arity to HttpHandler coercion?
(.addMethod ^MultiFn as-http-handler HttpHandler
            (fn
              ([handler] handler)
              ([handler, next-handler]
               (throw (ex-info (str "Cannot chain handlers " [handler '-> next-handler]) {})))))

(defmethod as-http-handler Fn
  [handler-fn]
  (adapter/*handler-fn-adapter* handler-fn))

(defmethod as-http-handler Sequential
  [handlers]
  (when-let [xs (some-> handlers seq reverse)]
    (reduce as-http-handler (as-http-handler (first xs)) (rest xs))))

(defmulti as-register-method (fn [k _] k))

(defmethod as-register-method :default [_ handler-fn] handler-fn)

(defmethod as-register-method ::not-configurable
  [_ handler-fn]
  (fn not-configurable-method
    ([_] (handler-fn))
    ([next-handler _]
     (handler-fn next-handler))))

(defn register-handler-type
  [handler-fn & {:keys [alias, as]}]
  (assert (fn? handler-fn))
  (let [method (as-register-method as handler-fn)]
    (doseq [dispatch-val (cond-> [handler-fn] alias (conj alias))]
      (.addMethod ^MultiFn as-http-handler dispatch-val method))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn path-prefix
  (^PathHandler
   [opts] (path-prefix nil opts))
  (^PathHandler
   [default-handler {:keys [prefixes exacts cache-size]}]
   (letfn [(add-prefix-path [this [path handler]]
             (.addPrefixPath ^PathHandler this path (as-http-handler handler)))
           (add-exact-path [this [path handler]]
             (.addExactPath ^PathHandler this path (as-http-handler handler)))]
     (as->
       (cond (and default-handler cache-size) (PathHandler. (as-http-handler default-handler) cache-size)
             default-handler,,,,,,,,,,,,,,,,, (PathHandler. (as-http-handler default-handler))
             cache-size,,,,,,,,,,,,,,,,,,,,,, (PathHandler. (int cache-size))
             :else,,,,,,,,,,,,,,,,,,,,,,,,,,, (PathHandler.))
       handler
       (reduce add-prefix-path handler prefixes)
       (reduce add-exact-path handler exacts)))))

(register-handler-type path-prefix :alias ::path-prefix)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn virtual-host
  (^NameVirtualHostHandler
   [{:keys [hosts]}]
   (reduce (fn [this [host handler]]
             (.addHost ^NameVirtualHostHandler this host (as-http-handler handler)))
           (NameVirtualHostHandler.)
           hosts))
  (^NameVirtualHostHandler
   [default-handler opts]
   (-> (virtual-host opts)
       (.setDefaultHandler (as-http-handler default-handler)))))

(register-handler-type virtual-host :alias ::virtual-host)

(comment
  (as-http-handler {:type virtual-host :hosts {"localhost" identity}})
  (as-http-handler {:type ::virtual-host :hosts {"localhost" identity}})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

(register-handler-type resource-handler :alias ::resource-handler)

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

(defn session-attachment
  ^SessionAttachmentHandler
  [next-handler {:keys [session-manager, session-config]
                 :or {session-manager {} session-config {}}}]
  (SessionAttachmentHandler. (as-http-handler next-handler)
                             (as-session-manager session-manager)
                             (as-session-config session-config)))

(register-handler-type session-attachment :alias ::session-attachment)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn proxy-peer-address
  "Handler that sets the peer address to the value of the X-Forwarded-For
  header.

  This should only be used behind a proxy that always sets this header,
  otherwise it is possible for an attacker to forge their peer address."
  ^ProxyPeerAddressHandler
  [next-handler]
  (ProxyPeerAddressHandler. next-handler))

(register-handler-type proxy-peer-address :alias ::proxy-peer-address :as ::not-configurable)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-error-page
  "Handler that generates an extremely simple no frills error page."
  ^SimpleErrorPageHandler
  [next-handler]
  (SimpleErrorPageHandler. (as-http-handler next-handler)))

(register-handler-type simple-error-page :alias ::simple-error-page :as ::not-configurable)

(comment
  (as-http-handler {:type simple-error-page})
  (as-http-handler {:type ::simple-error-page})
  (as-http-handler {:type simple-error-page} identity)
  (as-http-handler {:type ::simple-error-page} identity)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn graceful-shutdown
  ^HttpHandler
  [next-handler]
  (GracefulShutdownHandler. (as-http-handler next-handler)))

(register-handler-type graceful-shutdown :alias ::graceful-shutdown :as ::not-configurable)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  ^HttpHandler
  [next-handler]
  (RequestDumpingHandler. (as-http-handler next-handler)))

(fn [next-handler _] (request-dump next-handler))

(register-handler-type request-dump :alias ::request-dump :as ::not-configurable)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
