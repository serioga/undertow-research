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

#_(do (def as-handler-type nil) (def as-wrapper-type nil))

(defmulti handler-impl handler-type)

(defprotocol HandlerImpl
  (as-handler ^HttpHandler [obj])
  (as-wrapper [obj]))

(extend-protocol HandlerImpl
  HttpHandler
  (as-handler [handler] handler)
  Fn
  (as-handler [handler-fn] (adapter/*handler-fn-adapter* handler-fn))
  (as-wrapper [wrapper-fn] wrapper-fn)
  IPersistentMap
  (as-handler [m] (as-handler (handler-impl m)))
  (as-wrapper [m] (as-wrapper (handler-impl m)))
  Sequential
  (as-handler [xs] (when-let [xs (some-> xs seq reverse)]
                     ;; TODO: Raise exception for empty seq?
                     (reduce (fn [handler wrapper] ((as-wrapper wrapper) (as-handler handler)))
                             (as-handler (first xs)) (rest xs)))))

(defn declare-type-impl
  [t {as-handler-fn :as-handler, as-wrapper-fn :as-wrapper, alias :alias}]
  (assert (or (fn? as-handler-fn) (fn? as-wrapper-fn)))
  (letfn [(impl-method [obj]
            (reify HandlerImpl
              (as-handler [_]
                (if as-handler-fn
                  (as-handler-fn obj)
                  (throw (ex-info (str "Cannot be user as HttpHandler: " obj) {}))))
              (as-wrapper [_]
                (if as-wrapper-fn
                  (as-wrapper-fn obj)
                  (throw (ex-info (str "Cannot be user as handler wrapper: " obj) {}))))))]
    (.addMethod ^MultiFn handler-impl t impl-method)
    (when alias
      (.addMethod ^MultiFn handler-impl alias impl-method))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn path-prefix
  (^PathHandler
   [opts] (path-prefix nil opts))
  (^PathHandler
   [default-handler {:keys [prefixes exacts cache-size]}]
   (letfn [(add-prefix-path [this [path handler]]
             (.addPrefixPath ^PathHandler this path (as-handler handler)))
           (add-exact-path [this [path handler]]
             (.addExactPath ^PathHandler this path (as-handler handler)))]
     (as->
       (cond (and default-handler cache-size) (PathHandler. (as-handler default-handler) cache-size)
             default-handler,,,,,,,,,,,,,,,,, (PathHandler. (as-handler default-handler))
             cache-size,,,,,,,,,,,,,,,,,,,,,, (PathHandler. (int cache-size))
             :else,,,,,,,,,,,,,,,,,,,,,,,,,,, (PathHandler.))
       handler
       (reduce add-prefix-path handler prefixes)
       (reduce add-exact-path handler exacts)))))

(declare-type-impl path-prefix {:alias ::path-prefix
                                :as-handler path-prefix
                                :as-wrapper (fn [opts] (fn [handler] (path-prefix handler opts)))})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn virtual-host
  (^NameVirtualHostHandler
   [{:keys [hosts]}]
   (reduce (fn [this [host handler]]
             (.addHost ^NameVirtualHostHandler this host (as-handler handler)))
           (NameVirtualHostHandler.)
           hosts))
  (^NameVirtualHostHandler
   [default-handler opts]
   (-> (virtual-host opts)
       (.setDefaultHandler (as-handler default-handler)))))

(declare-type-impl virtual-host {:alias ::virtual-host
                                 :as-handler virtual-host
                                 :as-wrapper (fn [opts] (fn [handler] (virtual-host handler opts)))})

(comment
  (as-handler {:type virtual-host :hosts {"localhost" identity}})
  (as-handler {:type ::virtual-host :hosts {"localhost" identity}})
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
   (ResourceHandler. (resource-manager opts) (as-handler next-handler))))

(declare-type-impl resource-handler {:alias ::resource-handler
                                     :as-handler resource-handler
                                     :as-wrapper (fn [opts] (fn [handler] (resource-handler handler opts)))})

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
  (SessionAttachmentHandler. (as-handler next-handler)
                             (as-session-manager session-manager)
                             (as-session-config session-config)))

(declare-type-impl session-attachment {:alias ::session-attachment
                                       :as-wrapper (fn [opts] (fn [handler] (session-attachment handler opts)))})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn proxy-peer-address
  "Handler that sets the peer address to the value of the X-Forwarded-For
  header.

  This should only be used behind a proxy that always sets this header,
  otherwise it is possible for an attacker to forge their peer address."
  ^ProxyPeerAddressHandler
  [next-handler]
  (ProxyPeerAddressHandler. next-handler))

(declare-type-impl proxy-peer-address {:alias ::proxy-peer-address
                                       :as-wrapper (constantly proxy-peer-address)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-error-page
  "Handler that generates an extremely simple no frills error page."
  ^SimpleErrorPageHandler
  [next-handler]
  (SimpleErrorPageHandler. (as-handler next-handler)))

(declare-type-impl simple-error-page {:alias ::simple-error-page
                                      :as-wrapper (constantly simple-error-page)})

(comment
  (as-handler {:type simple-error-page})
  (as-handler {:type ::simple-error-page})
  ((as-wrapper {:type simple-error-page}) identity)
  ((as-wrapper {:type ::simple-error-page}) identity)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn graceful-shutdown
  ^HttpHandler
  [next-handler]
  (GracefulShutdownHandler. (as-handler next-handler)))

(declare-type-impl graceful-shutdown {:alias ::graceful-shutdown
                                      :as-wrapper (constantly graceful-shutdown)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  ^HttpHandler
  [next-handler]
  (RequestDumpingHandler. (as-handler next-handler)))

(fn [next-handler _] (request-dump next-handler))

(declare-type-impl request-dump {:alias ::request-dump
                                 :as-wrapper (constantly request-dump)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
