(ns undertow.handler
  (:require [undertow.adapter :as adapter])
  (:import (clojure.lang Fn IPersistentMap MultiFn Sequential)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers BlockingHandler GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler ResourceManager)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionConfig SessionCookieConfig SessionManager)))

(set! *warn-on-reflection* true)

;; TODO: Use io.undertow.Handlers to create handlers

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def handler-type (some-fn :type type))

#_(do (def as-handler-type nil) (def as-wrapper-type nil))

(defmulti handler-impl handler-type)

(defprotocol HandlerImpl
  (as-handler ^io.undertow.server.HttpHandler [obj])
  (as-wrapper [obj]))

(defn wrap-handler
  [handler with]
  (reduce (fn [handler wrapper] ((as-wrapper wrapper) (as-handler handler)))
          (as-handler handler)
          (reverse with)))

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
  (as-handler [xs] (when-let [xs (seq xs)]
                     ;; TODO: Raise exception for empty seq?
                     (wrap-handler (last xs) (butlast xs)))))

(defn declare-type
  [t {as-handler-fn :as-handler, as-wrapper-fn :as-wrapper, alias :type-alias}]
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

(defn as-wrapper-2-arity
  [f]
  (fn [opts] (fn [handler] (f handler opts))))

(defn as-wrapper-1-arity
  [f]
  (fn [_] f))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn dispatch
  ;; TODO: Update docstring
  "A HttpHandler that initiates a blocking request. If the thread is currently
  running in the io thread it will be dispatched."
  [handler]
  (let [handler (as-handler handler)]
    (reify HttpHandler
      (handleRequest [_ e]
        (if (.isInIoThread e)
          (.dispatch e handler)
          (.handleRequest handler e))))))

(declare-type dispatch {:type-alias ::dispatch
                        :as-wrapper (as-wrapper-1-arity dispatch)})

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

(declare-type path-prefix {:type-alias ::path-prefix
                           :as-handler path-prefix
                           :as-wrapper (as-wrapper-2-arity path-prefix)})

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

(declare-type virtual-host {:type-alias ::virtual-host
                            :as-handler virtual-host
                            :as-wrapper (as-wrapper-2-arity virtual-host)})

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

(declare-type resource-handler {:type-alias ::resource-handler
                                :as-handler resource-handler
                                :as-wrapper (as-wrapper-2-arity resource-handler)})

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

(declare-type session-attachment {:type-alias ::session-attachment
                                  :as-wrapper (as-wrapper-2-arity session-attachment)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn proxy-peer-address
  "Handler that sets the peer address to the value of the X-Forwarded-For
  header.

  This should only be used behind a proxy that always sets this header,
  otherwise it is possible for an attacker to forge their peer address."
  ^ProxyPeerAddressHandler
  [next-handler]
  (ProxyPeerAddressHandler. next-handler))

(declare-type proxy-peer-address {:type-alias ::proxy-peer-address
                                  :as-wrapper (as-wrapper-1-arity proxy-peer-address)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-error-page
  "Handler that generates an extremely simple no frills error page."
  ^SimpleErrorPageHandler
  [next-handler]
  (SimpleErrorPageHandler. (as-handler next-handler)))

(declare-type simple-error-page {:type-alias ::simple-error-page
                                 :as-wrapper (as-wrapper-1-arity simple-error-page)})

(comment
  (as-handler {:type simple-error-page})
  (as-handler {:type ::simple-error-page})
  ((as-wrapper {:type simple-error-page}) identity)
  ((as-wrapper {:type ::simple-error-page}) identity)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Check undertow implementation
(defn graceful-shutdown
  ^HttpHandler
  [next-handler]
  (GracefulShutdownHandler. (as-handler next-handler)))

(declare-type graceful-shutdown {:type-alias ::graceful-shutdown
                                 :as-wrapper (as-wrapper-1-arity graceful-shutdown)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  ^HttpHandler
  [next-handler]
  (RequestDumpingHandler. (as-handler next-handler)))

(fn [next-handler _] (request-dump next-handler))

(declare-type request-dump {:type-alias ::request-dump
                            :as-wrapper (as-wrapper-1-arity request-dump)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
