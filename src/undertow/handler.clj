(ns undertow.handler
  (:require [undertow.api.types :as types]
            [undertow.websocket.handler :as websocket])
  (:import (clojure.lang IPersistentMap MultiFn Sequential)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler ResourceManager)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionConfig SessionCookieConfig SessionManager)
           (io.undertow.websockets WebSocketProtocolHandshakeHandler)))

(set! *warn-on-reflection* true)

;; TODO: Use io.undertow.Handlers to create handlers

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def handler-type (some-fn :type type))

(defmulti handler-impl handler-type)

(defmulti wrapper-impl handler-type)

(defn wrap-handler
  [handler with]
  (reduce (fn [next-handler wrapper]
            ((types/as-wrapper wrapper) (types/as-handler next-handler)))
          (types/as-handler handler)
          (reverse with)))

(extend-protocol types/AsHandler IPersistentMap
  (as-handler
    [m]
    (types/as-handler (handler-impl m))))

(extend-protocol types/AsHandler Sequential
  (as-handler
    [xs]
    (when-let [xs (seq xs)]
      (wrap-handler (last xs) (butlast xs)))))

(extend-protocol types/AsHandlerWrapper IPersistentMap
  (as-wrapper
    [m]
    (types/as-wrapper (wrapper-impl m))))

(defn declare-type
  [t {as-handler-fn :as-handler, as-wrapper-fn :as-wrapper, alias :type-alias}]
  (assert (or (fn? as-handler-fn) (fn? as-wrapper-fn)))
  (letfn [(handler-impl-method [obj] (reify types/AsHandler
                                       (as-handler [_] (as-handler-fn obj))))
          (wrapper-impl-method [obj] (reify types/AsHandlerWrapper
                                       (as-wrapper [_] (as-wrapper-fn obj))))]
    (when as-handler-fn
      (.addMethod ^MultiFn handler-impl t handler-impl-method)
      (when alias
        (.addMethod ^MultiFn handler-impl alias handler-impl-method)))
    (when as-wrapper-fn
      (.addMethod ^MultiFn wrapper-impl t wrapper-impl-method)
      (when alias
        (.addMethod ^MultiFn wrapper-impl alias wrapper-impl-method)))))

(defn as-wrapper-2-arity
  [f]
  (fn [opts] (fn [handler] (f handler opts))))

(defn as-wrapper-1-arity
  [f]
  (fn [_] f))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn force-dispatch
  "A HttpHandler that dispatches request if it is running in the io thread."
  [handler]
  (let [handler (types/as-handler handler)]
    (reify HttpHandler
      (handleRequest [_ exchange]
        (if (.isInIoThread exchange)
          (-> exchange (.dispatch handler))
          (-> handler (.handleRequest exchange)))))))

(declare-type force-dispatch {:type-alias ::force-dispatch
                              :as-wrapper (as-wrapper-1-arity force-dispatch)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn path
  "Creates a new path handler, with optional default handler. Handler that
  dispatches to a given handler based of a prefix match of the path. This only
  matches a single level of a request, e.g. if you have a request that takes the
  form: `/foo/bar`.

  Options:

  **`prefix`** (map) The map of path prefixes and their handlers.

  If the path does not start with a `/` then one will be prepended. The match is
  done on a prefix bases, so registering `/foo` will also match `/foo/bar`.
  Though exact path matches are taken into account before prefix path matches.
  So if an exact path match exists its handler will be triggered. If `/` is
  specified as the path then it will replace the default handler.

  **`exact`** (map) The map of exact paths and their handlers.

  If the request path is exactly equal to the given path, run the handler. Exact
  paths are prioritized higher than prefix paths.

  **`cache-size`** (int) The cache size, unlimited by default.

  Example:

      (handler/path {:prefix {\"static\" (handler/resource {...})}
                     :exact {\"ws\" (handler/websocket {...})}})
  "
  (^HttpHandler
   [opts] (path nil opts))
  (^HttpHandler
   [default-handler {:keys [prefix exact cache-size]}]
   (letfn [(add-prefix-path [this [path handler]]
             (.addPrefixPath ^PathHandler this path (types/as-handler handler)))
           (add-exact-path [this [path handler]]
             (.addExactPath ^PathHandler this path (types/as-handler handler)))]
     (as->
       (cond (and default-handler
                  cache-size) (PathHandler. (types/as-handler default-handler) (int cache-size))
             default-handler, (PathHandler. (types/as-handler default-handler))
             cache-size,,,,,, (PathHandler. (int cache-size))
             :else,,,,,,,,,,, (PathHandler.))
       handler
       (reduce add-prefix-path handler prefix)
       (reduce add-exact-path handler exact)))))

(declare-type path {:type-alias ::path
                    :as-handler path
                    :as-wrapper (as-wrapper-2-arity path)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn virtual-host
  (^NameVirtualHostHandler
   [{:keys [hosts]}]
   (reduce (fn [this [host handler]]
             (.addHost ^NameVirtualHostHandler this host (types/as-handler handler)))
           (NameVirtualHostHandler.)
           hosts))
  (^NameVirtualHostHandler
   [default-handler opts]
   (-> (virtual-host opts)
       (.setDefaultHandler (types/as-handler default-handler)))))

(declare-type virtual-host {:type-alias ::virtual-host
                            :as-handler virtual-host
                            :as-wrapper (as-wrapper-2-arity virtual-host)})

(comment
  (types/as-handler {:type virtual-host :hosts {"localhost" identity}})
  (types/as-handler {:type ::virtual-host :hosts {"localhost" identity}})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- resource-manager
  ^ResourceManager
  [{:keys [prefix] :or {prefix "public"}}]
  (ClassPathResourceManager. (ClassLoader/getSystemClassLoader)
                             ^String prefix))

(defn resource-files
  (^ResourceHandler
   [opts]
   (ResourceHandler. (resource-manager opts)))
  (^ResourceHandler
   [next-handler opts]
   (ResourceHandler. (resource-manager opts) (types/as-handler next-handler))))

(declare-type resource-files {:type-alias ::resource-files
                              :as-handler resource-files
                              :as-wrapper (as-wrapper-2-arity resource-files)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

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

(.addMethod ^MultiFn types/as-session-manager IPersistentMap
            in-memory-session-manager)

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

(.addMethod ^MultiFn types/as-session-config IPersistentMap
            session-cookie-config)

(defn session-attachment
  ^SessionAttachmentHandler
  [next-handler {:keys [session-manager, session-config]
                 :or {session-manager {} session-config {}}}]
  (SessionAttachmentHandler. (types/as-handler next-handler)
                             (types/as-session-manager session-manager)
                             (types/as-session-config session-config)))

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
  (SimpleErrorPageHandler. (types/as-handler next-handler)))

(declare-type simple-error-page {:type-alias ::simple-error-page
                                 :as-wrapper (as-wrapper-1-arity simple-error-page)})

(comment
  (types/as-handler {:type simple-error-page})
  (types/as-handler {:type ::simple-error-page})
  ((types/as-wrapper {:type simple-error-page}) identity)
  ((types/as-wrapper {:type ::simple-error-page}) identity)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn graceful-shutdown
  ^HttpHandler
  [next-handler]
  (GracefulShutdownHandler. (types/as-handler next-handler)))

(declare-type graceful-shutdown {:type-alias ::graceful-shutdown
                                 :as-wrapper (as-wrapper-1-arity graceful-shutdown)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  ^HttpHandler
  [next-handler]
  (RequestDumpingHandler. (types/as-handler next-handler)))

(fn [next-handler _] (request-dump next-handler))

(declare-type request-dump {:type-alias ::request-dump
                            :as-wrapper (as-wrapper-1-arity request-dump)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn websocket
  {:arglists '([{:as listener :keys [on-connect, on-message, on-close, on-error]}]
               [next-handler, {:as listener :keys [on-connect, on-message, on-close, on-error]}]
               [listener]
               [next-handler, listener])}
  ^WebSocketProtocolHandshakeHandler
  ([callback]
   (websocket/handshake callback))
  ^WebSocketProtocolHandshakeHandler
  ([next-handler, callback]
   (websocket/handshake next-handler callback)))

(declare-type websocket {:type-alias ::websocket
                         :as-handler websocket
                         :as-wrapper (as-wrapper-2-arity websocket)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
