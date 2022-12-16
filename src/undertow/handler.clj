(ns undertow.handler
  (:require [undertow.api.types :as types]
            [undertow.websocket.handler :as websocket])
  (:import (clojure.lang IPersistentMap MultiFn Sequential)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionCookieConfig)
           (io.undertow.websockets WebSocketProtocolHandshakeHandler)))

(set! *warn-on-reflection* true)

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

(defn as-arity-2-wrapper
  [f]
  (fn [opts] (fn [handler] (f handler opts))))

(defn as-arity-1-wrapper
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
                              :as-wrapper (as-arity-1-wrapper force-dispatch)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn path
  "Returns a new path handler, with optional default handler. A `HttpHandler`
  that dispatches to a given handler based of a prefix match of the path. This
  only matches a single level of a request, e.g. if you have a request that
  takes the form: `/foo/bar`.

  Configuration options:

  **`:prefix`** The map of path prefixes and their handlers.

  - If the path does not start with a `/` then one will be prepended.
  - The match is done on a prefix bases, so registering `/foo` will also match
    `/foo/bar`. Though exact path matches are taken into account before prefix
    path matches. So if an exact path match exists its handler will be
    triggered.
  - If `/` is specified as the path then it will replace the default handler.

  **`:exact`** The map of exact paths and their handlers.

  - If the request path is exactly equal to the given path, run the handler.
  - Exact paths are prioritized higher than prefix paths.

  **`:cache-size`** (int) The cache size, unlimited by default.

  Example:

      (handler/path {:prefix {\"static\" (handler/resource {...})}
                     :exact {\"ws\" (handler/websocket {...})}})
  "
  (^PathHandler
   [config] (path nil config))
  (^PathHandler
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
                    :as-wrapper (as-arity-2-wrapper path)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn virtual-host
  "Returns a new virtual host handler, with optional default handler.
  A `HttpHandler` that implements virtual hosts based on the `Host:` http
  header.

  Configuration options:

  **`:host`** The map of hostnames and their handlers.

  Example:

      (handler/virtual-host {:host {\"static.localhost\" (handler/resource {...})
                                    \"ws.localhost\" (handler/websocket {...})})
  "
  (^NameVirtualHostHandler
   [{:keys [host]}]
   (reduce (fn [this [host handler]]
             (.addHost ^NameVirtualHostHandler this host (types/as-handler handler)))
           (NameVirtualHostHandler.)
           host))
  (^NameVirtualHostHandler
   [default-handler, config]
   (-> (virtual-host config)
       (.setDefaultHandler (types/as-handler default-handler)))))

(declare-type virtual-host {:type-alias ::virtual-host
                            :as-handler virtual-host
                            :as-wrapper (as-arity-2-wrapper virtual-host)})

(comment
  (types/as-handler {:type virtual-host :host {"localhost" identity}})
  (types/as-handler {:type ::virtual-host :host {"localhost" identity}})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn websocket
  "Returns a new web socket session handler with optional next handler to invoke
  if the web socket connection fails. A `HttpHandler` which will process the
  `HttpServerExchange` and do the actual handshake/upgrade to WebSocket.

  **`callback`** The instance of the `WebSocketConnectionCallback` or callback
                 configuration map.

  Callback configuration options:

  **`:on-connect`** `(fn [{:keys [callback exchange channel]}])`

  - Is called once the WebSocket connection is established, which means the
    handshake was successful.

  **`:on-message`** `(fn [{:keys [callback channel text data]}])`

  - Is called when listener receives a message.
  - The text message is provided in `:text` and binary message is provided in
    `:data`.

  **`:on-close`** `(fn [{:keys [callback channel code reason]}])`

  - Is called once the WebSocket connection is closed.
  - The `:code` is status code to close messages: http://tools.ietf.org/html/rfc6455#section-7.4

  **`:on-error`** `(fn [{:keys [callback channel error]}])`

  - Is called on WebSocket connection error.
  - Default implementation just closes WebSocket connection.
  "
  {:arglists '([{:as callback :keys [on-connect, on-message, on-close, on-error]}]
               [next-handler, {:as callback :keys [on-connect, on-message, on-close, on-error]}]
               [callback]
               [next-handler, callback])}
  ^WebSocketProtocolHandshakeHandler
  ([callback]
   (websocket/handshake callback))
  ^WebSocketProtocolHandshakeHandler
  ([next-handler, callback]
   (websocket/handshake next-handler callback)))

(declare-type websocket {:type-alias ::websocket
                         :as-handler websocket
                         :as-wrapper (as-arity-2-wrapper websocket)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Support non-classpath resource manager

(extend-protocol types/AsResourceManager IPersistentMap
  (as-resource-manager
    [{:keys [prefix]}]
    (let [prefix (or prefix "public")]
      (ClassPathResourceManager. (ClassLoader/getSystemClassLoader) ^String prefix))))

(defn resource
  "Returns a new resource handler with optional next handler that is called if
  no resource is found.

  **`resource-manager`**

  - The instance of `io.undertow.server.handlers.resource.ResourceManager`
  - or configuration map for `ClassPathResourceManager`.

  Resource manager configuration options:

  **`:prefix`** (string)

  - The prefix that is appended to resources that are to be loaded.
  - Default prefix is \"public\".
  "
  (^ResourceHandler
   [resource-manager]
   (ResourceHandler. (types/as-resource-manager resource-manager)))
  (^ResourceHandler
   [next-handler, resource-manager]
   (ResourceHandler. (types/as-resource-manager resource-manager) (types/as-handler next-handler))))

(declare-type resource {:type-alias ::resource
                        :as-handler resource
                        :as-wrapper (as-arity-2-wrapper resource)})

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
                                  :as-wrapper (as-arity-2-wrapper session-attachment)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn proxy-peer-address
  "Returns a new handler that sets the peer address based on the
  `X-Forwarded-For` and `X-Forwarded-Proto` headers.

  This should only be used behind a proxy that always sets this header,
  otherwise it is possible for an attacker to forge their peer address."
  ^ProxyPeerAddressHandler
  [next-handler]
  (ProxyPeerAddressHandler. next-handler))

(declare-type proxy-peer-address {:type-alias ::proxy-peer-address
                                  :as-wrapper (as-arity-1-wrapper proxy-peer-address)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-error-page
  "Returns a handler that generates an extremely simple no frills error page."
  ^SimpleErrorPageHandler
  [next-handler]
  (SimpleErrorPageHandler. (types/as-handler next-handler)))

(declare-type simple-error-page {:type-alias ::simple-error-page
                                 :as-wrapper (as-arity-1-wrapper simple-error-page)})

(comment
  (types/as-handler {:type simple-error-page})
  (types/as-handler {:type ::simple-error-page})
  ((types/as-wrapper {:type simple-error-page}) identity)
  ((types/as-wrapper {:type ::simple-error-page}) identity)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn graceful-shutdown
  "Returns a new handler that can be used to wait for all requests to finish
  before shutting down the server gracefully."
  ^GracefulShutdownHandler
  [next-handler]
  (GracefulShutdownHandler. (types/as-handler next-handler)))

(declare-type graceful-shutdown {:type-alias ::graceful-shutdown
                                 :as-wrapper (as-arity-1-wrapper graceful-shutdown)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn request-dump
  "Returns a handler that dumps requests to the log for debugging purposes."
  ^RequestDumpingHandler
  [next-handler]
  (RequestDumpingHandler. (types/as-handler next-handler)))

(declare-type request-dump {:type-alias ::request-dump
                            :as-wrapper (as-arity-1-wrapper request-dump)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
