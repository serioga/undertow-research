(ns undertow.handler
  (:require [undertow.api.types :as types]
            [undertow.websocket.handler :as websocket])
  (:import (clojure.lang MultiFn Sequential)
           (io.undertow.server HttpHandler)
           (io.undertow.server.handlers GracefulShutdownHandler NameVirtualHostHandler PathHandler ProxyPeerAddressHandler RequestDumpingHandler)
           (io.undertow.server.handlers.error SimpleErrorPageHandler)
           (io.undertow.server.handlers.resource ClassPathResourceManager ResourceHandler ResourceManager)
           (io.undertow.server.session InMemorySessionManager SecureRandomSessionIdGenerator SessionAttachmentHandler SessionCookieConfig)
           (io.undertow.websockets WebSocketProtocolHandshakeHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn wrap-handler
  [handler with]
  (reduce (fn [next-handler, wrapper]
            ((types/as-wrapper wrapper) (types/as-handler next-handler)))
          (types/as-handler handler)
          (reverse with)))

(defmethod types/as-handler Sequential
  [xs]
  (when-let [xs (seq xs)]
    (wrap-handler (last xs) (butlast xs))))

(defn declare-type
  [-type {:keys [as-handler, as-wrapper, type-alias]}]
  (assert (or (fn? as-handler) (fn? as-wrapper)))
  (when as-handler
    (.addMethod ^MultiFn types/as-handler -type as-handler)
    (when type-alias
      (.addMethod ^MultiFn types/as-handler type-alias as-handler)))
  (when as-wrapper
    (.addMethod ^MultiFn types/as-wrapper -type as-wrapper)
    (when type-alias
      (.addMethod ^MultiFn types/as-wrapper type-alias as-wrapper))))

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

  Arguments:

  - `default-handler` The handler that is invoked if there are no paths matched.

  - `config` The configuration map with options:

      - `:prefix` The map of path prefixes and their handlers.
          + If the path does not start with a `/` then one will be prepended.
          + The match is done on a prefix bases, so registering `/foo` will also
            match `/foo/bar`. Though exact path matches are taken into account
            before prefix path matches. So if an exact path match exists its handler
            will be triggered.
          + If `/` is specified as the path then it will replace the default handler.

      - `:exact` The map of exact paths and their handlers.
          + If the request path is exactly equal to the given path, run the handler.
          + Exact paths are prioritized higher than prefix paths.

      - `:cache-size` The cache size, unlimited by default, integer.

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
                  cache-size),, (PathHandler. (types/as-handler default-handler) (int cache-size))
             default-handler,,, (PathHandler. (types/as-handler default-handler))
             cache-size,,,,,,,, (PathHandler. (int cache-size))
             :else,,,,,,,,,,,,, (PathHandler.))
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

  Arguments:

  - `default-handler` The handler that is invoked if there are no hostnames
                      matched.

  - `config` The configuration map with options:

      - `:host` The map of hostnames and their handlers.

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

  Function arguments:

  - `next-handler` The handler that is invoked if there are no web socket
                   headers.

  - `callback` The instance of the `WebSocketConnectionCallback` or callback
               configuration map.

    Callback configuration options:

      - `:on-connect` The function `(fn [{:keys [callback exchange channel]}])`.
          + Is called once the WebSocket connection is established, which means
            the handshake was successful.

      - `:on-message` The function `(fn [{:keys [callback channel text data]}])`.
          + Is called when listener receives a message.
          + The text message is provided in `:text` and binary message is
            provided in `:data`.

      - `:on-close` The function `(fn [{:keys [callback channel code reason]}])`.
          + Is called once the WebSocket connection is closed.
          + The `:code` is status code to close messages:
            http://tools.ietf.org/html/rfc6455#section-7.4

      - `:on-error` The function `(fn [{:keys [callback channel error]}])`.
          + Is called on WebSocket connection error.
          + Default implementation just closes WebSocket connection.
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

(defmethod types/as-resource-manager :classpath-files
  [{:keys [prefix]}]
  (let [prefix (or prefix "public")
        res-man (ClassPathResourceManager. (ClassLoader/getSystemClassLoader) ^String prefix)]
    (reify ResourceManager
      (getResource [_ path]
        (let [resource (.getResource res-man path)]
          (when-not (some-> resource (.isDirectory))
            resource)))
      (isResourceChangeListenerSupported [_] false))))

(defn resource
  "Returns a new resource handler with optional next handler that is called if
  no resource is found.

  Function arguments:

  - `next-handler` The handler that is called if no resource is found.

  - `resource-manager` The instance of `ResourceManager` or resource manager
                       configuration map.

    Resource manager configuration options:

      - `:resource-manager` The type of configuration manager, keyword.
          + Used as `:type` in configuration passed to [[api.types/as-resource-manager]].

    Configuration options of `:classpath-files` resource manager:

      - `:prefix` The prefix that is appended to resources that are to be
                  loaded, string.
          + Default prefix is \"public\".

  Example:

      (handler/resource {:resource-manager :classpath-files
                         :prefix \"public/static\"})
  "
  (^ResourceHandler
   [resource-manager]
   (ResourceHandler. (types/as-resource-manager resource-manager)))
  (^ResourceHandler
   [next-handler, resource-manager]
   (ResourceHandler. (types/as-resource-manager resource-manager) (types/as-handler next-handler))))

(defmethod types/as-resource-manager resource
  [{:keys [resource-manager] :as config}]
  (types/as-resource-manager (assoc config :type resource-manager)))

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

(.addMethod ^MultiFn types/as-session-manager :default
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

(.addMethod ^MultiFn types/as-session-config :default
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
