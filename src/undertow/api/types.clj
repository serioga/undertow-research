(ns undertow.api.types
  (:import (clojure.lang Fn IPersistentMap MultiFn)
           (io.undertow Undertow$ListenerBuilder)
           (io.undertow.server HttpHandler)
           (io.undertow.server.session SessionConfig SessionManager)
           (io.undertow.websockets WebSocketConnectionCallback)
           (io.undertow.websockets.core WebSocketCallback)
           (org.xnio ChannelListener Option OptionMap)
           (undertow.websocket WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsHandler
  (as-handler
    ^io.undertow.server.HttpHandler [obj]
    "Coerces `obj` to the instance of `io.undertow.server.HttpHandler`."))

(extend-protocol AsHandler HttpHandler
  (as-handler
    [handler] handler))

(defprotocol AsHandlerWrapper
  (as-wrapper
    [obj]
    "Coerces `obj` to the 1-arity function which wraps handler and returns new
    handler."))

(extend-protocol AsHandlerWrapper
  Fn
  (as-wrapper
    [wrapper-fn] wrapper-fn)
  MultiFn
  (as-wrapper
    [wrapper-fn] wrapper-fn))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsListenerBuilder
  (as-listener-builder
    ^io.undertow.Undertow$ListenerBuilder [obj]
    "Coerces `obj` to the instance of `io.undertow.Undertow$ListenerBuilder`."))

(extend-protocol AsListenerBuilder
  Undertow$ListenerBuilder
  (as-listener-builder
    [builder] builder))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti as-option (fn [k _] k))

(defn define-option
  ([alias option] (define-option alias option identity))
  ([alias option coerce-fn]
   (defmethod as-option alias [_ v] [option (coerce-fn v)])))

(defmethod as-option :default
  [option value]
  (if (instance? Option option)
    [option value]
    (throw (ex-info (str "Unknown undertow option: " option "\n"
                         "Use `define-option` to define new options.") {}))))

(defn as-option-map
  ^OptionMap
  [m]
  (if (seq m)
    (-> (OptionMap/builder)
        (.add (->> m (into {} (map (fn [[k v]] (as-option k v))))))
        (.getMap))
    OptionMap/EMPTY))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmulti as-session-manager (some-fn :type type))
(.addMethod ^MultiFn as-session-manager SessionManager identity)

(defmulti as-session-config (some-fn :type type))
(.addMethod ^MultiFn as-session-config SessionConfig identity)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsWebSocketListener
  (as-websocket-listener
    ^org.xnio.ChannelListener [obj]
    "Coerces `obj` to the instance of `org.xnio.ChannelListener`."))

(extend-protocol AsWebSocketListener
  ChannelListener
  (as-websocket-listener
    [listener] listener)
  nil
  (as-websocket-listener
    [_] nil)
  IPersistentMap
  (as-websocket-listener
    [config]
    (WebSocketChannelListener. config)))

(defprotocol AsWebSocketConnectionCallback
  (as-websocket-connection-callback
    ^WebSocketConnectionCallback [obj]
    "Coerces `obj` to the instance of
    `io.undertow.websockets.WebSocketConnectionCallback`."))

(extend-protocol AsWebSocketConnectionCallback
  WebSocketConnectionCallback
  (as-websocket-connection-callback
    [callback] callback)
  nil
  (as-websocket-connection-callback
    [_] nil)
  Object
  (as-websocket-connection-callback
    [obj]
    (as-websocket-connection-callback (as-websocket-listener obj))))

(defprotocol AsWebSocketCallback
  (as-websocket-callback
    ^WebSocketCallback [obj]
    "Coerces `obj` to the instance of
    `io.undertow.websockets.core.WebSocketConnectionCallback`."))

(extend-protocol AsWebSocketCallback
  WebSocketCallback
  (as-websocket-callback [listener] listener)
  nil
  (as-websocket-callback [_] nil))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
