(ns undertow.types
  (:import (clojure.lang Fn IPersistentMap MultiFn)
           (io.undertow.server HttpHandler)
           (io.undertow.websockets WebSocketConnectionCallback)
           (io.undertow.websockets.core WebSocketCallback)
           (org.xnio ChannelListener)
           (undertow.websocket WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: protocol method docstrings

(defprotocol AsHandler
  (as-handler ^io.undertow.server.HttpHandler [obj]))

(extend-protocol AsHandler HttpHandler
  (as-handler
    [handler] handler))

(defprotocol AsHandlerWrapper
  (as-wrapper [obj]))

(extend-protocol AsHandlerWrapper
  Fn
  (as-wrapper
    [wrapper-fn] wrapper-fn)
  MultiFn
  (as-wrapper
    [wrapper-fn] wrapper-fn))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsWebSocketListener
  (as-websocket-listener ^org.xnio.ChannelListener [obj]))

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
  (as-websocket-connection-callback ^WebSocketConnectionCallback [obj]))

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
  (as-websocket-callback ^WebSocketCallback [obj]))

(extend-protocol AsWebSocketCallback
  WebSocketCallback
  (as-websocket-callback [listener] listener)
  nil
  (as-websocket-callback [_] nil))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
