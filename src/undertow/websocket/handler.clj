(ns undertow.websocket.handler
  (:require [undertow.api.types :as types])
  (:import (clojure.lang IPersistentMap)
           (io.undertow.websockets WebSocketConnectionCallback WebSocketProtocolHandshakeHandler)
           (io.undertow.websockets.core WebSocketChannel)
           (io.undertow.websockets.spi WebSocketHttpExchange)
           (org.xnio ChannelListener)
           (undertow.websocket WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(extend-protocol types/AsWebSocketListener IPersistentMap
  (as-websocket-listener
    [config]
    (WebSocketChannelListener. config)))

(extend-protocol types/AsWebSocketConnectionCallback ChannelListener
  (as-websocket-connection-callback
    [listener]
    (reify WebSocketConnectionCallback
      (^void onConnect
        [_, ^WebSocketHttpExchange exchange, ^WebSocketChannel chan]
        (when (instance? WebSocketConnectionCallback listener)
          (.onConnect ^WebSocketConnectionCallback listener exchange chan))
        (.set (.getReceiveSetter chan) listener)
        (.resumeReceives chan)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handshake
  {:arglists '([{:as callback :keys [on-connect, on-message, on-close, on-error]}]
               [next-handler, {:as callback :keys [on-connect, on-message, on-close, on-error]}]
               [callback]
               [next-handler, callback])}
  (^WebSocketProtocolHandshakeHandler
   [callback]
   (WebSocketProtocolHandshakeHandler. (types/as-websocket-connection-callback callback)))
  (^WebSocketProtocolHandshakeHandler
   [next-handler, callback]
   (WebSocketProtocolHandshakeHandler. (types/as-websocket-connection-callback callback)
                                       (types/as-handler next-handler))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
