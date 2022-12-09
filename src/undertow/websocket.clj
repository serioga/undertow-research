(ns undertow.websocket
  (:require [undertow.handler :as handler])
  (:import (clojure.lang IFn IPersistentMap)
           (io.undertow.websockets WebSocketConnectionCallback WebSocketProtocolHandshakeHandler)
           (io.undertow.websockets.core CloseMessage WebSocketCallback WebSocketChannel WebSockets)
           (io.undertow.websockets.spi WebSocketHttpExchange)
           (java.nio ByteBuffer)
           (org.xnio ChannelListener)
           (undertow.websocket OnOpenListener WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsChannelListener
  (as-channel-listener ^ChannelListener [obj]))

(extend-protocol AsChannelListener
  ChannelListener
  (as-channel-listener
    [listener] listener)
  nil
  (as-channel-listener
    [_] nil)
  IPersistentMap
  (as-channel-listener
    [config]
    (WebSocketChannelListener. config)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:const exchange-attr "websocket/exchange")

(defprotocol AsConnectionCallback
  (as-connection-callback ^WebSocketConnectionCallback [obj]))

(extend-protocol AsConnectionCallback
  WebSocketConnectionCallback
  (as-connection-callback
    [callback] callback)
  nil
  (as-connection-callback
    [_] nil)
  ChannelListener
  (as-connection-callback
    [listener]
    (reify WebSocketConnectionCallback
      (^void onConnect
        [_, ^WebSocketHttpExchange exchange, ^WebSocketChannel chan]
        (.setAttribute chan exchange-attr exchange)
        (when (instance? OnOpenListener listener)
          (.onOpen ^OnOpenListener listener chan))
        (.set (.getReceiveSetter chan) listener)
        (.resumeReceives chan))))
  Object
  (as-connection-callback
    [obj]
    (as-connection-callback (as-channel-listener obj))))

(defn get-exchange
  ^WebSocketHttpExchange
  [^WebSocketChannel channel]
  (.getAttribute channel exchange-attr))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handler
  {:arglists '([{:as listener :keys [on-open, on-message, on-close, on-error]}]
               [next-handler, {:as listener :keys [on-open, on-message, on-close, on-error]}]
               [^ChannelListener listener]
               [next-handler, ^ChannelListener listener])}
  (^WebSocketProtocolHandshakeHandler
   [callback]
   (WebSocketProtocolHandshakeHandler. (as-connection-callback callback)))
  (^WebSocketProtocolHandshakeHandler
   [next-handler, callback]
   (WebSocketProtocolHandshakeHandler. (as-connection-callback callback)
                                       (handler/as-handler next-handler))))

(handler/declare-type handler {:type-alias ::handler
                               :as-handler handler
                               :as-wrapper (handler/as-wrapper-2-arity handler)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol AsWebSocketCallback
  ;; TODO: docstring
  (as-websocket-callback ^WebSocketCallback [obj]))

(extend-protocol AsWebSocketCallback
  WebSocketCallback
  (as-websocket-callback [listener] listener)
  nil
  (as-websocket-callback [_] nil)
  IPersistentMap
  (as-websocket-callback [handlers]
    (reify WebSocketCallback
      (complete
        [_ channel _]
        (when-let [on-complete (:on-complete handlers)]
          (on-complete {:channel channel :callback :on-complete})))
      (onError
        [_ channel _ throwable]
        (when-let [on-error (:on-error handlers)]
          (on-error {:channel channel :error throwable :callback :on-error})))))
  IFn
  (as-websocket-callback [callback-fn]
    (reify WebSocketCallback
      (complete
        [_ channel _]
        (callback-fn {:channel channel :callback :on-complete}))
      (onError
        [_ channel _ throwable]
        (callback-fn {:channel channel :error throwable :callback :on-error})))))


;; TODO: docstrings

(defprotocol WebSocketSendText
  (send-text
    [text chan opts]
    #_{:arglists '([message channel {:keys [callback timeout]}]
                   [message channel {{:keys [on-complete on-error]} :callback, timeout :timeout}])}
    "message channel {:keys [on-complete, on-error, timeout]}")
  (send-text!!
    [text chan]))

(defprotocol WebSocketSendBinary
  (send-binary
    [data chan opts])
  (send-binary!!
    [data chan]))

(extend-protocol WebSocketSendText String
  (send-text
    [text chan opts]
    (WebSockets/sendText text, ^WebSocketChannel chan
                         (some-> opts :callback as-websocket-callback)
                         ^long (:timeout opts -1)))
  (send-text!!
    [text chan]
    (WebSockets/sendTextBlocking text, ^WebSocketChannel chan)))

(extend-protocol WebSocketSendBinary ByteBuffer
  (send-binary
    [data chan opts]
    (WebSockets/sendBinary data, ^WebSocketChannel chan
                           (some-> opts :callback as-websocket-callback)
                           ^long (:timeout opts -1)))
  (send-binary!!
    [data chan]
    (WebSockets/sendBinaryBlocking data, ^WebSocketChannel chan)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn normal-closure
  [] (CloseMessage. CloseMessage/NORMAL_CLOSURE ""))

(defn going-away
  ([] (CloseMessage. CloseMessage/GOING_AWAY ""))
  ([reason] (CloseMessage. CloseMessage/GOING_AWAY reason)))

(defn wrong-code
  ([] (CloseMessage. CloseMessage/WRONG_CODE ""))
  ([reason] (CloseMessage. CloseMessage/WRONG_CODE reason)))

(defn protocol-error
  ([] (CloseMessage. CloseMessage/PROTOCOL_ERROR ""))
  ([reason] (CloseMessage. CloseMessage/PROTOCOL_ERROR reason)))

(defn msg-contains-invalid-data
  ([] (CloseMessage. CloseMessage/MSG_CONTAINS_INVALID_DATA ""))
  ([reason] (CloseMessage. CloseMessage/MSG_CONTAINS_INVALID_DATA reason)))

(defn msg-violates-policy
  ([] (CloseMessage. CloseMessage/MSG_VIOLATES_POLICY ""))
  ([reason] (CloseMessage. CloseMessage/MSG_VIOLATES_POLICY reason)))

(defn msg-too-big
  ([] (CloseMessage. CloseMessage/MSG_TOO_BIG ""))
  ([reason] (CloseMessage. CloseMessage/MSG_TOO_BIG reason)))

(defn missing-extensions
  ([] (CloseMessage. CloseMessage/MISSING_EXTENSIONS ""))
  ([reason] (CloseMessage. CloseMessage/MISSING_EXTENSIONS reason)))

(defn unexpected-error
  ([] (CloseMessage. CloseMessage/UNEXPECTED_ERROR ""))
  ([reason] (CloseMessage. CloseMessage/UNEXPECTED_ERROR reason)))

(defprotocol WebSocketSendClose
  (send-close
    [message chan opts])
  (send-close!!
    [message chan]))

(extend-protocol WebSocketSendClose CloseMessage
  (send-close
    [message chan opts]
    (WebSockets/sendClose message, ^WebSocketChannel chan
                          (some-> opts :callback as-websocket-callback)))
  (send-close!!
    [message chan]
    (WebSockets/sendCloseBlocking message, ^WebSocketChannel chan)))

(extend-protocol WebSocketSendClose nil
  (send-close
    [_ chan opts]
    (send-close (normal-closure) chan opts))
  (send-close!!
    [_ chan]
    (send-close!! (normal-closure) chan)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
