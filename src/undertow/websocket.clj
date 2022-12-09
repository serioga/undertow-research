(ns undertow.websocket
  (:require [undertow.handler :as handler])
  (:import (clojure.lang IFn IPersistentMap)
           (io.undertow.websockets WebSocketConnectionCallback WebSocketProtocolHandshakeHandler)
           (io.undertow.websockets.core WebSocketCallback WebSocketChannel WebSockets)
           (io.undertow.websockets.spi WebSocketHttpExchange)
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
        [_, ^WebSocketHttpExchange exchange, ^WebSocketChannel channel]
        (.setAttribute channel exchange-attr exchange)
        (when (instance? OnOpenListener listener)
          (.onOpen ^OnOpenListener listener channel))
        (.set (.getReceiveSetter channel) listener)
        (.resumeReceives channel))))
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
(defprotocol WebSocketSend
  (send-text
    [text channel opts]
    #_{:arglists '([message channel {:keys [callback timeout]}]
                   [message channel {{:keys [on-complete on-error]} :callback, timeout :timeout}])}
    "message channel {:keys [on-complete, on-error, timeout]}")
  (send-text-blocking [text channel])
  (send-binary [data channel opts])
  (send-binary-blocking [data channel])
  (send-close [message channel opts])
  (send-close-blocking [message channel]))

(extend-protocol WebSocketSend String
  (send-text
    ;; TODO: Add :callback option
    [text channel opts]
    (WebSockets/sendText text, ^WebSocketChannel channel
                         (some-> opts :callback as-websocket-callback)
                         ^long (:timeout opts -1)))
  (send-text-blocking
    [message channel]
    (WebSockets/sendTextBlocking message, ^WebSocketChannel channel)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
