(ns undertow.websocket
  (:require [undertow.handler :as handler])
  (:import (io.undertow.websockets WebSocketConnectionCallback WebSocketProtocolHandshakeHandler)
           (io.undertow.websockets.core WebSocketCallback WebSocketChannel WebSockets)
           (io.undertow.websockets.spi WebSocketHttpExchange)
           (undertow.websocket OnOpenListener WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn channel-listener
  {:arglists '([{:keys [on-open, on-message, on-close, on-error, context]}])}
  [config]
  (WebSocketChannelListener. config))

(defn websocket-callback
  {:arglists '([{:keys [on-complete, on-error, context]}])
   :tag WebSocketCallback}
  [handlers]
  (reify WebSocketCallback
    (complete
      [_ channel context]
      (when-let [on-complete (:on-complete handlers)]
        (on-complete {:channel channel, :context (:context handlers context)})))
    (onError
      [_ channel context throwable]
      (when-let [on-error (:on-error handlers)]
        (on-error {:channel channel, :context (:context handlers context), :error throwable})))))

(defn connection-callback
  {:arglists '([{:keys [on-open, on-message, on-close, on-error, context]}]
               [{:keys [listener, context]}])
   :tag WebSocketConnectionCallback}
  [{:keys [listener context] :as config}]
  (reify WebSocketConnectionCallback
    (^void onConnect
      [_, ^WebSocketHttpExchange exchange, ^WebSocketChannel channel]
      (let [listener (or listener (channel-listener (cond-> config (not context)
                                                                   (assoc :context exchange))))]
        (when (instance? OnOpenListener listener)
          (.onOpen ^OnOpenListener listener channel (or context exchange)))
        (.set (.getReceiveSetter channel) listener))
      (.resumeReceives channel))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handler
  {:arglists '([{:as config :keys [on-open, on-message, on-close, on-error, context]}]
               [{:as config :keys [listener, context]}]
               [next-handler, config])}
  (^WebSocketProtocolHandshakeHandler
   [config]
   (WebSocketProtocolHandshakeHandler. (connection-callback config)))
  (^WebSocketProtocolHandshakeHandler
   [next-handler, config]
   (WebSocketProtocolHandshakeHandler. (connection-callback config)
                                       (handler/as-handler next-handler))))

(handler/declare-type handler {:type-alias ::handler
                               :as-handler handler
                               :as-wrapper (handler/as-wrapper-2-arity handler)})

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: docstrings
(defprotocol WebSocketSend
  (send-text
    [message channel opts]
    "message channel {:keys [on-complete, on-error, context, timeout]}")
  (send-text-blocking [message channel])
  (send-binary [message channel opts])
  (send-binary-blocking [message channel])
  (send-close [message channel opts])
  (send-close-blocking [message channel]))

(extend-protocol WebSocketSend String
  (send-text
    ;; TODO: Add :callback option
    [message channel opts]
    (WebSockets/sendText message, ^WebSocketChannel channel
                         (websocket-callback opts) ^long (:timeout opts -1)))
  (send-text-blocking
    [message channel]
    (WebSockets/sendTextBlocking message, ^WebSocketChannel channel)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
