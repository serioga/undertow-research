(ns undertow.websocket
  (:import (io.undertow.websockets.core WebSocketCallback WebSocketChannel WebSockets)
           (undertow WebSocketChannelListener)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn channel-listener
  {:arglists '([{:keys [on-message, on-close, on-error]}])}
  [handlers]
  (WebSocketChannelListener. handlers))

(defn websocket-callback
  {:arglists '([{:keys [on-complete, on-error, context]}])}
  ^WebSocketCallback
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
    [message channel opts]
    (WebSockets/sendText message, ^WebSocketChannel channel
                         (websocket-callback opts) ^long (:timeout opts -1)))
  (send-text-blocking
    [message channel]
    (WebSockets/sendTextBlocking message, ^WebSocketChannel channel)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
