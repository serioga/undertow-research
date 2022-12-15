(ns undertow.websocket.channel
  (:require [undertow.api.types :as types])
  (:import (clojure.lang IFn IPersistentMap)
           (io.undertow.websockets.core CloseMessage WebSocketCallback WebSocketChannel WebSockets)
           (java.nio ByteBuffer)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(extend-protocol types/AsWebSocketCallback IPersistentMap
  (as-websocket-callback
    [handlers]
    (reify WebSocketCallback
      (complete
        [_ channel _]
        (when-let [on-complete (:on-complete handlers)]
          (on-complete {:callback :on-complete :channel channel})))
      (onError
        [_ channel _ throwable]
        (when-let [on-error (:on-error handlers)]
          (on-error {:callback :on-error :channel channel :error throwable}))))))

(extend-protocol types/AsWebSocketCallback IFn
  (as-websocket-callback
    [callback-fn]
    (reify WebSocketCallback
      (complete
        [_ channel _]
        (callback-fn {:callback :on-complete :channel channel}))
      (onError
        [_ channel _ throwable]
        (callback-fn {:callback :on-error :channel channel :error throwable})))))

(defprotocol WebSocketSendText
  (send-text
    [text chan opts]
    #_{:arglists '([text chan {:keys [callback timeout]}]
                   [text chan {{:keys [on-complete on-error]} :callback, timeout :timeout}])}
    "Sends a complete text message, invoking the callback when complete.

    **`text`** (string) The text to send.

    **`chan`** (WebSocketChannel) The web socket channel.

    `opts` (map) The options:

    **`:callback`** (WebSocketCallback) The callback to invoke on completion.

    The callback can be a map with keys:

    - `:on-complete` The callback function `(fn [{:keys [callback, channel]}] ...)`.
    - `:on-error` The callback function `(fn [{:keys [callback, channel, error]}] ...)`

    The callback can be also just a function which receives a map with keys
    described above.

    **`:timeout`** (long) The timeout in milliseconds. No timeout by default.
    ")
  (send-text!!
    [text chan]
    "Sends a complete text message using blocking IO.

    **`text`** (string) The text to send.

    **`chan`** (WebSocketChannel) The web socket channel."))

(defprotocol WebSocketSendBinary
  (send-binary
    [data chan opts]
    #_{:arglists '([data chan {:keys [callback timeout]}]
                   [data chan {{:keys [on-complete on-error]} :callback, timeout :timeout}])}
    "Sends a complete binary message, invoking the callback when complete.

    **`data`** (string) The data to send.

    **`chan`** (WebSocketChannel) The web socket channel.

    `opts` (map) The options:

    **`:callback`** (WebSocketCallback) The callback to invoke on completion.

    The callback can be a map with keys:

    - `:on-complete` The callback function `(fn [{:keys [callback, channel]}] ...)`.
    - `:on-error` The callback function `(fn [{:keys [callback, channel, error]}] ...)`

    The callback can be also just a function which receives a map with keys
    described above.

    **`:timeout`** (long) The timeout in milliseconds. No timeout by default.
    ")
  (send-binary!!
    [data chan]
    "Sends a complete binary message using blocking IO.

    **`data`** (string) The data to send.

    **`chan`** (WebSocketChannel) The web socket channel."))

(extend-protocol WebSocketSendText String
  (send-text
    [text chan opts]
    (WebSockets/sendText text, ^WebSocketChannel chan
                         (some-> opts :callback types/as-websocket-callback)
                         ^long (:timeout opts -1)))
  (send-text!!
    [text chan]
    (WebSockets/sendTextBlocking text, ^WebSocketChannel chan)))

(extend-protocol WebSocketSendBinary ByteBuffer
  (send-binary
    [data chan opts]
    (WebSockets/sendBinary data, ^WebSocketChannel chan
                           (some-> opts :callback types/as-websocket-callback)
                           ^long (:timeout opts -1)))
  (send-binary!!
    [data chan]
    (WebSockets/sendBinaryBlocking data, ^WebSocketChannel chan)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;;; Status codes to close messages: http://tools.ietf.org/html/rfc6455#section-7.4
;;; See also https://github.com/Luka967/websocket-close-codes

(defn normal-closure
  "The code `1000` indicates a normal closure, meaning that the purpose for
  which the connection was established has been fulfilled."
  [] (CloseMessage. 1000 ""))

(defn going-away
  "The code `1001` indicates that an endpoint is \"going away\", such as a
  server going down or a browser having navigated away from a page."
  ([] (CloseMessage. 1001 ""))
  ([reason] (CloseMessage. 1001 reason)))

(defn protocol-error
  "The code `1002` indicates that an endpoint is terminating the connection due
  to a protocol error."
  ([] (CloseMessage. 1002 ""))
  ([reason] (CloseMessage. 1002 reason)))

(defn unsupported-data
  "The code `1003` indicates that an endpoint is terminating the connection
  because it has received a type of data it cannot accept (e.g., an endpoint
  that understands only text data MAY send this if it receives a binary
  message)."
  ([] (CloseMessage. 1003 ""))
  ([reason] (CloseMessage. 1003 reason)))

(defn msg-contains-invalid-data
  "The code `1007` indicates that an endpoint is terminating the connection
  because it has received data within a message that was not consistent with the
  type of the message (e.g., non-UTF-8 data within a text message)."
  ([] (CloseMessage. 1007 ""))
  ([reason] (CloseMessage. 1007 reason)))

(defn msg-violates-policy
  "The code `1008` indicates that an endpoint is terminating the connection
  because it has received a message that violates its policy.  This is a generic
  status code that can be returned when there is no other more suitable status
  code (e.g., 1003 or 1009) or if there is a need to hide specific details about
  the policy."
  ([] (CloseMessage. 1008 ""))
  ([reason] (CloseMessage. 1008 reason)))

(defn msg-too-big
  "The code `1009` indicates that an endpoint is terminating the connection
  because it has received a message that is too big for it to process."
  ([] (CloseMessage. 1009 ""))
  ([reason] (CloseMessage. 1009 reason)))

(defn missing-extensions
  "The code `1010` indicates that an endpoint (client) is terminating the
  connection because it has expected the server to negotiate one or more
  extension, but the server didn't return them in the response message of the
  WebSocket handshake.  The list of extensions that are needed SHOULD appear in
  the /reason/ part of the Close frame. Note that this status code is not used
  by the server, because it can fail the WebSocket handshake instead."
  ([reason] (CloseMessage. 1010 reason)))

(defn unexpected-error
  "The code `1011` indicates that a server is terminating the connection because
  it encountered an unexpected condition that prevented it from fulfilling the
  request."
  ([] (CloseMessage. 1011 ""))
  ([reason] (CloseMessage. 1011 reason)))

(defprotocol WebSocketSendClose
  (send-close
    #_{:arglists '([message chan {:keys [callback]}]
                   [message chan {{:keys [on-complete on-error]} :callback}])}
    [message chan opts]
    "Sends a complete close message, invoking the callback when complete.

    **`message`** (CloseMessage) The close message. See helper functions above.

    **`chan`** (WebSocketChannel) The web socket channel.

    `opts` (map) The options:

    **`:callback`** (WebSocketCallback) The callback to invoke on completion.

    The callback can be a map with keys:

    - `:on-complete` The callback function `(fn [{:keys [callback, channel]}] ...)`.
    - `:on-error` The callback function `(fn [{:keys [callback, channel, error]}] ...)`

    The callback can be also just a function which receives a map with keys
    described above.
    ")
  (send-close!!
    [message chan]
    "Sends a complete close message using blocking IO.

    **`message`** (CloseMessage) The close message. See helper functions above.

    **`chan`** (WebSocketChannel) The web socket channel."))

(extend-protocol WebSocketSendClose CloseMessage
  (send-close
    [message chan opts]
    (WebSockets/sendClose message, ^WebSocketChannel chan
                          (some-> opts :callback types/as-websocket-callback)))
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
