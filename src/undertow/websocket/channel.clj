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
          (on-complete {:channel channel :callback :on-complete})))
      (onError
        [_ channel _ throwable]
        (when-let [on-error (:on-error handlers)]
          (on-error {:channel channel :error throwable :callback :on-error}))))))

(extend-protocol types/AsWebSocketCallback IFn
  (as-websocket-callback
    [callback-fn]
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
