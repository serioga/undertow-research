(ns undertow.exchange
  (:import (io.undertow.io Sender)
           (io.undertow.server HttpServerExchange)
           (io.undertow.server.session Session SessionConfig SessionManager)
           (java.io OutputStream)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn in-io-thread?
  {:inline (fn [exchange] `(.isInIoThread ~(with-meta exchange {:tag 'HttpServerExchange})))}
  [exchange]
  (.isInIoThread ^HttpServerExchange exchange))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn get-session-manager
  {:inline (fn [exchange] `^SessionManager (.getAttachment ~(with-meta exchange {:tag 'HttpServerExchange})
                                              SessionManager/ATTACHMENT_KEY))}
  ^SessionManager
  [exchange]
  (-> ^HttpServerExchange exchange
      (.getAttachment SessionManager/ATTACHMENT_KEY)))

(defn get-session-config
  ^SessionConfig
  [exchange]
  (-> ^HttpServerExchange exchange
      (.getAttachment SessionConfig/ATTACHMENT_KEY)))

(defn get-session
  (^Session
   [exchange] (get-session exchange false))
  (^Session
   [exchange, create?]
   (when-let [mgr (get-session-manager exchange)]
     (let [cfg (get-session-config exchange)]
       (or (-> mgr (.getSession exchange cfg))
           (when create? (-> mgr (.createSession exchange cfg))))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn response-sender
  {:inline (fn [exchange] `^Sender (.getResponseSender ~(with-meta exchange {:tag 'HttpServerExchange})))}
  ^Sender
  [exchange]
  (.getResponseSender ^HttpServerExchange exchange))

(defn start-blocking*
  [^HttpServerExchange e]
  ;; TODO: Explain reasons to close request channel.
  (when (and (.isRequestChannelAvailable e)
             (.isRequestComplete e))
    (-> (.getRequestChannel e)
        (.close)))
  (.startBlocking e))

(defn get-input-stream
  [^HttpServerExchange e]
  (when-not (.isRequestComplete e)
    (when-not (.isBlocking e)
      (.startBlocking e))
    (.getInputStream e)))

(defn new-output-stream
  ^OutputStream
  [^HttpServerExchange e]
  (start-blocking* e)
  (.getOutputStream e))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
