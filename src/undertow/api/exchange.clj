(ns undertow.api.exchange
  (:import (io.undertow.io Sender)
           (io.undertow.server Connectors HttpHandler HttpServerExchange)
           (io.undertow.server.session Session SessionConfig SessionManager)
           (java.io OutputStream)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defmacro async-dispatch
  [exchange expr]
  `(-> ~(with-meta exchange {:tag 'io.undertow.server.HttpServerExchange})
       (.dispatch ^Runnable (^:once fn* [] ~expr))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn get-session-manager
  {:inline (fn [e] `^SessionManager (.getAttachment ~(with-meta e {:tag 'io.undertow.server.HttpServerExchange})
                                                    SessionManager/ATTACHMENT_KEY))}
  ^SessionManager
  [exchange]
  (-> ^HttpServerExchange exchange
      (.getAttachment SessionManager/ATTACHMENT_KEY)))

(defn sessions-enabled?
  {:inline (fn [e] `(boolean (get-session-manager ~e)))}
  [exchange]
  (boolean (get-session-manager exchange)))

(defn get-session-config
  ^SessionConfig
  [exchange]
  (-> ^HttpServerExchange exchange
      (.getAttachment SessionConfig/ATTACHMENT_KEY)))

(defn get-existing-session
  ^Session
  [exchange]
  (some-> (get-session-manager exchange)
          (.getSession exchange (get-session-config exchange))))

(defn get-or-create-session
  ^Session
  [exchange]
  (when-let [mgr (get-session-manager exchange)]
    (let [cfg (get-session-config exchange)]
      (or (.getSession mgr exchange cfg)
          (.createSession mgr exchange cfg)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn response-sender
  {:inline (fn [exchange] `^Sender (.getResponseSender ~(with-meta exchange {:tag 'io.undertow.server.HttpServerExchange})))}
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

(defn throw*
  [exchange, throwable]
  (-> (reify HttpHandler (handleRequest [_ _] (throw throwable)))
      (Connectors/executeRootHandler exchange)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
