(ns undertow.exchange
  (:import (io.undertow.server HttpServerExchange)
           (io.undertow.server.session Session SessionConfig SessionManager)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn in-io-thread?
  {:inline (fn [exchange] `(.isInIoThread ~(with-meta exchange {:tag 'HttpServerExchange})))}
  [exchange]
  (.isInIoThread ^HttpServerExchange exchange))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn get-session-manager
  {:inline (fn [exchange]
             `^SessionManager (.getAttachment ~(with-meta exchange {:tag 'HttpServerExchange})
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
