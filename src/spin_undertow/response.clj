(ns spin-undertow.response
  (:import (clojure.lang IPersistentMap)
           (io.undertow.server HttpServerExchange)
           (io.undertow.util HeaderMap HttpString)
           (java.nio.charset Charset)
           (java.util Collection)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn put-headers!
  [exchange headers]
  (reduce (fn [^HeaderMap hm [k v]]
            (cond (sequential? v)
                  (-> hm (.putAll (HttpString. (str k))
                                  ^Collection (map str v)))
                  (some? v)
                  (-> hm (.put (HttpString. (str k)) (str v)))
                  :else
                  (-> hm (.remove (HttpString. (str k))))))
          (.getResponseHeaders ^HttpServerExchange exchange)
          headers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol SpinResponse
  (handle-response [response exchange]))

(defprotocol SpinResponseBody
  (handle-response-body [body exchange]))

(extend-protocol SpinResponse IPersistentMap
  (handle-response
    [response ^HttpServerExchange exchange]
    (when-some [headers,,,,, (.valAt response :headers)],,, (doto exchange (put-headers! headers)))
    (when-some [status,,,,,, (.valAt response :status)],,,, (.setStatusCode exchange status))
    ;(when-some [session,,, (.entryAt response :session)],,, (doto exchange (session/update-values (val session))))
    (when-some [body,,,,,,,, (.valAt response :body)],,,,,, (handle-response-body body exchange))
    nil))

;; Allow just a body in response
(extend-protocol SpinResponse Object
  (handle-response
    [object, exchange]
    (handle-response-body object exchange)))

(extend-protocol SpinResponseBody String
  (handle-response-body
    [string, ^HttpServerExchange exchange]
    (-> (.getResponseSender exchange)
        (.send string (Charset/forName (.getResponseCharset exchange))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
