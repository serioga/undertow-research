(ns undertow-ring.response
  (:require [clojure.java.io :as io]
            [undertow.exchange :as exchange])
  (:import (clojure.lang IPersistentMap ISeq)
           (io.undertow.io Sender)
           (io.undertow.server HttpServerExchange)
           (io.undertow.util HeaderMap HttpString)
           (java.io File InputStream OutputStream)
           (java.nio ByteBuffer)
           (java.util Collection)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- put-headers
  [exchange headers]
  (reduce-kv (fn [^HeaderMap hs k v]
               (cond (sequential? v)
                     (-> hs (.putAll (HttpString. (str k))
                                     ^Collection (map str v)))
                     (some? v)
                     (-> hs (.put (HttpString. (str k)) (str v)))
                     :else
                     (-> hs (.remove (HttpString. (str k))))))
             (.getResponseHeaders ^HttpServerExchange exchange)
             headers))

(defn- update-session
  [^HttpServerExchange exchange values]
  (let [sess (exchange/get-session exchange values)]
    ;; TODO: Handle case when session manager is not assigned
    (when (and values (not sess))
      (throw (ex-info "Attempt to set session values in undefined session"
                      {:undertow/exchange exchange})))
    (if values
      (doseq [[k v] values]
        (if (some? v) (-> sess (.setAttribute (name k) v))
                      (-> sess (.removeAttribute (name k)))))
      (some-> sess (.invalidate exchange)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol ResponseBody
  ;; TODO: docstring
  (body-handler [body])
  (send-body [body using]))

(defn response-sender-handler
  [^HttpServerExchange e, body]
  (send-body body (.getResponseSender e)))

(defn output-stream-handler
  [^HttpServerExchange e, body]
  (if (.isInIoThread e)
    (.dispatch e ^Runnable (^:once fn* [] (output-stream-handler e body)))
    (with-open [output (exchange/new-output-stream e)]
      (send-body body output))))

;; TODO: Complete list of response body types

(extend-protocol ResponseBody String
  (body-handler [_] response-sender-handler)
  (send-body
    [string, sender]
    (.send ^Sender sender string)))

(extend-protocol ResponseBody ByteBuffer
  (body-handler [_] response-sender-handler)
  (send-body
    [buffer, sender]
    (.send ^Sender sender buffer)))

;; InputStream - The contents of the stream is sent to the client. When the
;; stream is exhausted, the stream is closed.
(extend-protocol ResponseBody InputStream
  (body-handler [_] output-stream-handler)
  (send-body
    [input, output]
    (with-open [input input]
      (io/copy input output))))

;; ISeq - Each element of the seq is sent to the client as a string.
(extend-protocol ResponseBody ISeq
  (body-handler [_] output-stream-handler)
  (send-body
    [xs, ^OutputStream output]
    ;; TODO: encoding?
    (doseq [x xs]
      (.write output (-> x str .getBytes)))))

;; File - The contents of the referenced file is sent to the client.
(extend-protocol ResponseBody File
  (body-handler [_] output-stream-handler)
  (send-body
    [file, ^OutputStream output]
    (with-open [input (io/input-stream file)]
      (io/copy input output))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handle-response
  [^IPersistentMap response, ^HttpServerExchange exchange]
  (when response
    (when-some [headers,, (.valAt response :headers)] (doto exchange (put-headers headers)))
    (when-some [status,,, (.valAt response :status)], (doto exchange (.setStatusCode status)))
    (when-some [session (.entryAt response :session)] (doto exchange (update-session (val session))))
    (when-some [body,,,,, (.valAt response :body)],,, (doto exchange ((body-handler body) body))))
  nil)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
