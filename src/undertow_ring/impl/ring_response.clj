(ns undertow-ring.impl.ring-response
  (:require [clojure.java.io :as io]
            [undertow-ring.impl.session :as session]
            [undertow.api.exchange :as exchange])
  (:import (clojure.lang IBlockingDeref IPersistentMap ISeq)
           (io.undertow.io Sender)
           (io.undertow.server HttpHandler HttpServerExchange)
           (io.undertow.server.handlers ResponseCodeHandler)
           (io.undertow.util HeaderMap HttpString)
           (java.io File InputStream OutputStream)
           (java.nio ByteBuffer)
           (java.nio.charset Charset)
           (java.util Collection)
           (java.util.concurrent TimeoutException)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- put-headers
  [exchange headers]
  (reduce-kv (fn [^HeaderMap hm k v]
               (cond (sequential? v)
                     (-> hm (.putAll (HttpString. (str k))
                                     ^Collection (map str v)))
                     (some? v)
                     (-> hm (.put (HttpString. (str k)) (str v)))
                     :else
                     (-> hm (.remove (HttpString. (str k))))))
             (.getResponseHeaders ^HttpServerExchange exchange)
             headers))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol ResponseBody
  ;; TODO: docstring
  (send-response-fn [body]))

(defn response-charset-fn
  [^HttpServerExchange e]
  (fn [] (Charset/forName (.getResponseCharset e))))

(defn with-response-sender
  [send-response]
  (fn [^HttpServerExchange e]
    (send-response (.getResponseSender e) (response-charset-fn e))))

(defn with-output-stream
  [send-response]
  (fn wrap-exchange [^HttpServerExchange e]
    (if (.isInIoThread e)
      (.dispatch e (reify HttpHandler
                     (handleRequest [_ e] (wrap-exchange e))))
      (with-open [output (exchange/new-output-stream e)]
        (send-response output (response-charset-fn e))))))

;; TODO: Complete list of response body types

(extend-protocol ResponseBody String
  (send-response-fn
    [string]
    (with-response-sender (fn send-string [sender, charset-fn]
                            (.send ^Sender sender string ^Charset (charset-fn))))))

(extend-protocol ResponseBody ByteBuffer
  (send-response-fn
    [buffer]
    (with-response-sender (fn send-byte-buffer [sender, _]
                            (.send ^Sender sender buffer)))))

;; InputStream - The contents of the stream is sent to the client. When the
;; stream is exhausted, the stream is closed.
(extend-protocol ResponseBody InputStream
  (send-response-fn
    [input]
    (with-output-stream (fn send-input-stream [output, _]
                          (with-open [input input]
                            (io/copy input output))))))

;; ISeq - Each element of the seq is sent to the client as a string.
(extend-protocol ResponseBody ISeq
  (send-response-fn
    [xs]
    (with-output-stream (fn send-seq [^OutputStream output, charset-fn]
                          (let [charset (charset-fn)]
                            (doseq [x xs]
                              (.write output (-> x str (.getBytes ^Charset charset)))))))))

;; File - The contents of the referenced file is sent to the client.
(extend-protocol ResponseBody File
  (send-response-fn
    [file]
    ;; TODO: Test charset for File response
    (with-output-stream (fn send-file [output, charset-fn]
                          (with-open [input (io/input-stream file)]
                            (io/copy input output))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defprotocol HandleResponse
  (handle-response [response exchange]))

;; Handle Ring response map.
(extend-protocol HandleResponse IPersistentMap
  (handle-response
    [response ^HttpServerExchange exchange]
    (when-some [headers,, (.valAt response :headers)] (doto exchange (put-headers headers)))
    (when-some [status,,, (.valAt response :status)], (doto exchange (.setStatusCode status)))
    (when-some [session (.entryAt response :session)] (doto exchange (session/update-values (val session))))
    (when-some [body,,,,, (.valAt response :body)],,, (doto exchange ((send-response-fn body))))
    nil))

;; Response HTTP 404 for `nil` response.
(extend-protocol HandleResponse nil
  (handle-response [_ exchange]
    (-> ResponseCodeHandler/HANDLE_404 (.handleRequest exchange))))

;; Allow to use any Undertow handler as response.
(extend-protocol HandleResponse HttpHandler
  (handle-response
    [handler exchange]
    (.handleRequest handler exchange)
    nil))

;; Async execution of blocking references (futures and promises).
(extend-protocol HandleResponse IBlockingDeref
  (handle-response [ref exchange]
    (exchange/async-dispatch exchange
      (try
        (let [response (.deref ref 120000 ::timeout)]
          (if (identical? response ::timeout)
            (exchange/throw* exchange (TimeoutException. "Pending response timed out"))
            (handle-response response exchange)))
        (catch Throwable throwable
          (exchange/throw* exchange throwable))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
