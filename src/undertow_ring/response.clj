(ns undertow-ring.response
  (:require [clojure.java.io :as io]
            [undertow.exchange :as exchange])
  (:import (io.undertow.server HttpServerExchange)
           (io.undertow.util HeaderMap HttpString)
           (java.io InputStream)
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
  (send-response-body [body exchange]))

;; TODO: Complete list of response body types

;; - ISeq Each element of the seq is sent to the client as a string.
;; - File The contents of the referenced file is sent to the client.
;; - InputStream The contents of the stream is sent to the client. When the stream is exhausted, the stream is closed.

(extend-protocol ResponseBody String
  (send-response-body [body exchange]
    (-> (exchange/response-sender exchange)
        (.send body))))

(extend-protocol ResponseBody ByteBuffer
  (send-response-body [body exchange]
    (-> (exchange/response-sender exchange)
        (.send body))))

(extend-protocol ResponseBody InputStream
  (send-response-body [input ^HttpServerExchange exchange]
    (if (.isInIoThread exchange)
      (.dispatch exchange ^Runnable (^:once fn* [] (send-response-body input exchange)))
      (with-open [input input, output (exchange/new-output-stream exchange)]
        (io/copy input output)
        #_(.endExchange exchange)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn handle-response
  [response, ^HttpServerExchange exchange]
  (some->> (:headers response) (put-headers exchange))
  ;; TODO: Add function for set-status-code
  (some->> (:status response) (.setStatusCode exchange))
  (when-let [[_ session] (find response :session)]
    (update-session exchange session))
  (some-> (:body response) (send-response-body exchange))
  nil)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
