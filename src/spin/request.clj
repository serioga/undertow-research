(ns spin.request
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISpinRequest
  (internal [_])
  (server-port [_])
  (server-name [_])
  (remote-addr [_])
  (uri [_])
  (query [_])
  (scheme [_])
  (method [_])
  (body ^java.io.InputStream [_])
  (path-info [_])
  (protocol [_])
  (header [_ name] "Returns first value of the header `name`.")
  (header* [_ name] "Returns seq of all values of the header `name`.")
  (cookie [_ name])
  (cookie* [_ name]))

(defn method-get? [req]
  ;; TODO: inline function
  (.equals :get (method req)))

(defn method-post? [req]
  ;; TODO: inline function
  (.equals :post (method req)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-protocol ISpinRequest ILookup
  (internal [m] m)
  (server-port [m] (.valAt m :server-port))
  (server-name [m] (.valAt m :server-name))
  (remote-addr [m] (.valAt m :remote-addr))
  (uri [m],,,,,,,, (.valAt m :uri))
  (query [m],,,,,, (.valAt m :query-string))
  (scheme [m],,,,, (.valAt m :scheme))
  (method [m],,,,, (.valAt m :request-method))
  (body [m],,,,,,, (.valAt m :body))
  (path-info [m],, (.valAt m :path-info))
  (protocol [m],,, (.valAt m :protocol))
  (header
    [m h] (get (.valAt m :headers) (string/lower-case h)))
  (header*
    [m h] (some-> (header m h) (string/split #",\s*")))
  ;; TODO: cookie/cookie*
  )

(comment
  (def -h {"content-length" "100"
           "content-type" "plain/text"
           "x-test-seq" "1, 2, 3"})
  (string/lower-case "Content-Type")
  (-h (string/lower-case "Content-Type"))
  (-h (string/lower-case "content-type"))
  (header {:headers -h} "content-type")
  (header* {:headers -h} "content-type")
  (header* {:headers -h} "x-test-seq")
  (:headers {:headers {}})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
