(ns spin.request
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISpinRequest
  (internal [_])
  (as-map [_])
  (server-port [_])
  (server-name [_])
  (remote-addr [_])
  (uri [_])
  (query-string [_])
  (request-method [_])
  (body [_])
  (path-info [_])
  (protocol [_])
  (header [_ name])
  (header-seq [_ name]))

(extend-protocol ISpinRequest ILookup
  (internal [m] m)
  (as-map [m] m)
  (server-port [m],,, (.valAt m :server-port))
  (server-name [m],,, (.valAt m :server-name))
  (remote-addr [m],,, (.valAt m :remote-addr))
  (uri [m],,,,,,,,,,, (.valAt m :uri))
  (query-string [m],, (.valAt m :query-string))
  (request-method [m] (.valAt m :request-method))
  (body [m],,,,,,,,,, (.valAt m :body))
  (path-info [m],,,,, (.valAt m :path-info))
  (protocol [m],,,,,, (.valAt m :protocol))
  (header
    [m h] (get (.valAt m :headers) (string/lower-case h)))
  (header-seq
    [m h] (some-> (header m h) (string/split #",\s*"))))

(comment
  (def -h {"content-length" "100"
           "content-type" "plain/text"
           "x-test-seq" "1, 2, 3"})
  (string/lower-case "Content-Type")
  (-h (string/lower-case "Content-Type"))
  (-h (string/lower-case "content-type"))
  (header {:headers -h} "content-type")
  (header-seq {:headers -h} "content-type")
  (header-seq {:headers -h} "x-test-seq")
  (:headers {:headers {}})
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
