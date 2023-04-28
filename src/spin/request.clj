(ns spin.request
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup MultiFn)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IRequestView
  (get-fn [_ key]
    "")
  (get-methods* [_]))

;; TODO: better name for dispatch fn?
(defn get-dispatch-key
  ""
  {:arglists '([request, key]
               [request, key, x]
               [request, key, x, y])}
  ([_ k] k)
  ([_ k _] k)
  ([_ k _ _] k))

(defmulti abstract-get
  "Returns value of `key` for arbitrary request implementation."
  {:arglists '([request, key]
               [request, key, x]
               [request, key, x, y])}
  get-dispatch-key)

(defn get
  ""
  ([request k] ((or (get-fn request k) abstract-get)))
  ([request k x] ((or (get-fn request k) abstract-get) x))
  ([request k x y] ((or (get-fn request k) abstract-get) x y)))

(defn get-methods
  [request]
  (merge (methods abstract-get)
         (get-methods* request)))

(defmethod abstract-get :method-get?
  [request _]
  ;; TODO: return true if method is nil
  (= :get (get request :request-method)))

(defmethod abstract-get :method-post?
  [request _] (= :post (get request :request-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti map-get
  ""
  get-dispatch-key)

(defn extend-map
  ""
  [f & ks]
  (doseq [k ks]
    (.addMethod ^MultiFn map-get k f)))

(defn map-value [^ILookup m k] (.valAt m k))
(defn map-header [^ILookup m _ x] (some-> ^ILookup (.valAt m :headers) (.valAt (string/lower-case x))))
(defn map-header* [^ILookup m _ x] (some-> (get m :header x) (string/split #",\s*")))

(extend-map map-value :server-port :server-name :remote-addr :uri :query-string :scheme :request-method :body)
(extend-map map-header :header)
(extend-map map-header* :header*)

;; TODO: cookie/cookie*
;; TODO: query-param
(extend-protocol IRequestView ILookup
  (get-fn
    [_ k]
    (get-method map-get k))
  (get-methods*
    [_]
    (methods map-get)))

(comment
  (get-methods {})
  (def -h {"content-length" "100"
           "content-type" "plain/text"
           "x-test-seq" "1, 2, 3"})
  (string/lower-case "Content-Type")
  (-h (string/lower-case "Content-Type"))
  (-h (string/lower-case "content-type"))
  (get {:headers -h} :header "content-type")
  (get {:headers -h} :header* "content-type")
  (get {:headers -h} :header* "x-test-seq")
  (:headers {:headers {}})

  (get {:uri "/uri"} :uri)
  (get {:request-method :get} :method-get?)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
