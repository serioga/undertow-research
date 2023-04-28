(ns spin.request
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup MultiFn)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IRequestView
  (get-fn [_ key]
    "")
  (get-methods [_]))

(defn get
  ""
  ([request k] ((get-fn request k)))
  ([request k x] ((get-fn request k) x))
  ([request k x y] ((get-fn request k) x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-fn-impl
  [request k get*]
  (fn
    ([] (get* request k))
    ([x] (get* request k x))
    ([x y] (get* request k x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn abstract-get-methods
  []
  (methods abstract-get))

(comment
  (abstract-get-methods)
  )

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
    [e k]
    (get-fn-impl e k (or (get-method map-get k) abstract-get)))
  (get-methods
    [_]
    (merge (abstract-get-methods)
           (methods map-get))))

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
