(ns spin.request
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol IRequestView
  (get-fn [_ key]
    ""))

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

(defmethod abstract-get :method-get?
  [request _]
  ;; TODO: return true if method is nil
  (= :get (get request :request-method)))

(defmethod abstract-get :method-post?
  [request _] (= :post (get request :request-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: cookie/cookie*
;; TODO: query-param
(extend-protocol IRequestView ILookup
  (get-fn
    [m k]
    (fn
      ([]
       ;; TODO: use abstract-get fallback
       (.valAt m k))
      ([x]
       (case k :header (get (.valAt m :headers) (string/lower-case x))
               :header* (some-> (get m :header x) (string/split #",\s*")))))))

(comment
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
