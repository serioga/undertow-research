(ns spin.request
  (:refer-clojure :exclude [get])
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup MultiFn)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn api-dispatch
  ""
  {:arglists '([request, method]
               [request, method, x]
               [request, method, x, y])}
  ([_ k] k)
  ([_ k _] k)
  ([_ k _ _] k))

(defn request-fn
  ""
  [obj api]
  (fn
    ([] (methods api))
    ([method] (api obj method))
    ([method x] (api obj method x))
    ([method x y] (api obj method x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn api-add
  ""
  [multi, method, method-name & more-names]
  (doseq [n (cons method-name more-names)]
    (.addMethod ^MultiFn multi n method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti lookup-api
  ""
  api-dispatch)

(def lookup-api-add (partial api-add lookup-api))

(defn lookup-request-fn
  [m]
  (request-fn m lookup-api))

(defn lookup-key
  [^ILookup m k] (.valAt m k))

(defn lookup-method
  ([^ILookup m _] (.valAt m :request-method))
  ([^ILookup m _ x] (= x (.valAt m :request-method))))

(defn lookup-header
  [^ILookup m _ x] (some-> ^ILookup (.valAt m :headers)
                           (.valAt (string/lower-case x))))

(defn lookup-header*
  [^ILookup m _ x] (list (lookup-header m :header x)))

;; TODO: return nil for "" query string

(lookup-api-add lookup-key :server-exchange :server-port :server-name :remote-addr :uri :query-string :scheme :body)
(lookup-api-add lookup-method :method :request-method)
(lookup-api-add lookup-header :header)
(lookup-api-add lookup-header* :header*)

;; TODO: cookie/cookie*
;; TODO: query-param

(comment
  (methods lookup-api)
  (def -req (lookup-request-fn {:uri "/uri" :request-method :get
                                :headers {"content-length" "100"
                                          "content-type" "plain/text"
                                          "x-test-seq" "1, 2, 3"}}))
  (-req)
  (-req :header "content-type")
  (-req :header* "content-type")
  (-req :header "x-test-seq")
  (-req :header* "x-test-seq")
  (-req :uri)
  (-req :method)
  (-req :method :get)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
