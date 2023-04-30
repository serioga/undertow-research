(ns spin.request
  (:require [clojure.string :as string])
  (:import (clojure.lang ILookup PersistentHashMap)
           (java.util IdentityHashMap Map)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn api-init
  ^Map []
  (IdentityHashMap.))

(defn api-add
  ""
  [api, method, k & ks]
  (doseq [k (cons k ks)]
    (.put ^Map api k method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^Map custom-api (api-init))

(def add-method (partial api-add custom-api))

;; TODO: remove extra default keys

(add-method (fn custom-proxy
              ([request _ method] (request method))
              ([request _ method k] (request method k))
              ([request _ method k v] (request method k v)))
            :proxy)

(add-method (fn custom-state-k
              ([request _] (request :state! :state-k))
              ([request _ v] (request :state! :state-k v)))
            :state-k)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn request-fn
  ""
  [obj, ^Map api]
  (fn request*
    ([] (merge (PersistentHashMap/create custom-api)
               (PersistentHashMap/create api)))
    ([k]
     (if-let [method (.get api k)]
       (method obj k)
       (if-let [custom (.get custom-api k)]
         (custom request* k)
         (throw (ex-info (str "Undefined request method " k " for " (class obj))
                         {:methods (request*)})))))
    ([k x]
     (if-let [method (.get api k)]
       (method obj k x)
       (if-let [custom (.get custom-api k)]
         (custom request* k x)
         (throw (ex-info (str "Undefined request method " k " for " (class obj))
                         {:methods (request*)})))))
    ([k x y]
     (if-let [method (.get api k)]
       (method obj k x y)
       (if-let [custom (.get custom-api k)]
         (custom request* k x y)
         (throw (ex-info (str "Undefined request method " k " for " (class obj))
                         {:methods (request*)})))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: change ILookup to Associative because of :state ?

(def ^Map lookup-api (api-init))

(def lookup-api-add (partial api-add lookup-api))

(defn lookup-request-fn
  [m]
  (-> (assoc m ::state! (or (get m ::state!) (atom nil)))
      (request-fn lookup-api)))

(defn lookup-key
  [^ILookup m k] (.valAt m k))

(defn lookup-method
  ([^ILookup m _] (.valAt m :request-method))
  ([^ILookup m _ x] (= x (.valAt m :request-method))))

(defn lookup-header
  [^ILookup m _ x] (some-> ^ILookup (.valAt m :headers)
                           (.valAt (string/lower-case x))))

(defn lookup-header*
  [^ILookup m k x] (list (lookup-header m k x)))

(defn lookup-state!
  ([^ILookup m _ k] (-> (.valAt m ::state!) (deref) (get k)))
  ([^ILookup m _ k v] (as-> (.valAt m ::state!) state
                            (if (some? v)
                              (swap! state assoc k v)
                              (swap! state dissoc k))
                            nil)))

;; TODO: return nil for "" query string

(lookup-api-add lookup-key :server-exchange :server-port :server-name :remote-addr :uri :query-string :scheme :body)
(lookup-api-add lookup-method :method :request-method)
(lookup-api-add lookup-header :header)
(lookup-api-add lookup-header* :header*)
(lookup-api-add lookup-state! :state!)

;; TODO: cookie/cookie*
;; TODO: query-param

(comment
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
  (-req :state! :x)
  (-req :state! :x :val)
  (-req :proxy :method)
  (-req :proxy :method :get)
  (-req :state-k)
  (-req :state-k :x)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
