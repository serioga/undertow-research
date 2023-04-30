(ns spin.request
  (:import (clojure.lang PersistentHashMap)
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
