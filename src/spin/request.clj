(ns spin.request
  (:refer-clojure :exclude [key])
  (:import (clojure.lang PersistentHashMap)
           (java.util IdentityHashMap Map)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-methods
  ^Map []
  (IdentityHashMap.))

(defn add-method*
  ""
  [methods f key & ks]
  (doseq [k (cons key ks)]
    (.put ^Map methods k f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce ^Map request-methods (create-methods))

(def add-method (partial add-method* request-methods))

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

(defn create-request
  ""
  [object, ^Map object-methods]
  (fn request*
    ([] (merge (PersistentHashMap/create request-methods)
               (PersistentHashMap/create object-methods)))
    ([key]
     (if-let [method (.get object-methods key)]
       (method object key)
       (if-let [method (.get request-methods key)]
         (method request* key)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         {:methods (request*)})))))
    ([key x]
     (if-let [method (.get object-methods key)]
       (method object key x)
       (if-let [method (.get request-methods key)]
         (method request* key x)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         {:methods (request*)})))))
    ([key x y]
     (if-let [method (.get object-methods key)]
       (method object key x y)
       (if-let [method (.get request-methods key)]
         (method request* key x y)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         {:methods (request*)})))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
