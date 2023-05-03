(ns spin.request
  (:refer-clojure :exclude [key])
  (:import (java.util IdentityHashMap Map)))

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
    ([]
     (letfn [(add-method-type [typ] (fn [method] {:type typ :method method}))]
       (with-meta (->> (distinct (concat (.keySet object-methods)
                                         (.keySet request-methods)))
                       (into {} (map (fn [key] [key (try (request* key) (catch Throwable t t))]))))
                  {::object object
                   ::methods (merge (update-vals request-methods (add-method-type ::request-method))
                                    (update-vals object-methods (add-method-type ::object-method)))})))
    ([key]
     (if-let [method (.get object-methods key)]
       (method object key)
       (if-let [method (.get request-methods key)]
         (method request* key)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         (meta (request*)))))))
    ([key x]
     (if-let [method (.get object-methods key)]
       (method object key x)
       (if-let [method (.get request-methods key)]
         (method request* key x)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         (meta (request*)))))))
    ([key x y]
     (if-let [method (.get object-methods key)]
       (method object key x y)
       (if-let [method (.get request-methods key)]
         (method request* key x y)
         (throw (ex-info (str "Undefined request method " key " for " (class object))
                         (meta (request*)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
