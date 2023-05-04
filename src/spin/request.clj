(ns spin.request
  (:import (java.util IdentityHashMap Map)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-methods
  ^Map []
  (IdentityHashMap.))

(defn add-method*
  ""
  [methods f id & ks]
  (doseq [k (cons id ks)]
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
     (letfn [(add-type [typ] (fn [method] {:type typ :method method}))]
       (with-meta (->> (distinct (concat (.keySet object-methods)
                                         (.keySet request-methods)))
                       (into {} (map (fn [id] [id (try (request* id) (catch Throwable t t))]))))
                  {::object object
                   ::methods (merge (update-vals request-methods (add-type ::request-method))
                                    (update-vals object-methods (add-type ::object-method)))})))
    ([id]
     (if-let [method (.get object-methods id)]
       (method object id)
       (if-let [method (.get request-methods id)]
         (method request* id)
         (throw (ex-info (str "Undefined request method " id " for " (class object))
                         (meta (request*)))))))
    ([id x]
     (if-let [method (.get object-methods id)]
       (method object id x)
       (if-let [method (.get request-methods id)]
         (method request* id x)
         (throw (ex-info (str "Undefined request method " id " for " (class object))
                         (meta (request*)))))))
    ([id x y]
     (if-let [method (.get object-methods id)]
       (method object id x y)
       (if-let [method (.get request-methods id)]
         (method request* id x y)
         (throw (ex-info (str "Undefined request method " id " for " (class object))
                         (meta (request*)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
