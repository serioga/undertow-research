(ns undertow-ring.session
  (:import (clojure.lang IEditableCollection IFn IPersistentMap MapEquivalence)
           (io.undertow.server.session Session)
           (java.util Map)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Keyword keys in session map?

(defn persistent-map
  [^Session session]
  (when session
    (persistent! (reduce (fn [m! k] (assoc! m! k (.getAttribute session k)))
                         (transient {})
                         (.getAttributeNames session)))))

(deftype HeaderMapProxy [session-ref]
  Map
  (size
    [_]
    (count (some-> ^Session @session-ref (.getAttributeNames))))
  (get
    [this k]
    (.valAt this k))
  MapEquivalence
  IFn
  (invoke
    [_ k]
    (some-> ^Session @session-ref (.getAttribute (str k))))
  (invoke
    [_ k not-found]
    (when-some [v (some-> ^Session @session-ref (.getAttribute (str k)))]
      v, not-found))
  IPersistentMap
  (valAt
    [_ k]
    (some-> ^Session @session-ref (.getAttribute (str k))))
  (valAt
    [_ k not-found]
    (when-some [v (some-> ^Session @session-ref (.getAttribute (str k)))]
      v, not-found))
  (entryAt
    [_ k]
    (throw (ex-info "Not implemented: entryAt" {})))
  (containsKey
    [_ k]
    (some? (some-> ^Session @session-ref (.getAttribute (str k)))))
  (assoc
    [_ k v]
    (-> (persistent-map @session-ref)
        (assoc k v)))
  (assocEx
    [_ k v]
    (throw (ex-info "Not implemented: assocEx" {})))
  (cons
    [_ o]
    (-> (or (persistent-map @session-ref) {})
        (conj o)))
  (without
    [_ k]
    (-> (persistent-map @session-ref)
        (dissoc (.toLowerCase (str k)))))
  (empty
    [_]
    {})
  (count
    [_]
    (count (some-> ^Session @session-ref (.getAttributeNames))))
  (seq
    [_]
    (seq (persistent-map @session-ref)))
  (equiv
    [_ o]
    (= o (persistent-map @session-ref)))
  (iterator
    [_]
    (throw (ex-info "Not implemented: iterator" {})))
  IEditableCollection
  (asTransient
    [_]
    (transient (persistent-map @session-ref))))

(defn as-persistent-map [session-ref]
  (HeaderMapProxy. session-ref))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
