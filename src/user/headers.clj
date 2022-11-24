(ns user.headers
  (:require [clojure.string :as string])
  (:import (clojure.lang IEditableCollection IFn IPersistentMap MapEquivalence)
           (io.undertow.util HeaderMap HeaderValues)
           (java.util Map)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn header-name
  [^HeaderValues x]
  (.toLowerCase (.toString (.getHeaderName x))))

(defn header-value
  [^HeaderValues x]
  (if (< 1 (.size x))
    ;; TODO: Why comma separated values?
    (string/join "," x)
    (or (.peekFirst x) "")))

(defn persistent-map
  [^HeaderMap headers]
  (persistent! (reduce (fn [m! x] (assoc! m! (header-name x) (header-value x)))
                       (transient {})
                       headers)))

(deftype HeaderMapProxy [^HeaderMap headers]
  Map
  (size
    [_]
    (.size headers))
  (get
    [this k]
    (.valAt this k))
  MapEquivalence
  IFn
  (invoke
    [_ k]
    (some-> (.get headers (str k)) header-value))
  (invoke
    [_ k not-found]
    (or (some-> (.get headers (str k)) header-value)
        not-found))
  IPersistentMap
  (valAt
    [_ k]
    (some-> (.get headers (str k)) header-value))
  (valAt
    [_ k not-found]
    (or (some-> (.get headers (str k)) header-value)
        not-found))
  (entryAt
    [_ k]
    (throw (ex-info "Not implemented: entryAt" {})))
  (containsKey
    [_ k]
    (.contains headers (str k)))
  (assoc
    [_ k v]
    (-> (persistent-map headers)
        (assoc k v)))
  (assocEx
    [_ k v]
    (throw (ex-info "Not implemented: assocEx" {})))
  (cons
    [_ o]
    (-> (persistent-map headers)
        (conj o)))
  (without
    [_ k]
    (-> (persistent-map headers)
        (dissoc (.toLowerCase (str k)))))
  (empty
    [_]
    {})
  (count
    [_]
    (.size headers))
  (seq
    [_]
    (seq (persistent-map headers)))
  (equiv
    [_ o]
    (= o (persistent-map headers)))
  (iterator
    [_]
    (throw (ex-info "Not implemented: iterator" {})))
  IEditableCollection
  (asTransient
    [_]
    (transient (persistent-map headers))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
