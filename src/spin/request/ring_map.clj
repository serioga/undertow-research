(ns spin.request.ring-map
  ""
  (:refer-clojure :exclude [key])
  (:require [clojure.string :as string]
            [spin.request :as request])
  (:import (clojure.lang IPersistentMap)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -default
  [^IPersistentMap m k]
  (.valAt m k))

(defn -method
  ([^IPersistentMap m _]
   (.valAt m :request-method))
  ([^IPersistentMap m _ x]
   (= x (.valAt m :request-method))))

(defn -header
  ([^IPersistentMap m _]
   (some->> (.valAt m :headers)
            (map #(update % 1 list))))
  ([^IPersistentMap m _ x]
   (some-> ^IPersistentMap (.valAt m :headers)
           (.valAt (string/lower-case x))))
  ([^IPersistentMap m _ x many?]
   (cond-> (-header m _ x)
     many? (list))))

(defn -state
  ([^IPersistentMap m _]
   (-> (.valAt m ::state!) (deref)))
  ([^IPersistentMap m _ k]
   (-> (.valAt m ::state!) (deref) (get k)))
  ([^IPersistentMap m _ k v]
   (as-> (.valAt m ::state!) state
         (if (some? v)
           (swap! state assoc k v)
           (swap! state dissoc k))
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce hashmap-methods (request/create-methods))

(def add-method (partial request/add-method* hashmap-methods))

;; TODO: return nil for "" query string

(add-method -default :server-exchange :server-port :server-name :remote-addr :uri :query-string :scheme :body)
(add-method -method :method :request-method)
(add-method -header :header)
(add-method -state :state!)

;; TODO: cookie/cookie*
;; TODO: query-param

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-request
  [m]
  (-> (assoc m ::state! (or (get m ::state!) (atom nil)))
      (request/create-request hashmap-methods)))

(comment
  (def -req (create-request {:uri "/uri" :request-method :get
                             :headers {"content-length" "100"
                                       "content-type" "plain/text"
                                       "x-test-seq" "1, 2, 3"}}))
  (-req)
  (meta (-req))
  (-req :header)
  (-req :header "content-type")
  (-req :header "x-test-seq")
  (-req :header "x-test-seq" :many)
  (-req :header "x-test-seq" false)
  (-req :uri)
  (-req :method)
  (-req :method :get)
  (-req :state!)
  (-req :state! :x)
  (-req :state! :x :val)
  (-req :state! :x nil)
  (-req :proxy :method)
  (-req :proxy :method :get)
  (-req :state-k)
  (-req :state-k :x)

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
