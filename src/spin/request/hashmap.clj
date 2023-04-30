(ns spin.request.hashmap
  (:require [clojure.string :as string]
            [spin.request :as request])
  (:import (clojure.lang IPersistentMap)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn hashmap-value
  [^IPersistentMap m k]
  (.valAt m k))

(defn request-method
  ([^IPersistentMap m _]
   (.valAt m :request-method))
  ([^IPersistentMap m _ x]
   (= x (.valAt m :request-method))))

(defn request-header
  [^IPersistentMap m _ x]
  (some-> ^IPersistentMap (.valAt m :headers)
          (.valAt (string/lower-case x))))

(defn request-header*
  [^IPersistentMap m k x]
  (list (request-header m k x)))

(defn request-state
  ([^IPersistentMap m _ k]
   (-> (.valAt m ::state!) (deref) (get k)))
  ([^IPersistentMap m _ k v]
   (as-> (.valAt m ::state!) state
         (if (some? v)
           (swap! state assoc k v)
           (swap! state dissoc k))
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce lookup-api (request/api-init))

(def add-method (partial request/api-add lookup-api))

;; TODO: return nil for "" query string

(add-method hashmap-value :server-exchange :server-port :server-name :remote-addr :uri :query-string :scheme :body)
(add-method request-method :method :request-method)
(add-method request-header :header)
(add-method request-header* :header*)
(add-method request-state :state!)

;; TODO: cookie/cookie*
;; TODO: query-param

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn create-request
  [m]
  (-> (assoc m ::state! (or (get m ::state!) (atom nil)))
      (request/request-fn lookup-api)))

(comment
  (def -req (create-request {:uri "/uri" :request-method :get
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
