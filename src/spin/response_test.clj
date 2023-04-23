(ns spin.response-test
  (:require [clojure.core.async :as async]
            [spin.response :as resp])
  (:import (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-response
  [response]
  #_(Thread/sleep 100)
  (if-let [instant (resp/instant response)]
    (instant)
    (if-let [blocking (resp/blocking response)]
      (blocking)
      (do ((resp/async response) identity)
          :async))))

(comment
  (def -nil nil)
  (def -resp {:status 200})

  ;; instant values
  ((resp/instant -nil))
  ((resp/instant -resp))

  (test-response -resp)
  (test-response (resp/fmap -resp identity))

  ;; completable future
  (CompletableFuture.)
  (resp/async (CompletableFuture.))
  ((resp/async (CompletableFuture.)) identity)
  (test-response (-> (CompletableFuture.)
                     (doto (.completeExceptionally (ex-info "Oops" {})))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (resp/fmap (fn [_] (throw (ex-info "oops" {}))))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (resp/fmap identity)))

  ;; delay
  (test-response (delay {:status 200}))
  (test-response (-> (delay {:status 200})
                     (resp/fmap identity)))
  (test-response (-> (delay {})
                     (resp/fmap #(assoc % :status 200))))
  (test-response (delay (throw (ex-info "oops" {}))))
  (test-response (-> (delay (throw (ex-info "oops" {})))
                     (resp/fmap #(assoc % :status 200))))

  ;; core.async
  (test-response (async/go {:status 200}))
  (test-response (async/to-chan! [{:status 200}]))
  (count (.-buf (async/to-chan! [{:status 200}])))
  (def -ch (async/to-chan! [{:status 200}]))
  (def -ch (async/go {:status 200}))
  (def -ch (-> (async/to-chan! [{:status 200}])
               (resp/fmap identity)))
  (count (.-puts -ch))
  (count (.-buf -ch))
  (count (.-puts (async/go {:status 200})))
  (test-response -ch)
  (test-response (-> (async/go {:status 200})
                     (resp/fmap #(assoc % :headers {}))
                     (resp/fmap #(assoc % :body ""))))
  (test-response (-> (async/go (ex-info "oops" {}))
                     (resp/fmap #(assoc % :headers {}))
                     (resp/fmap #(assoc % :body ""))))
  (let [ch (async/chan)]
    (test-response (-> ch
                       (resp/fmap #(assoc % :headers {}))
                       (resp/fmap #(assoc % :body ""))))
    (async/>!! ch {:status 200}))
  (test-response (ex-info "oops" {}))
  (test-response (-> (ex-info "oops" {})
                     (resp/fmap #(assoc % :body ""))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
