(ns spin.response-test
  (:require [clojure.core.async :as async]
            [spin.response :as resp])
  (:import (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-response
  [response]
  (Thread/sleep 100)
  (if-let [async (resp/async response)]
    (do (async prn) :async)
    (resp/value response)))

(comment

  ;; sync values
  (resp/value nil)
  (resp/value {:status 200})
  (resp/error {:status 200})
  (resp/value (ex-info "oops" {}))
  (resp/error (ex-info "oops" {}))

  ;; completable future
  (CompletableFuture.)
  (resp/async (CompletableFuture.))
  ((resp/async (CompletableFuture.)) identity)
  (test-response (-> (CompletableFuture.)
                     (doto (.completeExceptionally (ex-info "Oops" {})))))
  (resp/error (-> (CompletableFuture.)
                  (doto (.completeExceptionally (ex-info "Oops" {})))))
  (resp/error (CompletableFuture.))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (resp/apply (fn [_] (throw (ex-info "oops" {}))))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))))
  (test-response {:status 200})
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (resp/apply identity)))
  (test-response (resp/apply {:status 200} identity))

  ;; core.async
  (test-response (async/go {:status 200}))
  (test-response (async/to-chan! [{:status 200}]))
  (count (.-buf (async/to-chan! [{:status 200}])))
  (def -ch (async/to-chan! [{:status 200}]))
  (def -ch (async/go {:status 200}))
  (def -ch (-> (async/to-chan! [{:status 200}])
               (resp/apply identity)))
  (count (.-puts -ch))
  (count (.-buf -ch))
  (count (.-puts (async/go {:status 200})))
  (test-response -ch)
  (test-response (-> (async/go {:status 200})
                     (resp/apply #(assoc % :headers {}))
                     (resp/apply #(assoc % :body ""))))
  (test-response (-> (async/go (ex-info "oops" {}))
                     (resp/apply #(assoc % :headers {}))
                     (resp/apply #(assoc % :body ""))))
  (let [ch (async/chan)]
    (test-response (-> ch
                       (resp/apply #(assoc % :headers {}))
                       (resp/apply #(assoc % :body ""))))
    (async/>!! ch {:status 200}))
  (test-response (ex-info "oops" {}))
  (test-response (-> (ex-info "oops" {})
                     (resp/apply #(assoc % :body ""))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
