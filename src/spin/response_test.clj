(ns spin.response-test
  (:require [clojure.core.async :as async]
            [spin.handler :as handler])
  (:import (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn test-response
  [result]
  #_(Thread/sleep 100)
  (if-let [instant-result (handler/instant-result-fn result)]
    (instant-result)
    (if-let [blocking-result (handler/blocking-result-fn result)]
      (blocking-result)
      (do ((handler/async-result-fn result) identity)
          :async))))

(comment
  (def -nil nil)
  (def -resp {:status 200})

  ;; instant values
  ((handler/instant-result-fn -nil))
  ((handler/instant-result-fn -resp))

  (test-response -resp)
  (test-response (handler/update-result -resp identity))

  ;; completable future
  (CompletableFuture.)
  (handler/async-result-fn (CompletableFuture.))
  ((handler/async-result-fn (CompletableFuture.)) identity)
  (test-response (-> (CompletableFuture.)
                     (doto (.completeExceptionally (ex-info "Oops" {})))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (handler/update-result (fn [_] (throw (ex-info "oops" {}))))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))))
  (test-response (-> (CompletableFuture.)
                     (doto (.complete {:status 200}))
                     (handler/update-result identity)))

  ;; delay
  (test-response (delay {:status 200}))
  (test-response (-> (delay {:status 200})
                     (handler/update-result identity)))
  (test-response (-> (delay {})
                     (handler/update-result #(assoc % :status 200))))
  (test-response (delay (throw (ex-info "oops" {}))))
  (test-response (-> (delay (throw (ex-info "oops" {})))
                     (handler/update-result #(assoc % :status 200))))

  ;; core.async
  (test-response (async/go {:status 200}))
  (test-response (async/to-chan! [{:status 200}]))
  (count (.-buf (async/to-chan! [{:status 200}])))
  (def -ch (async/to-chan! [{:status 200}]))
  (def -ch (async/go {:status 200}))
  (def -ch (-> (async/to-chan! [{:status 200}])
               (handler/update-result identity)))
  (count (.-puts -ch))
  (count (.-buf -ch))
  (count (.-puts (async/go {:status 200})))
  (test-response -ch)
  (test-response (-> (async/go {:status 200})
                     (handler/update-result #(assoc % :headers {}))
                     (handler/update-result #(assoc % :body ""))))
  (test-response (-> (async/go (ex-info "oops" {}))
                     (handler/update-result #(assoc % :headers {}))
                     (handler/update-result #(assoc % :body ""))))
  (let [ch (async/chan)]
    (test-response (-> ch
                       (handler/update-result #(assoc % :headers {}))
                       (handler/update-result #(assoc % :body ""))))
    (async/>!! ch {:status 200}))
  (test-response (ex-info "oops" {}))
  (test-response (-> (ex-info "oops" {})
                     (handler/update-result #(assoc % :body ""))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
