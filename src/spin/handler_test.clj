(ns spin.handler-test
  (:require [clojure.core.async :as async]
            [spin.handler :as handler])
  (:import (clojure.lang IDeref)
           (java.util.concurrent CompletableFuture)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle
  [result]
  #_(Thread/sleep 100)
  (if-let [ref (handler/instant-result result)]
    (.deref ^IDeref ref)
    (if-let [ref (handler/blocking-result result)]
      (.deref ^IDeref ref)
      (do ((handler/async-result result) prn)
          :async))))

(comment
  (def -nil nil)
  (def -result {:response {:status 200}})

  ;; instant values
  (handler/instant-result -nil)
  (handler/instant-result -result)

  (handle -result)
  (handle (handler/update-result -result identity))

  ;; completable future
  (CompletableFuture.)
  (handler/async-result (CompletableFuture.))
  ((handler/async-result (CompletableFuture.)) identity)
  (handle (-> (CompletableFuture.)
              (doto (.completeExceptionally (ex-info "Oops" {})))))
  (handle (-> (CompletableFuture.)
              (doto (.complete {:response {:status 200}}))
              (handler/update-result (fn [_] (throw (ex-info "oops" {}))))))
  (handle (-> (CompletableFuture.)
              (doto (.complete {:response {:status 200}}))))
  (handle (-> (CompletableFuture.)
              (doto (.complete {:response {:status 200}}))
              (handler/update-result identity)))

  ;; delay
  (handle (delay {:response {:status 200}}))
  (handle (-> (delay {:response {:status 200}})
              (handler/update-result identity)))
  (handle (-> (delay {})
              (handler/update-result #(assoc % :response {:status 200}))))
  (handle (delay (throw (ex-info "oops" {}))))
  (handle (-> (delay (throw (ex-info "oops" {})))
              (handler/update-result #(assoc % :response {:status 200}))))

  ;; core.async
  (handle (async/go {:response {:status 200}}))
  (handle (async/to-chan! [{:response {:status 200}}]))
  (count (.-buf (async/to-chan! [{:response {:status 200}}])))
  (def -ch (async/to-chan! [{:response {:status 200}}]))
  (def -ch (async/go {:response {:status 200}}))
  (def -ch (-> (async/to-chan! [{:response {:status 200}}])
               (handler/update-result identity)))
  (count (.-puts -ch))
  (count (.-buf -ch))
  (count (.-puts (async/go {:response {:status 200}})))
  (handle -ch)
  (handle (-> (async/go {:response {:status 200}})
              (handler/update-result #(update % :response assoc :headers {}))
              (handler/update-result #(update % :response assoc :body ""))))
  (handle (-> (async/go (ex-info "oops" {}))
              (handler/update-result #(update % :response assoc :headers {}))
              (handler/update-result #(update % :response assoc :body ""))))
  (let [ch (async/chan)]
    (handle (-> ch
                (handler/update-result #(update % :response assoc :headers {}))
                (handler/update-result #(update % :response assoc :body ""))))
    (async/>!! ch {:response {:status 200}}))
  (handle (ex-info "oops" {}))
  (handle (-> (ex-info "oops" {})
              (handler/update-result #(update % :response assoc :body ""))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
