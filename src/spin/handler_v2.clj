(ns spin.handler-v2
  (:require [spin.impl-v2.adapter :as adapter]
            [spin.impl-v2.handler :as handler])
  (:import (clojure.lang ILookup)
           (java.util.concurrent CompletableFuture)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-error-handler
  [context f]
  (assoc context :spin/error-handlers (cons f (some-> ^ILookup context (.valAt :spin/error-handlers)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -handle [context handlers]
  (let [p (promise)]
    (-> (reify adapter/HandlerAdapter
          (complete-context [_ context] (deliver p context))
          (complete-error [_ throwable] (deliver p throwable))
          (nio-thread? [_] false)
          (dispatch-blocking [_ f] (f))
          (dispatch-async [_ f] (f)))
        (adapter/run-handlers context handlers))
    (-> (deref p 1000 ::timed-out)
        (handler/value-context))))

(comment
  (def -handle (partial adapter/run-handlers (reify adapter/HandlerAdapter
                                               (complete-context [_ context])
                                               (complete-error [_ throwable])
                                               (nio-thread? [_] false)
                                               (dispatch-blocking [_ f] (f))
                                               (dispatch-async [_ f] (f)))))
  (do (defn -hia [ctx] (assoc ctx :a 1))
      (defn -hia-r [ctx] (reduced (assoc ctx :a 1)))
      (defn -hib [ctx] (assoc ctx :b 2))
      (defn -hi0 [ctx] nil)
      (defn -hie [ctx] (Exception. "hie"))
      (defn -hit [ctx] (throw (Exception. "hit")))
      (defn -hih-f [ctx] -hia)
      (defn -hih-r [ctx] (reduced -hia))
      (defn -hih-c [ctx] [-hia -hib])
      ;; blocking (delay)
      (defn -hba [ctx] (delay (assoc ctx :a 1)))
      (defn -hba-r [ctx] (reduced (delay (assoc ctx :a 1))))
      (defn -hbb [ctx] (delay (assoc ctx :b 2)))
      (defn -hb0 [ctx] (delay nil))
      (defn -hbe [ctx] (delay (Exception. "hbe")))
      (defn -hbt [ctx] (delay (throw (Exception. "hbt"))))
      (defn -hbh-f [ctx] (delay -hia))
      (defn -hbh-r [ctx] (reduced (delay -hia)))
      (defn -hbh-c [ctx] (delay [-hia -hib]))
      ;; async (future)
      (defn -haa [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (assoc ctx :async true)))))
      (defn -haa-r [ctx] (reduced (CompletableFuture/supplyAsync (reify Supplier (get [_] (assoc ctx :async true))))))
      (defn -ha0 [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] nil))))
      (defn -hae [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (Exception. "hae")))))
      (defn -hat [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (throw (Exception. "hat"))))))
      ;; error handling
      (defn -he1 [ctx] (set-error-handler ctx (fn [ctx t] (assoc ctx :response {:status 500 :body (ex-message t)}))))
      (defn -he2 [ctx] (set-error-handler ctx (fn [ctx t] (println ctx "->" (str (class t) ": " (ex-message t))))))
      )
  (-handle {} [-hia -hib])
  (-handle {} [-hia-r -hib])
  (-handle {} [-hia -hi0 -hib])
  (-handle {} [-hia -hie -hib])
  (-handle {} [-hia -hit -hib])
  (-handle {} [-hia-r -hie -hib])
  (-handle {} [-hba -hib])
  (-handle {} [-hia -hbb])
  (-handle {} [-hba -hbb])
  (-handle {} [-hba-r -hib])
  (-handle {} [-hia -hb0 -hib])
  (-handle {} [-hia -hbe -hib])
  (-handle {} [-hia -hbt -hib])
  (-handle {} [-hia -haa -hib])
  (-handle {} [-hia -haa-r -hib])
  (-handle {} [-hia -ha0 -hib])
  (-handle {} [-hia -hae -hib])
  (-handle {} [-hia -hat -hib])
  (-handle {} [-hia-r -hae -hib])
  ;; function as handler chain
  (-handle {} -hia)
  ;; edge cases
  (-handle {} [])
  (-handle {} nil)
  (-handle nil [])
  (-handle nil [-hia])
  (-handle (fn [_] {}) [-hia])
  ;; handlers returning handler chain
  (-handle {} [-hih-f -hib])
  (-handle {} [-hib -hih-f])
  (-handle {} [-hih-r -hib])
  (-handle {} [-hih-c -haa])
  (-handle {} [-hbh-f -hib])
  (-handle {} [-hbb -hih-f])
  (-handle {} [-hbh-r -hib])
  (-handle {} [-hbh-c -haa])
  ;; error handling
  (-handle {} [-he1 -hia -hie -hib])
  (-handle {} [-he2 -hia -hbe -hib])
  (-handle {} [-he1 -he2 -hia -hbe -hib])
  (-handle {} [-he2 -he1 -hia -hbe -hib])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
