(ns spin.handler-v2
  (:require [spin.impl-v2.adapter :as adapter]
            [spin.impl-v2.handler :as handler])
  (:import (clojure.lang ILookup)
           (java.util.concurrent CompletableFuture)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn blocking
  [handler]
  (fn [context]
    (if (adapter/thread-nio?)
      (delay handler)
      (handler context))))

(defn set-error-handler
  [context f]
  (assoc context :spin/error-handlers (handler/prepend-seq f (some-> ^ILookup context (.valAt :spin/error-handlers)))))

(defn set-response-handler
  [context f]
  (assoc context :spin/response-handlers (handler/append-vec f (some-> ^ILookup context (.valAt :spin/response-handlers)))))

(comment
  (set-error-handler {} identity)
  (set-error-handler {} [identity])
  (set-response-handler {} identity)
  (set-response-handler {} [identity])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn -handle [context handlers]
  (let [p (promise) nio! (atom true)]
    (-> (reify adapter/HandlerAdapter
          (result-context [_ context] (deliver p context))
          (result-error [_ context throwable]
            (println context "-> result-error:" (ex-message throwable))
            (deliver p throwable))
          (nio? [_] @nio!)
          (blocking-call [_ f] (future (reset! nio! false) (f)))
          (async-call [_ f] (f)))
        (adapter/run-handlers context handlers))
    (-> (deref p 1000 ::timed-out)
        (handler/get-context))))

(comment
  (def -handle (partial adapter/run-handlers (reify adapter/HandlerAdapter
                                               (result-context [_ context])
                                               (result-error [_ _ throwable])
                                               (nio? [_] false)
                                               (blocking-call [_ f] (f))
                                               (async-call [_ f] (f)))))
  (do (defn -hia [ctx] (assoc ctx :a 1))
      (defn -hia-r [ctx] (reduced (assoc ctx :a 1)))
      (defn -hib [ctx] (assoc ctx :b 2))
      (defn -hi0 [ctx] nil)
      (defn -hie [ctx] (Exception. "hie"))
      (defn -hit [ctx] (throw (Exception. "hit")))
      (defn -hih-f [ctx] -hia)
      (defn -hih-r [ctx] (reduced -hia))
      (defn -hih-c [ctx] [-hia -hib])
      ;; delay
      (defn -hda [ctx] (delay (assoc ctx :a 1)))
      (defn -hda-r [ctx] (reduced (delay (assoc ctx :a 1))))
      (defn -hdb [ctx] (delay (assoc ctx :b 2)))
      (defn -hd0 [ctx] (delay nil))
      (defn -hde [ctx] (delay (Exception. "hde")))
      (defn -hdt [ctx] (delay (throw (Exception. "hdt"))))
      (defn -hdh-f [ctx] (delay -hia))
      (defn -hdh-r [ctx] (reduced (delay -hia)))
      (defn -hdh-c [ctx] (delay [-hia -hib]))
      ;; blocking handler
      (def -hba (blocking (fn [ctx] (assoc ctx :a 1))))
      (def -hba-r (blocking (fn [ctx] (reduced (assoc ctx :a 1)))))
      (def -hbb (blocking (fn [ctx] (assoc ctx :b 2))))
      (def -hb0 (blocking (fn [ctx] nil)))
      (def -hbe (blocking (fn [ctx] (Exception. "hbe"))))
      (def -hbt (blocking (fn [ctx] (throw (Exception. "hbt")))))
      ;; async (future)
      (defn -haa [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (assoc ctx :async true)))))
      (defn -haa-r [ctx] (reduced (CompletableFuture/supplyAsync (reify Supplier (get [_] (assoc ctx :async true))))))
      (defn -ha0 [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] nil))))
      (defn -hae [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (Exception. "hae")))))
      (defn -hat [ctx] (CompletableFuture/supplyAsync (reify Supplier (get [_] (throw (Exception. "hat"))))))
      ;; error handling
      (defn -he1 [ctx] (set-error-handler ctx (fn [ctx t] (assoc ctx :response {:status 500 :body (ex-message t)}))))
      (defn -he2 [ctx] (set-error-handler ctx (fn [ctx t] (println ctx "->" (str (class t) ": " (ex-message t))))))
      ;; response handlers
      (defn -hir [ctx] (set-response-handler ctx -hib))
      )
  (def -xsi (repeat 1000 identity))
  (-handle {} -xsi)
  ;; instant
  (-handle {} [-hia -hib])
  (-handle {} [-hia-r -hib])
  (-handle {} [-hia -hi0 -hib])
  (-handle {} [-hia -hie -hib])
  (-handle {} [-hia -hit -hib])
  (-handle {} [-hia-r -hie -hib])
  ;; delay
  (-handle {} [-hda -hib])
  (-handle {} [-hia -hdb])
  (-handle {} [-hda -hdb])
  (-handle {} [-hda-r -hib])
  (-handle {} [-hia -hd0 -hib])
  (-handle {} [-hia -hde -hib])
  (-handle {} [-hia -hdt -hib])
  ;; blocking
  (-handle {} [-hba -hib])
  (-handle {} [-hia -hbb])
  (-handle {} [-hba -hdb])
  (-handle {} [-hba-r -hib])
  (-handle {} [-hia -hb0 -hib])
  (-handle {} [-hia -hbe -hib])
  (-handle {} [-hia -hbt -hib])
  ;; async
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
  (-handle {} [-hdh-f -hib])
  (-handle {} [-hdb -hih-f])
  (-handle {} [-hdh-r -hib])
  (-handle {} [-hdh-c -haa])
  ;; error handling
  (-handle {} [-he1 -hia -hie -hib])
  (-handle {} [-he2 -hia -hde -hib])
  (-handle {} [-he2 -hia -hdt -hib])
  (-handle {} [-he1 -he2 -hia -hde -hib])
  (-handle {} [-he2 -he1 -hia -hde -hib])
  (-handle {} [-he1 -he2 -hia -hdt -hib])
  (-handle {} [-he2 -he1 -hia -hdt -hib])
  ;; response handlers
  (-handle {} [-hir -he1])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
