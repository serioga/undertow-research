(ns spin.handler-v2
  (:import (clojure.lang Delay Fn IPersistentMap RT Sequential)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol HandlerResult
  "The abstraction for http handler result:

  - new context map
  - handler chain to execute over context map
  - error.
  "

  (result-context
    [result]
    "Returns context map from the result, or nil. Throws exception for errors.")

  (result-handlers
    [result]
    "Returns handler seq from the result, or nil. Throws exception for errors."))

(defprotocol HandlerReturn
  "The abstraction for return value from http handler:

  - instant (non-blocking) result
  - blocking result
  - async result
  "

  (instant-result
    [return]
    "When result is instantly available returns function `(fn [] result)`
    which returns result or throws exception.")

  (blocking-result
    [return]
    "When result is available only in blocking call returns function
    `(fn [] result)` which returns result or throws exception. This function
    should not be called on IO thread.")

  (async-result
    [return]
    ;; TODO: review docstring.
    "Returns `nil` for instant result. For async result returns function
    `(fn [f callback] ... (callback result))` which receives 1-arity callback to
    listen for future result completion."))

(defprotocol HandlerImpl
  ""
  (impl-complete [impl context])
  (impl-error [impl throwable])
  (impl-nio? [impl])
  (impl-blocking [impl f])
  (impl-async [impl f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## HandlerResult for base types ##

;; Persistent map as context map.
(extend-protocol HandlerResult IPersistentMap
  (result-context,,,, [m] m)
  (result-handlers,,, [_] nil))

;; Sequential as handler chain.
(extend-protocol HandlerResult Sequential
  (result-context,,,, [_] nil)
  (result-handlers,,, [s] s))

;; Function as 1-item handler chain.
(extend-protocol HandlerResult Fn
  (result-context,,,, [_] nil)
  (result-handlers,,, [f] (RT/list f)))

;; Exceptions as context error.
(extend-protocol HandlerResult Throwable
  (result-context,,,, [t] (throw t))
  (result-handlers,,, [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Instant result implementations ##

;; ### Any object defaults ###
;;
;; All types represent instant result by default.

(extend-protocol HandlerReturn Object
  (instant-result,,,, [o] (fn [] o))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil))

(extend-protocol HandlerReturn nil
  (instant-result,,,, [_] (fn [] nil))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil))

;; ### Exceptions ###
;;
;; Exceptions represents error result.

(extend-protocol HandlerReturn Throwable
  (instant-result,,,, [t] (fn throwable-instant [] (throw t)))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Blocking/async result implementations ##

;; ### Delay as blocking result ###

(extend-protocol HandlerReturn Delay
  (instant-result
    [d]
    (when (.isRealized d)
      (fn delay-instant [] (.deref d))))
  (blocking-result
    [d]
    (fn delay-blocking [] (.deref d)))
  (async-result
    [_] nil))

;; ### CompletableFuture as async result ###

(extend-protocol HandlerReturn CompletableFuture
  (instant-result
    [ft]
    (when (.isDone ft)
      (fn future-instant [] (.get ft))))
  (blocking-result
    [_] nil)
  (async-result
    [ft]
    (fn future-async [callback]
      (.whenComplete ft (reify BiConsumer (accept [_ v e] (callback (or e v)))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn apply-handlers
  [impl context handlers]
  (letfn [(reduce* [prev result chain]
            (try
              (loop [prev prev, result result, chain (seq chain)]
                (cond
                  result
                  (if-let [context (result-context result)]
                    (if chain
                      (if-let [handler (first chain)]
                        (let [return (handler context)
                              reduced-return? (reduced? return)
                              return (if reduced-return? (unreduced return) return)
                              chain (when-not reduced-return? (next chain))]
                          (if-let [instant (instant-result return)]
                            (recur context (instant) chain)
                            (if-let [blocking (blocking-result return)]
                              (if (impl-nio? impl)
                                (impl-blocking impl (^:once fn* [] (reduce* context (blocking) chain)))
                                (recur context (blocking) chain))
                              (if-let [async (async-result return)]
                                (impl-async impl (^:once fn* [] (async (fn [result] (reduce* context result chain)))))
                                (throw (ex-info (str "Cannot handle return: " return) {}))))))
                        ;; handler is falsy, skip
                        (recur prev result (next chain)))
                      ;; chain is empty, complete
                      (impl-complete impl context))
                    (if-let [chain+ (result-handlers result)]
                      (recur nil prev (concat chain+ chain))
                      (throw (ex-info (str "Handler result is not new context or handler chain: " result)
                                      {::result result ::chain chain}))))
                  prev
                  (recur nil prev chain)
                  :else
                  (throw (ex-info "Handle empty context" {::chain chain}))))
              (catch Throwable t
                (impl-error impl t)))
            nil)]
    (assert (map? context) (str "Requires context map to apply handlers "
                                {:context context :handlers handlers}))
    (reduce* nil context (some-> handlers (result-handlers)))))

(defn -handle [ctx handlers]
  (let [p (promise)]
    (-> (reify HandlerImpl
          (impl-complete [_ context] (deliver p context))
          (impl-error [_ throwable] (deliver p throwable))
          (impl-nio? [_] false)
          (impl-blocking [_ f] (f))
          (impl-async [_ f] (f)))
        (apply-handlers ctx handlers))
    (-> (deref p 1000 ::timed-out)
        (result-context))))

(comment
  (def -handle (partial apply-handlers (reify HandlerImpl
                                         (impl-complete [_ context])
                                         (impl-error [_ throwable])
                                         (impl-nio? [_] false)
                                         (impl-blocking [_ f] (f))
                                         (impl-async [_ f] (f)))))
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
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
