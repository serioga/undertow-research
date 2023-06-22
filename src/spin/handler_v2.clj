(ns spin.handler-v2
  (:import (clojure.lang Delay)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer)
           (java.util.function Supplier)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol HandlerContext
  "The abstraction for http handler function argument (context) which can be
  valid context or error."
  (apply-handler
    [ctx f]
    "Returns [[HandlerResult]] of `(f ctx). Raises exception if `ctx` represents
    error."))

(defprotocol HandlerResult
  "The abstraction for 1) instant (non-blocking) 2) blocking 3) async result of
  http handler."

  (instant-get
    [result]
    ;; TODO: docstring
    "When context is instantly available returns function `(fn [f] (f ctx))`
    which applies handler to `f` and returns new result or throws exception.")

  (blocking-get
    [result]
    "When context is available only in blocking call returns function `(fn [f]
    (f ctx))` which returns new result or throws exception. The returned
    function should not be called on IO thread.")

  (async-fn
    [result]
    ;; TODO: review docstring.
    "Returns `nil` for instant result. For async result returns function
    `(fn [f callback] ... (callback result))` which receives 1-arity callback to
    listen for future result completion."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Instant result implementations ##

;; ### Any object defaults ###
;;
;; All types represent instant result by default.

(extend-protocol HandlerContext Object
  (apply-handler [o f] (f o)))

;; TODO: extend IPersistentMap instead of Object?
(extend-protocol HandlerResult Object
  (instant-get,,,, [o] (fn object-instant [] o))
  (blocking-get,,, [_] nil)
  (async-fn,,,,,,, [_] nil))

;; ### Exceptions ###
;;
;; Exceptions represents error result.

(extend-protocol HandlerContext Throwable
  (apply-handler [t _] (throw t)))

(extend-protocol HandlerResult Throwable
  (instant-get,,,, [t] (fn throwable-instant [] (throw t)))
  (blocking-get,,, [_] nil)
  (async-fn,,,,,,, [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Blocking/async result implementations ##

;; ### Delay as blocking result ###

(extend-protocol HandlerResult Delay
  (instant-get
    [d]
    (when (.isRealized d)
      (fn delay-instant [] (.deref d))))
  (blocking-get
    [d]
    (fn delay-blocking [] (.deref d)))
  (async-fn
    [_] nil))

;; ### CompletableFuture as async result ###

(extend-type CompletableFuture HandlerResult
  (instant-get
    [fu]
    (when (.isDone fu)
      (fn future-instant [] (.get fu))))
  (blocking-get
    [_] nil)
  (async-fn
    [fu]
    (fn future-async [callback]
      (.whenComplete fu (reify BiConsumer (accept [_ v e] (callback (or e v)))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn handle-chain-fn
  [complete error]
  (letfn
    [(handle-blocking [blocking chain]
       ;; TODO: execute blocking on worker thread
       (handle-chain (blocking) chain))
     (handle-async [async callback]
       (async callback)
       nil)
     (handle-chain [ctx chain]
       (try
         (loop [ctx ctx, chain (seq chain)]
           (if chain
             (if-let [handler (first chain)]
               (let [result (apply-handler ctx handler)
                     reduced-result? (reduced? result)
                     result (if reduced-result? (unreduced result) result)
                     next-chain (when-not reduced-result? (next chain))]
                 (if (some? result)
                   (if-let [instant (instant-get result)]
                     (recur (instant) next-chain)
                     (if-let [blocking (blocking-get result)]
                       (handle-blocking blocking next-chain)
                       (if-let [async (async-fn result)]
                         (handle-async async (fn [ctx'] (handle-chain (or ctx' ctx) next-chain)))
                         (throw (ex-info (str "Cannot handle result: " result) {})))))
                   (recur ctx next-chain)))
               (recur ctx (next chain)))
             (do (apply-handler ctx complete)
                 nil)))
         (catch Throwable t
           (error t)
           nil)))]
    handle-chain))

(defn -handle [ctx chain]
  (let [p (promise)
        complete (partial deliver p)
        handle (handle-chain-fn complete complete)]
    (handle ctx chain)
    (-> (deref p 1000 ::timed-out)
        (apply-handler identity))))

(comment
  (def -handle (handle-chain-fn identity identity))
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] nil)])
  (-handle {} [(fn [ctx] (Exception. "xxx"))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn [ctx] (reduced (assoc ctx :a 1)))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn [ctx] (reduced (delay (assoc ctx :a 1))))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn h1 [ctx] (delay (assoc ctx :a 1)))
               (fn h2 [ctx] (delay (assoc ctx :b 2)))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (delay (assoc ctx :b 2)))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (delay (throw (Exception. "Exception in delay"))))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (CompletableFuture/supplyAsync
                           (reify Supplier (get [_] (assoc ctx :async true)))))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (CompletableFuture/supplyAsync
                           (reify Supplier (get [_] nil))))
               (fn [ctx] (assoc ctx :b 2))])
  (-handle {} [(fn [ctx] (assoc ctx :a 1))
               (fn [ctx] (CompletableFuture/supplyAsync
                           (reify Supplier (get [_] (throw (Exception. "Oops"))))))
               (fn [ctx] (assoc ctx :b 2))])
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
