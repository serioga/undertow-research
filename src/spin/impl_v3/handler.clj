(ns spin.impl-v3.handler
  (:import (clojure.lang Delay Fn IPersistentMap IPersistentVector RT Reduced Sequential)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ResultValue
  "The abstraction for the value of http handler result:

  - new context map
  - handler chain to execute over context map
  - error.
  "

  (get-context
    [value]
    "Returns context map from the result value, or nil.
    Throws exception for error.")

  (prepend-handlers
    [value handlers]
    ""))

;; Persistent map is a context map.
(extend-protocol ResultValue IPersistentMap
  (get-context [m] m)
  (prepend-handlers [_ handlers] handlers))

;; Sequential is a handler seq.
(extend-protocol ResultValue Sequential
  (get-context [_] nil)
  (prepend-handlers [xs handlers] (concat xs handlers)))

;; Function is an 1-item handler seq.
(extend-protocol ResultValue Fn
  (get-context [_] nil)
  (prepend-handlers [f handlers] (cons f handlers)))

;; Exceptions are error values.
(extend-protocol ResultValue Throwable
  (get-context [t] (throw t))
  (prepend-handlers [t _] (cons (fn [_] (throw t)) nil)))

(extend-protocol ResultValue Reduced
  (get-context [x] (get-context (.deref x)))
  (prepend-handlers [x _] (prepend-handlers (.deref x) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerFn

  (io-handler [handler])

  (blocking-handler [handler])

  (async-handler [handler])

  )

(extend-protocol HandlerFn Fn
  (io-handler [f] f)
  (blocking-handler [_] nil)
  (async-handler [_] nil))

(deftype BlockingHandler [f]
  ResultValue
  (get-context [_] nil)
  (prepend-handlers [this handlers] (cons this handlers))
  HandlerFn
  (io-handler [_] nil)
  (blocking-handler [_] f)
  (async-handler [_] nil))

(extend-protocol HandlerFn Throwable
  (io-handler [t] (fn [_] (throw t)))
  (blocking-handler [_] nil)
  (async-handler [_] nil))

;; TODO: Do we need blocking handler abstraction over delay?
(extend-type Delay
  ResultValue
  (get-context [_] nil)
  (prepend-handlers [d handlers] (cons d handlers))
  HandlerFn
  (io-handler
    [d]
    (when (.isRealized d)
      (fn delay-io [_] (.deref d))))
  (blocking-handler
    [d]
    (fn delay-blocking [_] (.deref d)))
  (async-handler
    [_] nil))

(extend-type CompletableFuture
  ResultValue
  (get-context [_] nil)
  (prepend-handlers [ft handlers] (cons ft handlers))
  HandlerFn
  (io-handler
    [ft]
    (when (.isDone ft)
      (fn future-io [_] (.getNow ft nil))))
  (blocking-handler
    [_] nil)
  (async-handler
    [ft]
    (fn future-async [_ callback]
      (.whenComplete ft (reify BiConsumer (accept [_ v e] (callback (or e v)))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerSeq
  (handler-seq [x])
  (prepend-seq [x to])
  (append-vec [x to]))

(extend-protocol HandlerSeq
  Fn
  (handler-seq
    [x] (RT/list x))
  (prepend-seq
    [x to] (cons x to))
  (append-vec
    [f to] (as-> (or to []) handlers
                 (.cons ^IPersistentVector handlers f)))
  Sequential
  (handler-seq
    [xs] xs)
  (prepend-seq
    [xs to] (reduce conj to (seq xs)))
  (append-vec
    [xs to] (reduce #(.cons ^IPersistentVector %1 %2) (or to []) xs)))

(comment
  (prepend-seq identity nil)
  (prepend-seq [identity] nil)
  (append-vec identity nil)
  (append-vec [identity] nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
