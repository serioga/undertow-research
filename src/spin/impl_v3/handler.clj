(ns spin.impl-v3.handler
  (:import (clojure.lang Delay Fn IPersistentMap IPersistentVector RT Reduced Sequential)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ResultContext
  "The abstraction for the value of http handler result:

  - new context map
  - handler chain to execute over context map
  - error.
  "

  (result-context
    [value]
    "Returns context map from the result value, or nil.
    Throws exception for error."))

(defprotocol ResultHandler
  ""

  (result-prepend
    [value handlers]
    ""))

(defprotocol HandlerFunction

  (nio-handler [handler])

  (blocking-handler [handler])

  (async-handler [handler]))

(defprotocol HandlerSeq

  (handler-seq [x])

  (prepend-seq [x to])

  (append-vec [x to]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Persistent map is a context map.
(extend-type IPersistentMap
  ResultContext (result-context [m] m))

;; Exceptions are error values.
(extend-type Throwable
  ResultContext (result-context [t] (throw t)))

;; `Reduced` represents the last result in handler chain.
(extend-type Reduced
  ResultContext (result-context [x] (result-context (.deref x)))
  ResultHandler (result-prepend [x _] (result-prepend (.deref x) nil)))

;; Function is a nio-handler as an 1-item handler seq.
(extend-type Fn
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [f handlers] (cons f handlers))
  HandlerFunction
  (nio-handler [f] f)
  (blocking-handler [_] nil)
  (async-handler [_] nil)
  HandlerSeq
  (handler-seq [x] (RT/list x))
  (prepend-seq [x to] (cons x to))
  (append-vec [f to] (as-> (or to []) handlers
                           (.cons ^IPersistentVector handlers f))))

;; Sequential is a handler seq.
(extend-type Sequential
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [xs handlers] (concat xs handlers))
  HandlerSeq
  (handler-seq [xs] xs)
  (prepend-seq [xs to] (reduce conj to (seq xs)))
  (append-vec [xs to] (reduce #(.cons ^IPersistentVector %1 %2) (or to []) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype BlockingHandler [f]
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [this handlers] (cons this handlers))
  HandlerFunction
  (nio-handler [_] nil)
  (blocking-handler [_] f)
  (async-handler [_] nil))

;; TODO: Do we need blocking handler abstraction over delay?
(extend-type Delay
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [d handlers] (cons d handlers))
  HandlerFunction
  (nio-handler
    [d] (when (.isRealized d)
          (fn delay-nio [_] (.deref d))))
  (blocking-handler
    [d] (fn delay-blocking [_] (.deref d)))
  (async-handler
    [_] nil))

(extend-type CompletableFuture
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [ft handlers] (cons ft handlers))
  HandlerFunction
  (nio-handler
    [ft] (when (.isDone ft)
           (fn future-nio [_] (.getNow ft nil))))
  (blocking-handler
    [_] nil)
  (async-handler
    [ft] (fn future-async [_ callback]
           (.whenComplete ft (reify BiConsumer (accept [_ v e] (callback (or e v)))))
           nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (nio-handler identity)
  (blocking-handler identity)
  (def -bh (->BlockingHandler identity))
  (nio-handler -bh)
  (blocking-handler -bh)
  (prepend-seq identity nil)
  (prepend-seq [identity] nil)
  (append-vec identity nil)
  (append-vec [identity] nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
