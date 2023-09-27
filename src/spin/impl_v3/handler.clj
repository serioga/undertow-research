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

(defprotocol HandlerType

  (handler-type [handler]))

(defprotocol HandlerBlocking

  (invoke-blocking [handler context]))

(defprotocol HandlerAsync

  (invoke-async [handler context callback]))

(defprotocol AsHandlerSeq

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

;; Function is an 1-item handler seq.
(extend-type Fn
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [f handlers] (cons f handlers))
  HandlerType (handler-type [_] nil)
  AsHandlerSeq
  (handler-seq [x] (RT/list x))
  (prepend-seq [x to] (cons x to))
  (append-vec [f to] (as-> (or to []) handlers
                           (.cons ^IPersistentVector handlers f))))

;; Sequential is a handler seq.
(extend-type Sequential
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [xs handlers] (concat xs handlers))
  AsHandlerSeq
  (handler-seq [xs] xs)
  (prepend-seq [xs to] (reduce conj to (seq xs)))
  (append-vec [xs to] (reduce #(.cons ^IPersistentVector %1 %2) (or to []) xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Blocking [f]
  HandlerType (handler-type [_] :blocking-handler)
  HandlerBlocking (invoke-blocking [_ x] (f x))
  ResultContext (result-context [_] nil)
  ResultHandler (result-prepend [this handlers] (cons this handlers)))

;; TODO: Do we need blocking handler abstraction over delay?
(extend-type Delay
  HandlerType (handler-type [_] :blocking-handler)
  HandlerBlocking (invoke-blocking [d _] (.deref d))
  ResultHandler (result-prepend [d handlers] (cons d handlers))
  ResultContext (result-context [d] (when (.isRealized d)
                                      (some-> (.deref d) (result-context)))))

(extend-type CompletableFuture
  HandlerType (handler-type [_] :async-handler)
  HandlerAsync (invoke-async [ft _ callback]
                (.whenComplete ft (reify BiConsumer (accept [_ v e] (callback (or e v)))))
                nil)
  ResultHandler (result-prepend [ft handlers] (cons ft handlers))
  ResultContext (result-context [ft] (when (.isDone ft)
                                       (some-> (.getNow ft nil) (result-context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def -bh (->Blocking identity))
  (invoke-blocking -bh :ok)
  (prepend-seq identity nil)
  (prepend-seq [identity] nil)
  (append-vec identity nil)
  (append-vec [identity] nil)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
