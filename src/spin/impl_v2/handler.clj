(ns spin.impl-v2.handler
  (:import (clojure.lang Delay Fn IPersistentMap RT Sequential)
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

  (value-context
    [value]
    "Returns context map from the result value, or nil.
    Throws exception for error.")

  (value-handlers
    [value]
    "Returns handler seq from the result value, or nil.
    Throws exception for error."))

;; Persistent map is a context map.
(extend-protocol ResultValue IPersistentMap
  (value-context,,,, [m] m)
  (value-handlers,,, [_] nil))

;; Sequential is a handler seq.
(extend-protocol ResultValue Sequential
  (value-context,,,, [_] nil)
  (value-handlers,,, [s] s))

;; Function is an 1-item handler seq.
(extend-protocol ResultValue Fn
  (value-context,,,, [_] nil)
  (value-handlers,,, [f] (RT/list f)))

;; Exceptions are error values.
(extend-protocol ResultValue Throwable
  (value-context,,,, [t] (throw t))
  (value-handlers,,, [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HandlerResult
  "The abstraction for http handler result:

  - instant (non-blocking) result value
  - blocking result value
  - async result value
  "

  (instant-result
    [result]
    "When result is instantly available returns function `(fn [] value)`
    which returns value or throws exception.")

  (blocking-result
    [result]
    "When result is available only in blocking call returns function
    `(fn [] value)` which returns value or throws exception. This function
    should not be called on IO thread.")

  (async-result
    [result]
    ;; TODO: review docstring.
    "Returns `nil` for instant result. For async result returns function
    `(fn [f callback] ... (callback value))` which receives 1-arity callback to
    listen for future value completion."))

;; Everything is an instant result by default.
(extend-protocol HandlerResult
  Object
  (instant-result,,,, [o] (fn [] o))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil)
  nil
  (instant-result,,,, [_] (fn [] nil))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil))

;; Exceptions are instant error results.
(extend-protocol HandlerResult Throwable
  (instant-result,,,, [t] (fn throwable-instant [] (throw t)))
  (blocking-result,,, [_] nil)
  (async-result,,,,,, [_] nil))

;; Delay is a blocking result.
(extend-protocol HandlerResult Delay
  (instant-result
    [d]
    (when (.isRealized d)
      (fn delay-instant [] (.deref d))))
  (blocking-result
    [d]
    (fn delay-blocking [] (.deref d)))
  (async-result
    [_] nil))

;; CompletableFuture is an async result.
(extend-protocol HandlerResult CompletableFuture
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
