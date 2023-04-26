(ns spin.handler
  (:require [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (clojure.lang Delay)
           (java.util LinkedList)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer Function)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol HandlerResult
  "The abstraction for 1) non-blocking 2) blocking 3) async result of http handler."

  (instant-result-fn 
    [result]
    "When result is available returns function `(fn [] result)` which returns
    the result value or throws exception.")

  (blocking-result-fn
    [result]
    "When result is available only with blocking call returns function
    `(fn [] result)` which returns new computed result or throws exception.
    Returned function should not be called on IO thread.")

  (async-result-fn
    [result]
    ;; TODO: review docstring.
    "Returns `nil` for instant result. For async result returns function
    `(fn [callback] ... (callback result))` which receives 1-arity callback to
    listen for future result completion.")

  (update-result 
    [result, f]
    "Returns new result with function `f` applied to result value."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Instant result implementations ##

;; ### Any object defaults ###
;;
;; All types represent instant result value by default.

(extend-protocol HandlerResult Object
  (instant-result-fn [this] (fn object-result [] this))
  (blocking-result-fn [_],, nil)
  (async-result-fn [_],,,,, nil)
  (update-result [this f],, (f this)))

(extend-protocol HandlerResult nil
  (instant-result-fn [_], (fn nil-result [] nil))
  (blocking-result-fn [_] nil)
  (async-result-fn [_],,, nil)
  (update-result [_ f],,, (f nil)))

;; ### Exceptions ###
;;
;; Exceptions represents error result.

(extend-protocol HandlerResult Throwable
  (instant-result-fn [t], (fn throwable-result [] (throw t)))
  (blocking-result-fn [_] nil)
  (async-result-fn [_],,, nil)
  (update-result [t _],,, (throw t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Blocking/async result implementations ##

;; ### Delay as blocking result ###

(extend-protocol HandlerResult Delay
  (instant-result-fn
    [d]
    (when (.isRealized d)
      (instant-result-fn (.deref d))))
  (blocking-result-fn
    [d]
    (fn delay-blocking-result [] (.deref d)))
  (async-result-fn
    [_] nil)
  (update-result
    [d f]
    (if (.isRealized d)
      (update-result (.deref d) f)
      (delay (update-result (.deref d) f)))))

;; ### CompletableFuture ###

(extend-type CompletableFuture HandlerResult
  (instant-result-fn
    [fut]
    (when (.isDone fut)
      (instant-result-fn (.get fut))))
  (blocking-result-fn
    [_] nil)
  (async-result-fn
    [fut]
    (fn future-async-result [callback]
      (.whenComplete fut (reify BiConsumer (accept [_ v e] (callback (or e v)))))))
  (update-result
    [fut f]
    (if (.isDone fut)
      (update-result (.get fut) f)
      (.thenApply fut (reify Function (apply [_ x] (update-result x f)))))))

;; ### core.async channel ###

(defn- chan-value?
  [^ManyToManyChannel ch]
  (or (not (.isEmpty ^LinkedList (.-puts ch)))
      (some-> (.-buf ch) count pos?)))

(defn- chan-get
  [ch]
  (if-some [x (async/poll! ch)]
    x, (throw (ex-info "Async channel is closed" {}))))

(extend-type ManyToManyChannel HandlerResult
  (instant-result-fn
    [ch]
    (when (chan-value? ch)
      (instant-result-fn (chan-get ch))))
  (blocking-result-fn
    [_] nil)
  (async-result-fn
    [ch]
    (fn chan-async-result [callback]
      (async/go
        (if-some [x (async/<! ch)]
          (callback x)
          (callback (ex-info "Cannot get result from closed channel" {}))))))
  (update-result
    [ch f]
    (if (chan-value? ch)
      (update-result (chan-get ch) f)
      (async/map #(update-result % f) [ch]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
