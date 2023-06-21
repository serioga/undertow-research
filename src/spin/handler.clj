(ns spin.handler
  (:require [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (clojure.lang Delay IDeref)
           (java.util LinkedList)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer Function)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol HandlerResult
  "The abstraction for 1) instant (non-blocking) 2) blocking 3) async result of
  http handler."

  (instant-result
    ^IDeref [result]
    "When result is available returns `IDeref` which returns the result value or
    throws exception on `deref`.")

  (blocking-result
    ^IDeref [result]
    "When result is available only with blocking call returns `IDeref` which
    computes new result on `deref`. The `deref` should not be called on IO
    thread.")

  (async-result
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
;; All types represent instant result by default.

(deftype InstantResult [x] IDeref (deref [_] x))

(extend-protocol HandlerResult Object
  (instant-result,,, [this] (->InstantResult this))
  (blocking-result,,,,, [_] nil)
  (async-result,,,,,,,, [_] nil)
  (update-result,, [this f] (f this)))

(extend-protocol HandlerResult nil
  (instant-result,,, [_] (->InstantResult nil))
  (blocking-result,, [_] nil)
  (async-result,,,,, [_] nil)
  (update-result,, [_ f] (f nil)))

;; ### Exceptions ###
;;
;; Exceptions represents error result.

(deftype InstantThrowable [t] IDeref (deref [_] (throw t)))

(extend-protocol HandlerResult Throwable
  (instant-result,,, [t] (->InstantThrowable t))
  (blocking-result,, [_] nil)
  (async-result,,,,, [_] nil)
  (update-result,, [t _] (throw t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Blocking/async result implementations ##

;; ### Delay as blocking result ###

(extend-protocol HandlerResult Delay
  (instant-result
    [d]
    (when (.isRealized d)
      (instant-result (.deref d))))
  (blocking-result
    [d] d)
  (async-result
    [_] nil)
  (update-result
    [d f]
    (if (.isRealized d)
      (update-result (.deref d) f)
      (delay (update-result (.deref d) f)))))

;; ### CompletableFuture ###

(extend-type CompletableFuture HandlerResult
  (instant-result
    [fut]
    (when (.isDone fut)
      (instant-result (.get fut))))
  (blocking-result
    [_] nil)
  (async-result
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
  (instant-result
    [ch]
    (when (chan-value? ch)
      (instant-result (chan-get ch))))
  (blocking-result
    [_] nil)
  (async-result
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

(defn handle-chain
  [ctx xs]
  (loop [ctx ctx, xs (seq xs)]
    (let [ctx (if-let [result (instant-result ctx)]
                (.deref result)
                ;; TODO: deref blocking on worker thread
                (if-let [blocking (blocking-result ctx)]
                  (.deref blocking)
                  ;; TODO: async handler
                  (if-let [async (async-result ctx)]
                    {:async async}
                    :wtf)))]
      (if xs
        (if-let [handler (first xs)]
          (recur (handler ctx) (next xs))
          (recur ctx (next xs)))
        ctx))))

(comment
  (handle-chain {} [(fn [ctx] (assoc ctx :a 1))
                    (fn [ctx] (assoc ctx :b 2))])
  (handle-chain {} [(fn [ctx] (delay (assoc ctx :a 1)))
                    (fn [ctx] (assoc ctx :b 2))])
  (handle-chain {} [(fn [ctx] (assoc ctx :a 1))
                    (fn [ctx] (delay (assoc ctx :b 2)))])
  )
