(ns spin.response
  (:require [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (clojure.lang Delay)
           (java.util LinkedList)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer Function)))

(set! *warn-on-reflection* true)

;; TODO: Review usage of `response` term?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol HandlerResult
  "The abstraction for 1) non-blocking 2) blocking 3) async result of http handler."

  (instant [result]
    "When response is available without blocking returns function
    `(fn [] response)` which returns response or throws exception.")

  (blocking [result]
    "When response is available only with blocking call returns function
    `(fn [] response)` which returns response or throws exception. Returned
    function should not be called on IO thread.")

  (async [result]
    ;; TODO: review docstring.
    "Returns `nil` for sync response. For async response returns function
    `(fn [callback] ... (callback response))` which receives 1-arity callback to
    listen for future response (value or error) completion.")

  (fmap [result, f]
    "Returns new result with function `f` applied to response."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Sync responses ##

;; ### Any object defaults ###
;;
;; All types represent sync response value by default.

(extend-protocol HandlerResult Object
  (instant [this] (fn object-instant [] this))
  (blocking [_] nil)
  (async [_] nil)
  (fmap [this f] (f this)))

(extend-protocol HandlerResult nil
  (instant [_] (fn nil-instant [] nil))
  (blocking [_] nil)
  (async [_] nil)
  (fmap [_ f] (f nil)))

;; ### Exceptions ###
;;
;; Exceptions represents response error:
;; - throws on `value`.
;; - does nothing on `fmap`.

(extend-protocol HandlerResult Throwable
  (instant [t] (fn throwable-instant [] (throw t)))
  (blocking [_] nil)
  (async [_] nil)
  (fmap [t _] t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Blocking/async responses ##

;; ### Delay as blocking result ###

(extend-protocol HandlerResult Delay
  (instant
    [d]
    (when (.isRealized d)
      (fn delay-instant [] (.deref d))))
  (blocking
    [d]
    (fn delay-blocking [] (.deref d)))
  (async
    [_] nil)
  (fmap
    [d f]
    (delay (f (.deref d)))))

;; ### CompletableFuture ###

(extend-type CompletableFuture HandlerResult
  (instant
    [fut]
    (when (.isDone fut)
      (fn future-instant [] (.get fut))))
  (blocking
    [_] nil)
  (async
    [fut]
    (fn future-async [callback]
      (.whenComplete fut (reify BiConsumer (accept [_ v e] (callback (or e v)))))))
  (fmap
    [fut f]
    (if (.isDone fut)
      (fmap (.get fut) f)
      (.thenApply fut (reify Function (apply [_ x] (fmap x f)))))))

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
  (instant
    [ch]
    (when (chan-value? ch)
      (fn chan-instant [] (chan-get ch))))
  (blocking
    [_] nil)
  (async
    [ch]
    (fn chan-async [callback]
      (async/go
        (if-some [x (async/<! ch)]
          (callback x)
          (callback (ex-info "Channel closed without response" {}))))))
  (fmap
    [ch f]
    (if (chan-value? ch)
      (fmap (chan-get ch) f)
      (async/map #(fmap % f) [ch]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
