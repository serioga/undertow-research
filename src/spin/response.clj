(ns spin.response
  (:refer-clojure :exclude [apply])
  (:require [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)
           (java.util LinkedList)
           (java.util.concurrent CompletableFuture)
           (java.util.function BiConsumer Function)))

(set! *warn-on-reflection* true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Protocols ##

(defprotocol IResponse
  "The abstraction for 1) sync or async response, 2) response value or error."

  (value [response]
    "Returns response value or throws exception if value is pending.")

  (error [response]
    "Returns response error or `nil` if response has no error. By default, any
    `Throwable` represent response error, but generally it can be any type.")

  (async [response]
    "Returns `nil` for sync response. For async response returns function
    `(fn [callback] ... (callback response))` which receives 1-arity callback to
    listen for future response (value or error) completion.")

  (apply [response, f]
    "Returns response with function `f` applied to value."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Sync responses ##

;; ### Any object defaults ###
;;
;; All types represent sync response value by default.

(extend-protocol IResponse Object
  (value [this] this)
  (error [_] nil)
  (async [_] nil)
  (apply [this f] (f this)))

(extend-protocol IResponse nil
  (value [_] nil)
  (error [_] nil)
  (async [_] nil)
  (apply [_ f] (f nil)))

;; ### Exceptions ###
;;
;; Exceptions represents response error:
;; - throws on `value`.
;; - does nothing on `apply`.

(extend-protocol IResponse Throwable
  (value [t] (throw t))
  (error [t] t)
  (async [_] nil)
  (apply [t _] t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ## Async responses ##

(defn async-exception
  "The exception in case of using of incomplete async response value."
  ([] (async-exception {}))
  ([data]
   (ex-info "Async response not available" data)))

;; ### CompletableFuture ###

(extend-type CompletableFuture IResponse
  (value
    [fut]
    (if (.isDone fut)
      (.get fut)
      (throw (async-exception))))
  (error
    [fut]
    (if (.isDone fut)
      (if (.isCompletedExceptionally fut)
        (.get (.exceptionally fut (reify Function (apply [_ t] t))))
        (error (.get fut)))
      (throw (async-exception))))
  (async
    [fut]
    (when-not (.isDone fut)
      (fn future-async [callback]
        (.whenComplete fut (reify BiConsumer (accept [_ v e] (callback (or e v))))))))
  (apply
    [fut f]
    (if (.isDone fut)
      (apply (.get fut) f)
      (.thenApply fut (reify Function (apply [_ x] (apply x f)))))))

;; ### core.async channel ###

(defn- chan-value?
  [^ManyToManyChannel ch]
  (or (not (.isEmpty ^LinkedList (.-puts ch)))
      (some-> (.-buf ch) count pos?)))

(extend-type ManyToManyChannel IResponse
  (value
    [ch]
    (if-some [x (async/poll! ch)]
      x, (throw (async-exception))))
  (error
    [ch]
    (error (value ch)))
  (async
    [ch]
    (when-not (chan-value? ch)
      (fn chan-async [callback]
        (async/go
          (if-some [x (async/<! ch)]
            (callback x)
            (callback (ex-info "Channel closed without response" {})))))))
  (apply
    [ch f]
    (if (chan-value? ch)
      (apply (value ch) f)
      (async/map #(apply % f) [ch]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
