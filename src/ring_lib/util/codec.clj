(ns ring-lib.util.codec
  (:require [ring.util.codec :as codec])
  (:import (clojure.lang Associative IReduceInit MapEntry)
           (java.net URLDecoder)
           (java.nio.charset Charset)
           (java.util StringTokenizer)
           (sun.nio.cs UTF_8)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn array-suffix-name
  [^String s]
  (when (.endsWith s "[]")
    (-> s (.substring (unchecked-int 0)
                      (unchecked-subtract-int (.length s) (unchecked-int 2))))))

(comment
  (array-suffix-name "a")
  (array-suffix-name "a[]")
  )

(defn assoc-param-fn
  [{:keys [array-name-fn param-key-fn]
    :or {array-name-fn array-suffix-name}}]
  (fn
    ([] {})
    ([m] m)
    ([^Associative m, e]
     (let [k (key e)]
       (if-let [kk (when array-name-fn (array-name-fn k))]
         (let [kk (cond-> kk param-key-fn (param-key-fn))
               vv (-> (.valAt m kk [])
                      (conj (val e)))]
           (-> m (.assoc kk vv)))
         (if param-key-fn
           (-> m (.assoc (param-key-fn k) (val e)))
           (-> m (.cons e))))))))

(defn url-decode
  "Decodes the supplied www-form-urlencoded string using the specified charset.
  Returns `s` if decoding failed."
  ([s, ^Charset charset]
   (try
     (URLDecoder/decode ^String s charset)
     (catch IllegalArgumentException _
       s))))

(defn form-param-entry
  [^String s, charset]
  (let [i (.indexOf s #=(int \=))]
    (cond
      (pos? i)
      (MapEntry. (url-decode (.substring s 0 i) charset)
                 (url-decode (.substring s (inc i)) charset))
      (zero? i)
      (MapEntry. "" (url-decode (.substring s (inc i)) charset))
      :else
      (MapEntry. (url-decode s charset) ""))))

(defn form-param-reducer
  [s charset]
  (reify IReduceInit
    (reduce [_ f init]
      (let [tok (StringTokenizer. s "&")]
        (loop [result init]
          (if (.hasMoreTokens tok)
            (recur (f result (-> (.nextToken tok) (form-param-entry charset))))
            result))))))

;; TODO: ??? if-not (.contains encoded "=") (form-decode-str encoded encoding)

(defn form-decode-fn
  {:arglists '([{:keys [array-name-fn, param-key-fn]}]
               [rf])}
  [opts]
  (let [rf (if (fn? opts) opts (assoc-param-fn opts))]
    (fn form-decode
      ([s] (form-decode s UTF_8/INSTANCE))
      ([s charset]
       (->> (form-param-reducer s charset)
            (reduce rf (rf))
            (rf))))))

(def form-decode* (form-decode-fn {#_#_:array-name-fn array-suffix-name
                                   #_#_:param-key-fn keyword}))

(comment

  (form-decode* "a=1&b=2&c=3")
  (form-decode* "a[]=1&a[]=2&b=2")

  (form-decode* "a=1&b=2&c=3")
  ;             Execution time mean : 469,363501 ns
  ;    Execution time std-deviation : 49,421289 ns
  ;   Execution time lower quantile : 432,412366 ns ( 2,5%)
  ;   Execution time upper quantile : 528,715599 ns (97,5%)
  (codec/form-decode "a=1&b=2&c=3")
  ;             Execution time mean : 704,243767 ns
  ;    Execution time std-deviation : 9,721997 ns
  ;   Execution time lower quantile : 695,134605 ns ( 2,5%)
  ;   Execution time upper quantile : 719,809398 ns (97,5%)

  (form-decode* "a&b&c")
  (codec/form-decode "a&b&c")

  (codec/form-decode "a=1&a=2&b=2")
  (codec/form-decode "aaa")
  (codec/form-decode "a&b&c")
  (codec/form-decode "a=1&b&c")
  (form-decode* "%2")
  (codec/form-decode "%2")

  (url-decode "abc" UTF_8/INSTANCE)
  (codec/form-decode-str "abc")
  (codec/form-decode-str "abc" "utf-8")
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
