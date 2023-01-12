(ns ring-lib.util.codec
  (:require [ring.util.codec :as codec])
  (:import (clojure.lang Associative IReduceInit MapEntry)
           (java.net URLDecoder)
           (java.nio.charset Charset)
           (java.util Iterator StringTokenizer)
           (sun.nio.cs UTF_8)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn assoc-vec
  ([] {})
  ([m] m)
  ([^Associative m, e]
   (let [k (key e)]
     (if-let [cur (.valAt m k)]
       (.assoc m k (if (vector? cur)
                     (conj cur (val e))
                     [cur (val e)]))
       (.cons m e)))))

(defn assoc-first
  ([] {})
  ([m] m)
  ([^Associative m, e]
   (if (.containsKey m (key e))
     m
     (.cons m e))))

(defn assoc-last
  ([] {})
  ([m] m)
  ([^Associative m, e]
   (.cons m e)))

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

(defn form-params-iterator
  ([s] (form-params-iterator s UTF_8/INSTANCE))
  ([s charset]
   (let [tok (StringTokenizer. s "&")]
     (reify
       Iterator
       (hasNext [_] (.hasMoreTokens tok))
       (next [_] (-> (.nextToken tok) (form-param-entry charset)))
       IReduceInit
       (reduce [_ f init]
         (let [tok (StringTokenizer. s "&")]
           (loop [result init]
             (if (.hasMoreTokens tok)
               (recur (f result (-> (.nextToken tok) (form-param-entry charset))))
               result))))))))

;; TODO: ??? if-not (.contains encoded "=") (form-decode-str encoded encoding)

(defn form-decode
  ([s] (form-decode s UTF_8/INSTANCE assoc-vec))
  ([s charset] (form-decode s charset assoc-vec))
  ([s charset rf]
   (->> (form-params-iterator s charset)
        (reduce rf (rf))
        (rf))))

(defn form-decode-fn
  [rf]
  (fn form-params*
    ([s] (form-params* s UTF_8/INSTANCE))
    ([s charset]
     (->> (form-params-iterator s charset)
          (reduce rf (rf))
          (rf)))))

(def form-decode-vec (form-decode-fn assoc-vec))
(def form-decode-first (form-decode-fn assoc-first))
(def form-decode-last (form-decode-fn assoc-last))

(comment
  (reduce assoc-vec (assoc-vec)
          (form-params-iterator "a=1&b=2&c=3" UTF_8/INSTANCE))

  (form-decode-vec "a=1&b=2&c=3")
  (form-decode-first "a=1&b=2&c=3")
  (form-decode-last "a=1&b=2&c=3")

  (form-decode-vec "a=1&a=2&b=2")
  (form-decode-first "a=1&a=2&b=2")
  (form-decode-last "a=1&a=2&b=2")

  (form-decode "a=1&a=2&b=2" UTF_8/INSTANCE assoc-vec)
  (form-decode "a=1&a=2&b=2" UTF_8/INSTANCE assoc-first)
  (form-decode "a=1&a=2&b=2" UTF_8/INSTANCE assoc-last)

  (form-decode-vec "a=1&a=2&b=2")
  ;             Execution time mean : 510,763304 ns
  ;    Execution time std-deviation : 47,300007 ns
  ;   Execution time lower quantile : 474,152750 ns ( 2,5%)
  ;   Execution time upper quantile : 572,025199 ns (97,5%)
  (codec/form-decode "a=1&a=2&b=2")
  ;             Execution time mean : 704,243767 ns
  ;    Execution time std-deviation : 9,721997 ns
  ;   Execution time lower quantile : 695,134605 ns ( 2,5%)
  ;   Execution time upper quantile : 719,809398 ns (97,5%)

  (form-decode "aaa")
  (form-decode "%2")
  (codec/form-decode "a&b&c")

  (reduce assoc-vec (assoc-vec)
          (form-params-iterator "a=1&a=2&b=2" UTF_8/INSTANCE))

  (iterator-seq (form-params-iterator "a=1&a=2&b=2" UTF_8/INSTANCE))

  (codec/form-decode "a=1&a=2&b=2")
  (codec/form-decode "aaa")
  (codec/form-decode "a&b&c")
  (codec/form-decode "a=1&b&c")
  (codec/form-decode "%2")

  (codec/form-decode-str "abc")
  (codec/form-decode-str "abc" "utf-8")
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
