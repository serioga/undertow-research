(ns ring-lib.util.codec
  (:require [ring.util.codec :as codec])
  (:import (clojure.lang Associative)
           (java.io ByteArrayInputStream ByteArrayOutputStream InputStream InputStreamReader StreamTokenizer)
           (java.net URLDecoder)
           (java.nio.charset Charset StandardCharsets)
           (java.util StringTokenizer)))

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

(defn assoc-param-rf
  [{:keys [array-name-fn param-key-fn]
    :or {array-name-fn array-suffix-name}}]
  (fn
    ([] {})
    ([m] m)
    ([^Associative m k v]
     (if-let [kk (when array-name-fn (array-name-fn k))]
       (let [kk (cond-> kk param-key-fn (param-key-fn))]
         (.assoc m kk (conj (.valAt m kk []) v)))
       (.assoc m (cond-> k param-key-fn (param-key-fn)) v)))))

(defn url-decode
  "Decodes the supplied www-form-urlencoded string using UTF-8 charset.
  Returns `s` if decoding failed."
  [^String s]
  (try
    (URLDecoder/decode s StandardCharsets/UTF_8)
    (catch IllegalArgumentException _
      s)))

(defn read-input-stream
  [^InputStream body, ^Charset charset]
  ;; Works since Java 9!
  (-> (.readAllBytes body)
      (String. charset)))

(defn read-input-stream
  [^InputStream body, ^Charset charset]
  (let [output (ByteArrayOutputStream.)]
    (loop [x (.read body)]
      (when-not (neg? x)
        (.write output (unchecked-int x))
        (recur (.read body))))
    (String. (.toString output charset))))

(comment
  (String. (.readAllBytes ^InputStream (ByteArrayInputStream. (.getBytes "f=1")))
           StandardCharsets/UTF_8)
  (read-input-stream (ByteArrayInputStream. (.getBytes "f=1")) StandardCharsets/UTF_8)
  )

;; TODO: ??? if-not (.contains encoded "=") (form-decode-str encoded encoding)

(defn reduce-param-token
  [rf result ^String s]
  (let [i (.indexOf s #=(int \=))]
    (cond
      (pos? i)
      (-> result (rf (url-decode (.substring s 0 i))
                     (url-decode (.substring s (inc i)))))
      (zero? i)
      (-> result (rf "" (url-decode (.substring s (inc i)))))
      :else
      (-> result (rf (url-decode s) "")))))

(defn form-decode-fn
  {:arglists '([{:keys [array-name-fn, param-key-fn]}]
               [rf])}
  [opts]
  (let [rf (if (fn? opts) opts (assoc-param-rf opts))]
    (fn form-decode
      [s]
      (let [tok (StringTokenizer. s "&")]
        (loop [result (rf)]
          (if (.hasMoreTokens tok)
            (recur (reduce-param-token rf result (.nextToken tok)))
            (rf result)))))))

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

  (codec/form-decode-str "abc")
  (codec/form-decode-str "abc" "utf-8")

  (form-decode* (ByteArrayInputStream. (.getBytes "a=1&b=2")))

  (let [body (ByteArrayInputStream. (.getBytes "a=1&b=2"))
        tokenizer (StreamTokenizer. (StringReader. (InputStreamReader. body StandardCharsets/UTF_8)))]
    tokenizer)

  (let [body (ByteArrayInputStream. (.getBytes "a=1&b=2"))
        tokenizer (StreamTokenizer. (InputStreamReader. body StandardCharsets/UTF_8))]
    (doto tokenizer
      (.wordChars 0 256)
      (.whitespaceChars #=(int \&) #=(int \&)))
    (loop [t (.nextToken tokenizer)]
      (when-not (= t StreamTokenizer/TT_EOF)
        #_(println (.-sval tokenizer))
        (recur (.nextToken tokenizer)))))

  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
