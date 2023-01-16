(ns ring-lib.middleware.params
  (:require [ring-lib.util.codec :as codec]
            [ring.middleware.params :as params]
            [ring.util.parsing :as parsing]
            [ring.util.request :as request]
            [strojure.zmap.core :as zmap])
  (:import (clojure.lang Associative IDeref MultiFn)
           (java.io ByteArrayInputStream InputStream)
           (java.nio.charset Charset StandardCharsets)
           (org.apache.http.entity ContentType)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn merge*
  ([m] m)
  ([m1 m2]
   (if (and m1 m2)
     (reduce-kv (fn [m k v] (.assoc ^Associative m k v)) m1 m2)
     (or m1 m2))))

(comment
  (merge* {"a" "", "b" "", "c" ""} {"f" "1"})
  (merge* {"f" "1"} {"a" "", "b" "", "c" ""})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn query-params-request-fn
  [{:keys [merge-into] :as opts}]
  (let [form-decode (codec/form-decode-fn opts)]
    (fn [request]
      (when request
        (if-let [query-string (request :query-string)]
          (let [query-params-delay (zmap/delay (form-decode query-string))]
            (-> request
                (assoc :query-params query-params-delay)
                (cond-> merge-into
                        (assoc merge-into (zmap/delay
                                            (merge* (request merge-into)
                                                    (.deref ^IDeref query-params-delay)))))
                (zmap/wrap)))
          request)))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn post-request?
  [request]
  (when request
    (let [method (request :request-method)]
      (or (identical? method :post)
          (identical? method :put)))))

(defn form-urlencoded?
  [request]
  (and (post-request? request)
       (= (request/content-type request)
          "application/x-www-form-urlencoded")))

(defn parse-charset
  [^String s]
  (when (pos? (.indexOf s #=(int \;)))
    (some-> (parsing/find-content-type-charset s)
            (Charset/forName))))

(defn parse-charset
  [^String s]
  (when (pos? (.indexOf s (int \;)))
    (.getCharset (ContentType/parse s))))

(defn content-charset
  [request]
  (or (some-> request :headers (get "content-type")
              (ContentType/parse)
              (.getCharset))
      StandardCharsets/UTF_8))

(defn content-charset
  {:tag Charset}
  [request]
  (or (some-> request :headers (get "content-type") (parse-charset))
      StandardCharsets/UTF_8))

(comment
  (def -q {:query-string "a=&b&c" :request-method :post
           :headers {"content-type" "application/x-www-form-urlencoded"}
           :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (def -q {:query-string "a=&b&c" :request-method :post
           :headers {"content-type" "application/x-www-form-urlencoded; charset=Windows-1251"}
           :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (content-charset -q)
  (ContentType/parse "application/x-www-form-urlencoded")
  (parse-charset "application/x-www-form-urlencoded")
  (parse-charset "application/x-www-form-urlencoded; charset=Windows-1251")
  (.indexOf "application/x-www-form-urlencoded" #=(int \;))
  (.getCharset (ContentType/parse "application/x-www-form-urlencoded; charset=windows-1251"))
  )

(defn form-params-request-fn
  [{:keys [merge-into] :as opts}]
  (let [form-decode (codec/form-decode-fn opts)]
    (fn [request]
      (when request
        (if (form-urlencoded? request)
          (let [form-params-delay (zmap/delay
                                    (some-> ^InputStream (request :body)
                                            (codec/read-input-stream (content-charset request))
                                            (form-decode)))]
            (-> request
                (assoc :form-params form-params-delay)
                (cond-> merge-into
                        (assoc merge-into (zmap/delay
                                            (merge* (request merge-into)
                                                    (.deref ^IDeref form-params-delay)))))
                (zmap/wrap)))
          request)))))

(defmulti ^MultiFn named-params-fn identity)

(.addMethod named-params-fn :query-params (constantly query-params-request-fn))
(.addMethod named-params-fn :form-params (constantly form-params-request-fn))

(defn named-params-fn*
  [k opts]
  ((named-params-fn k) opts))

(defn params-request-fn
  [{:keys [merge-keys] :or {merge-keys [:form-params :query-params]} :as opts}]
  (assert (not-empty merge-keys))
  (let [opts (assoc opts :merge-into :params)]
    (reduce (fn [f k] (let [ff (named-params-fn* k opts)]
                        (fn [request] (ff (f request)))))
            (named-params-fn* (first merge-keys) opts)
            (rest merge-keys))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(do
  (def -qp (query-params-request-fn {:merge-into :params}))
  (def -fp (form-params-request-fn {:merge-into :params}))
  (def -pp (params-request-fn {#_#_:merge-keys [:query-params :form-params]})))

(comment

  (def -q {:query-string "a=&b&c" :request-method :post
           :headers {"content-type" "application/x-www-form-urlencoded"}
           :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (form-urlencoded? -q)

  (-qp nil)
  (-qp {})
  (-fp nil)
  (-fp {})

  (let [request {:query-params {"a" "", "b" "", "c" ""},
                 :form-params {"f" "1"}}
        request (assoc request :params (merge* (request :params) (request :query-params)))
        request (assoc request :params (merge* (request :params) (request :form-params)))]
    request)

  (-> {:query-string "a=&b&c" :request-method :post
       :headers {"content-type" "application/x-www-form-urlencoded"}
       :body (ByteArrayInputStream. (.getBytes "f=1"))}
      (-qp)
      (-fp)
      #_:params
      #_((fn [m] [(:form-params m) (:query-params m)]))
      #_:form-params
      #_:query-params)

  (-> {:query-string "a=&b&c" :request-method :post
       :headers {"content-type" "application/x-www-form-urlencoded"}
       :body (ByteArrayInputStream. (.getBytes "f=1"))}
      (-pp)
      #_:params
      #_((fn [m] [(:form-params m) (:query-params m)]))
      #_:form-params
      #_:query-params)

  (content-charset {:query-string "a=&b&c" :request-method :post
                    :headers {"content-type" "application/x-www-form-urlencoded"}
                    :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (request/urlencoded-form? {:query-string "a=&b&c" :request-method :post
                             :headers {"content-type" "application/x-www-form-urlencoded"}
                             :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (request/character-encoding {})
  (request/character-encoding {:headers {"content-type" "text/plain"}})
  (request/character-encoding {:headers {"content-type" "text/plain; charset=windows-1251"}})
  (params/params-request {:query-string "a=&b&c"
                          :headers {"content-type" "application/x-www-form-urlencoded"}
                          :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (-> {:query-string "a=&b&c"
       :headers {"content-type" "application/x-www-form-urlencoded"}
       :body (ByteArrayInputStream. (.getBytes "f=1"))}
      (params/params-request)
      :params)
  (params/params-request {:query-string "a=&b&c"})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
