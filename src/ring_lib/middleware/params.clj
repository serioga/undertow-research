(ns ring-lib.middleware.params
  (:require [ring-lib.util.codec :as codec]
            [ring.middleware.params :as params]
            [ring.util.parsing :as parsing]
            [ring.util.request :as req-util]
            [strojure.zmap.core :as zmap])
  (:import (clojure.lang Associative)
           (java.io ByteArrayInputStream InputStream)
           (java.nio.charset Charset StandardCharsets)
           (org.apache.commons.io IOUtils)
           (org.apache.http.entity ContentType)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn merge*
  [m1 m2]
  (if (and m1 m2)
    (merge m1 m2)
    (or m1 m2)))

(defn content-charset
  [request]
  (when-let [ct (-> request :headers (get "content-type"))]
    (.getCharset (ContentType/parse ct))))

(defn content-charset
  {:tag Charset}
  [request]
  (or (some-> request :headers (get "content-type")
              (parsing/find-content-type-charset)
              (Charset/forName))
      StandardCharsets/UTF_8))

(defn assoc-params-fn
  [{:keys [merge-params]}]
  (if merge-params
    (fn [request k v]
      (let [request (assoc request k v)]
        (assoc request :params (zmap/delay (merge* (:params request) (get request k))))))
    assoc))

(defn query-params-request-fn
  [opts]
  (let [form-decode (codec/form-decode-fn opts)
        assoc-params (assoc-params-fn opts)]
    (fn [request]
      (if (.containsKey ^Associative request :query-params)
        request
        (assoc-params (zmap/wrap request) :query-params
                      (zmap/delay (some-> (:query-string request) (form-decode))))))))

(defn form?
  [request]
  (let [method (:request-method request)]
    (and (or (identical? method :post)
             (identical? method :put))
         (req-util/urlencoded-form? request))))

(defn form-params-request-fn
  [opts]
  (let [form-decode (codec/form-decode-fn opts)
        assoc-params (assoc-params-fn opts)]
    (fn [request]
      (cond (.containsKey ^Associative request :form-params)
            request
            (form? request)
            (assoc-params (zmap/wrap request) :form-params
                          (zmap/delay (some-> ^InputStream (:body request)
                                              (IOUtils/toString (content-charset request))
                                              (form-decode))))
            :else request))))

(comment
  (do
    (def -qp (query-params-request-fn {:merge-params true}))
    (def -fp (form-params-request-fn {:merge-params true})))
  (do
    (def -qp (query-params-request-fn {:merge-params false}))
    (def -fp (form-params-request-fn {:merge-params false})))

  (-> {:query-string "a=&b&c" :request-method :post
       :headers {"content-type" "application/x-www-form-urlencoded"}
       :body (ByteArrayInputStream. (.getBytes "f=1"))}
      (-qp)
      (-fp)
      #_:query-params)

  (ByteArrayInputStream. (.getBytes "f=1"))
  (IOUtils/toString (ByteArrayInputStream. (.getBytes "f=1")) StandardCharsets/UTF_8)
  (slurp (ByteArrayInputStream. (.getBytes "f=1")))
  (content-charset {:query-string "a=&b&c" :request-method :post
                    :headers {"content-type" "application/x-www-form-urlencoded"}
                    :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (req-util/urlencoded-form? {:query-string "a=&b&c" :request-method :post
                              :headers {"content-type" "application/x-www-form-urlencoded"}
                              :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (req-util/character-encoding {})
  (req-util/character-encoding {:headers {"content-type" "text/plain"}})
  (req-util/character-encoding {:headers {"content-type" "text/plain; charset=windows-1251"}})
  (params/params-request {:query-string "a=&b&c"
                          #_#_:headers {"content-type" "application/x-www-form-urlencoded"}
                          :body (ByteArrayInputStream. (.getBytes "f=1"))})
  (params/params-request {:query-string "a=&b&c"})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
