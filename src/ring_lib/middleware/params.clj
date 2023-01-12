(ns ring-lib.middleware.params
  (:require [ring.middleware.params :as params])
  (:import (java.io ByteArrayInputStream)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(comment
  (params/params-request {:query-string "a=q1&a=q2"
                          :headers {"content-type" "application/x-www-form-urlencoded"}
                          :body (ByteArrayInputStream. (.getBytes "a=f1&a=f2" "utf-8"))})
  (params/params-request {:query-string "a=&b&c"})
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
