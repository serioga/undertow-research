(ns user.main-handler
  (:require [ring.adapter.undertow.websocket :as ws])
  (:import (java.io ByteArrayInputStream InputStream)
           (org.apache.commons.io IOUtils)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#_(require '[user.server-adapter :reload true])
#_(require '[user.server-immutant :reload true])

(comment
  (require '[user.main_server :reload true]))

(defn with-request-body [{:keys [body] :as req}]
  (cond-> req
    (instance? InputStream body)
    (assoc :body (IOUtils/toString ^InputStream body ^String (:content-encoding req "UTF-8")))))

(def ^String response-charset "ISO-8859-1")
(def ^String response-charset "utf-8")
(def ^String response-charset "Windows-1252")
(def ^String response-charset "Windows-1251")

(defn ring-handler-fn
  ([] (ring-handler-fn "Hello World" nil))
  ([greet] (ring-handler-fn greet nil))
  ([greet {:keys [charset]
           :or {charset response-charset}}]
   (fn handler
     ([{:keys [::async?] :as req}]
      #_(throw (ex-info "Oops" {}))
      #_ req
      (let [headers {"x-a" "1"
             "x-b" "2"
             #_#_"x-c" [3 4]
             "content-type" (str "text/plain; charset=" charset)}
            body (seq [greet " [" (.getName (Thread/currentThread)) "]"
                       (if async? " async-ring" " sync-ring")
                       "\n\n"
                       (-> req with-request-body)])
            body (apply str body)
            body (ByteArrayInputStream. (.getBytes ^String body ^String charset))]
        (if (req :websocket?)
          {:headers headers
           :status 400
           :undertow/websocket {:on-message (fn [{:keys [channel data]}]
                                              (ws/send "OK" channel)
                                              (ws/send "OK!" channel))}}
          (cond-> {:body body
                   :headers headers
                   :session {"test" "Test session value"}
                   #_#_:status 200}
            #_#_(:session req) (assoc-in [:session "test"] "Test session value")))))
     ([req respond raise]
      (try (respond (handler (assoc req ::async? true)))
           (catch Throwable e (raise e)))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
