(ns user.main-handler
  (:require [undertow-ring.request :as request]
            [undertow.handler :as handler]
            [undertow.websocket.channel :as channel])
  (:import (java.io ByteArrayInputStream File InputStream)
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
(def ^String response-charset "Windows-1252")
(def ^String response-charset "utf-8")
(def ^String response-charset "Windows-1251")

(defn websocket-response
  [request]
  (handler/websocket {:on-message (fn [{:keys [channel text]}]
                                    (if (= "bye" text)
                                      (do (channel/send-text "Bye-bye!" channel nil)
                                          (channel/send-close nil channel nil))
                                      (-> (str (:remote-addr request) ": " text)
                                          (channel/send-text channel nil))))}))

(comment
  (request/websocket? -req)
  (str (:headers -req))
  (reduce-kv (fn [_ k v] [k v]) nil (:headers -req))
  (str -req)
  (File. "./resources/public/static/test.txt")
  )

(defn ring-handler-fn
  ([] (ring-handler-fn "Hello World" nil))
  ([greet] (ring-handler-fn greet nil))
  ([greet {:keys [charset websocket-response-fn]
           :or {charset response-charset
                websocket-response-fn websocket-response}}]
   (fn handler
     ([{:keys [::async?] :as req}]
      #_(throw (ex-info "Oops" {}))
      #_req
      #_(request/sessions-enabled? req)
      (def -req req)
      (let [headers {"x-a" "1"
                     "x-b" "2"
                     #_#_"x-c" [3 4]
                     "content-type" (str "text/plain; charset=" charset)}
            body (seq [greet " [" (.getName (Thread/currentThread)) "]"
                       (if async? " async-ring" " sync-ring")
                       "\n\n"
                       (-> req with-request-body pr-str)])
            body (apply str body)
            body (ByteArrayInputStream. (.getBytes ^String body ^String charset))
            #_#_body (File. "./resources/public/static/test.txt")]
        (println (str "\n" {:body/class (class body)} "\n"))
        (if (and websocket-response-fn (request/websocket? req))
          (websocket-response-fn req)
          #_(future
            (throw (ex-info "oops" {}))
            (Thread/sleep 120000))
          (cond-> {:body body
                   :headers headers
                   :session {:test (or (some-> req :session :test inc) 0)
                             :blink (when-not (-> req :session :blink) true)}
                   #_#_:session (when-not (-> req :session :test)
                                  {:test "Test session value"})
                   #_#_:status 200}
            #_#_(:session req) (assoc-in [:session "test"] "Test session value")))))
     ([req respond raise]
      (future
        (Thread/sleep 4000)
        (try (respond (handler (assoc req ::async? true)))
             (catch Throwable e (raise e))))))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
