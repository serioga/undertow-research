(ns ring-pile.core
  (:refer-clojure :exclude [compile])
  (:require [ring.middleware.params :as params]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.content-type :as content-type]))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn apply-wraps
  [handler fns]
  (reduce (fn [handler wrapper] (wrapper handler))
          handler
          (reverse fns)))

(defn apply-enter
  [handler fns]
  (let [request-fn (reduce (fn [f ff]
                             (fn [request]
                               (f (ff request))))
                           (reverse fns))]
    (fn
      ([request]
       (handler (request-fn request)))
      ([request respond raise]
       (handler (request-fn request) respond raise)))))

(defn apply-leave
  [handler fns]
  (let [response-fn (reduce (fn [f ff]
                              (fn [response request]
                                (f (ff response request) request)))
                            (reverse fns))]
    (fn
      ([request]
       (response-fn (handler request) request))
      ([request respond raise]
       (handler request
                (fn [resp] (respond (response-fn resp request)))
                raise)))))

(defn compile
  [handler {:keys [outer
                   enter
                   leave
                   inner]}]
  (cond-> handler
    (seq inner) (apply-wraps inner)
    (seq leave) (apply-leave leave)
    (seq enter) (apply-enter enter)
    (seq outer) (apply-wraps outer)))

(defn wrap-outer-1 [handler] (fn [request]
                               (println 'wrap-outer-1 :request request)
                               (doto (handler request)
                                 (->> (println 'wrap-outer-1 :response)))))

(defn wrap-outer-2 [handler] (fn [request]
                               (println 'wrap-outer-2 :request request)
                               (doto (handler request)
                                 (->> (println 'wrap-outer-2 :response)))))

(defn wrap-inner-1 [handler] (fn [request]
                               (println 'wrap-inner-1 :request request)
                               (doto (handler request)
                                 (->> (println 'wrap-inner-1 :response)))))

(defn wrap-inner-2 [handler] (fn [request]
                               (println 'wrap-inner-2 :request request)
                               (doto (handler request)
                                 (->> (println 'wrap-inner-2 :response)))))

(defn enter-1 [request]
  (println 'enter-1 :request request)
  request)

(defn enter-2 [request]
  (println 'enter-2 :request request)
  request)

(defn leave-1 [response request]
  (println 'leave-1 :response response)
  (assoc response 'leave-1 request))

(defn leave-2 [response request]
  (println 'leave-2 :response response)
  (assoc response 'leave-2 request))

(def my-handler (compile (fn [req]
                           (println 'my-handler)
                           {:req req})
                         {:outer [wrap-outer-1
                                  wrap-outer-2]
                          :enter [enter-1
                                  enter-2]
                          :leave [leave-1
                                  leave-2]
                          :inner [wrap-inner-1
                                  wrap-inner-2]}))

(comment
  (def -handler (compile identity {:leave [(fn [resp _] resp)
                                           (fn [resp _] resp)
                                           (fn [resp _] resp)]}))
  (def -handler (compile identity {:enter [(fn [req] req)
                                           (fn [req] req)
                                           (fn [req] req)]}))
  (def -handler (compile identity {:enter [params/params-request
                                           keyword-params/keyword-params-request]
                                   :leave [content-type/content-type-response]}))
  (-handler {:uri "/" :query-string "a=1"})
  (-handler {})
  (my-handler ::req)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
