(ns ring-pile.core
  (:refer-clojure :exclude [compile])
  (:require [ring.middleware.params :as params]
            [ring.middleware.keyword-params :as keyword-params]
            [ring.middleware.content-type :as content-type])
  (:import (clojure.lang IFn MultiFn)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn middleware-type
  [obj]
  (or (and (keyword? obj) obj)
      (:type obj)
      (some-> obj meta :type)
      (and (ifn? obj) IFn)
      (class obj)))

(comment
  (do (def as-wrapper nil)
      (def as-enter nil)
      (def as-leave nil))
  )

(defmulti as-wrapper middleware-type)
(defmulti as-enter middleware-type)
(defmulti as-leave middleware-type)
(defmulti requires middleware-type)

(.addMethod ^MultiFn as-wrapper IFn identity)
(.addMethod ^MultiFn as-enter IFn identity)
(.addMethod ^MultiFn as-leave IFn identity)
(.addMethod ^MultiFn requires :default (constantly nil))

(defn apply-wraps
  [handler fns]
  (->> (reverse fns)
       (map as-wrapper)
       (reduce (fn [handler wrapper] (wrapper handler))
               handler)))

(defn apply-enter
  [handler fns]
  (let [request-fn (->> (reverse fns)
                        (map as-enter)
                        (reduce (fn [f ff] (fn [request]
                                             (f (ff request))))))]
    (fn
      ([request]
       (handler (request-fn request)))
      ([request respond raise]
       (handler (request-fn request) respond raise)))))

(defn apply-leave
  [handler fns]
  (let [response-fn (->> (reverse fns)
                         (map as-leave)
                         (reduce (fn [f ff]
                                   (fn [response request]
                                     (f (ff response request) request)))))]
    (fn
      ([request]
       (response-fn (handler request) request))
      ([request respond raise]
       (handler request
                (fn [resp] (respond (response-fn resp request)))
                raise)))))

(defn validate-requires
  [{:keys [ignore-requires] :as config}]
  (let [config-types (-> (select-keys config [:outer :enter :leave :inner])
                         (update-vals (partial map middleware-type)))
        ignore (set ignore-requires)]
    (doseq [[_ group-middlewares], config
            middleware,,,,,,,,,,,, group-middlewares
            [config-key req-types] (requires middleware)
            req-type,,,,,,,,,,,,,, req-types
            :when (not (ignore req-type))]
      (when-not (->> (config-key config-types)
                     (take-while (complement (partial = (middleware-type middleware))))
                     (some (partial = req-type)))
        (throw (ex-info (str "Required middleware missing: " {:middleware (middleware-type middleware)
                                                              :requires req-type})
                        {:middleware-type (middleware-type middleware)
                         :requires (requires middleware)
                         :middleware middleware
                         :missing req-type}))))))

(defn compile
  [handler {:keys [outer
                   enter
                   leave
                   inner] :as config}]
  (validate-requires config)
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

(.addMethod ^MultiFn as-wrapper ::wrap-outer-1 (constantly wrap-outer-1))
(.addMethod ^MultiFn as-wrapper ::wrap-outer-2 (constantly wrap-outer-2))
(.addMethod ^MultiFn as-wrapper ::wrap-inner-1 (constantly wrap-inner-1))
(.addMethod ^MultiFn as-wrapper ::wrap-inner-2 (constantly wrap-inner-2))
(.addMethod ^MultiFn as-enter ::enter-1 (constantly enter-1))
(.addMethod ^MultiFn as-enter ::enter-2 (constantly enter-2))
(.addMethod ^MultiFn as-leave ::leave-1 (constantly leave-1))
(.addMethod ^MultiFn as-leave ::leave-2 (constantly leave-2))

(def my-handler (compile (fn [req]
                           (println 'my-handler)
                           {:req req})
                         {:outer [::wrap-outer-1
                                  ::wrap-outer-2]
                          :enter [::enter-1
                                  ::enter-2]
                          :leave [::leave-1
                                  ::leave-2]
                          :inner [::wrap-inner-1
                                  ::wrap-inner-2]}))

(.addMethod ^MultiFn as-enter ::params/request (constantly params/params-request))
#_(.addMethod ^MultiFn as-enter ::keyword-params/request (constantly keyword-params/keyword-params-request))
(defmethod as-enter ::keyword-params/request
  [this]
  (fn [request] (keyword-params/keyword-params-request request this)))
(.addMethod ^MultiFn requires ::keyword-params/request (constantly {:enter [::params/request]}))
(.addMethod ^MultiFn as-leave ::content-type/response (constantly content-type/content-type-response))

(def params-request (-> params/params-request (with-meta {:type ::params/request})))
(def keyword-params-request (-> keyword-params/keyword-params-request
                                (with-meta {:type ::keyword-params/request})))

(comment
  (def keyword-params {:enter [params/params-request
                               keyword-params/keyword-params-request]})
  (def keyword-params (with-meta keyword-params/keyword-params-request
                                 {:require {:enter [params/params-request]}}))
  keyword-params/keyword-params-request
  {:enter [params/params-request]}
  (def -handler (compile identity {:leave [(fn [resp _] resp)
                                           (fn [resp _] resp)
                                           (fn [resp _] resp)]}))
  (def -handler (compile identity {:enter [(fn [req] req)
                                           (fn [req] req)
                                           (fn [req] req)]}))
  (def -handler (compile identity {:enter [::params/request
                                           {:type ::keyword-params/request :parse-namespaces? true}]
                                   :leave [::content-type/response]
                                   #_#_:ignore-requires #{::params/request}}))
  (def -handler (compile identity {:enter [params-request
                                           keyword-params-request]
                                   :leave [::content-type/response]
                                   #_#_:ignore-requires #{::params/request}}))
  (-handler {:uri "/" :query-string "a=1"})
  (-handler {})
  (my-handler ::req)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
