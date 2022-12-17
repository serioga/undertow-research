(ns undertow.adapter
  (:require [undertow.api.types :as types])
  (:import (clojure.lang Fn MultiFn)
           (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:dynamic *fn-as-handler*
  (fn [f]
    (throw (ex-info (str "Cannot use function as undertow handler: " f "\n"
                         "Define coercion using `set-fn-as-handler`.")
                    {}))))

(defn- validate-fn-as-handler
  [f]
  (or (instance? Fn f)
      (instance? MultiFn f)
      (throw (IllegalArgumentException. (str "Requires function for handler fn-adapter: " f)))))

(set-validator! #'*fn-as-handler* validate-fn-as-handler)

(.addMethod ^MultiFn types/as-handler Fn,,,,, #'*fn-as-handler*)
(.addMethod ^MultiFn types/as-handler MultiFn #'*fn-as-handler*)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn set-fn-as-handler
  [f]
  (alter-var-root #'*fn-as-handler* (constantly f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-fn-as-handler
  [f]
  (reify HttpHandler
    (handleRequest [_ exchange] (f exchange))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
