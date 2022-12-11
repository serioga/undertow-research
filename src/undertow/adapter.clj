(ns undertow.adapter
  (:import (clojure.lang AFn)
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
  (or (instance? AFn f)
      (throw (IllegalArgumentException. (str "Requires function for handler fn-adapter: " f)))))

(set-validator! #'*fn-as-handler* validate-fn-as-handler)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn set-fn-as-handler
  [f]
  (alter-var-root #'*fn-as-handler* (constantly f)))

(defmacro with-fn-as-handler
  [f expr]
  `(binding [*fn-as-handler* ~f]
     ~expr))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn simple-fn-as-handler
  [f]
  (reify HttpHandler
    (handleRequest [_ exchange] (f exchange))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

