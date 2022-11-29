(ns undertow.adapter)

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:dynamic *handler-fn-adapter* identity)

(defn- validate-handler-fn-adapter
  [f]
  (or (fn? f)
      (throw (IllegalArgumentException. (str "Requires function for handler-fn-adapter: " f)))))

(set-validator! #'*handler-fn-adapter* validate-handler-fn-adapter)

(defn set-handler-fn-adapter
  [f]
  (alter-var-root #'*handler-fn-adapter* (constantly f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
