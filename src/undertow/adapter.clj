(ns undertow.adapter)

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(def ^:dynamic *handler-fn-adapter* identity)

(set-validator! #'*handler-fn-adapter* fn?)

(defn set-handler-fn-adapter
  [f]
  (alter-var-root #'*handler-fn-adapter* (constantly f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
