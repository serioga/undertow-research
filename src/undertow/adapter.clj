(ns undertow.adapter
  (:import (io.undertow.server HttpHandler)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn default-handler-fn-adapter
  [f]
  (reify HttpHandler
    (handleRequest [_ exchange] (f exchange))))

(def ^:dynamic *handler-fn-adapter* default-handler-fn-adapter)

(defn- validate-handler-fn-adapter
  [f]
  (or (fn? f)
      (throw (IllegalArgumentException. (str "Requires function for handler-fn-adapter: " f)))))

(set-validator! #'*handler-fn-adapter* validate-handler-fn-adapter)

(defn set-handler-fn-adapter
  [f]
  (alter-var-root #'*handler-fn-adapter* (constantly f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
