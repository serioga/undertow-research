(ns undertow.server
  (:require [undertow.builder :as builder])
  (:import (io.undertow Undertow)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Decide about instance-data and instance as map.

(defn start
  {:arglists
   '([{:keys [ports, handler,
              buffer-size, io-threads, worker-threads, direct-buffers,
              server-options, socket-options, worker-options,
              wrap-builder-fn, instance-data]}])}
  [{:keys [wrap-builder-fn, instance-data] :as config}]
  (let [builder-fn (cond-> builder/configure wrap-builder-fn (wrap-builder-fn))
        server (-> (Undertow/builder)
                   (builder-fn config)
                   (builder/build))]
    (.start server)
    (-> instance-data (assoc ::undertow server))))

(defn stop
  [{::keys [undertow]}]
  (.stop ^Undertow undertow))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
