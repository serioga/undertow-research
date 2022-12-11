(ns undertow.server
  (:require [undertow.api.builder :as builder]
            [undertow.api.types :as types])
  (:import (io.undertow Undertow UndertowOptions)
           (org.xnio Options)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; TODO: Complete set of known undertow options
(types/define-option :undertow/max-header-size UndertowOptions/MAX_HEADER_SIZE)
(types/define-option :undertow/max-entity-size UndertowOptions/MAX_ENTITY_SIZE)
(types/define-option :undertow/multipart-max-entity-size UndertowOptions/MULTIPART_MAX_ENTITY_SIZE)
(types/define-option :undertow/max-parameters UndertowOptions/MAX_PARAMETERS)
(types/define-option :undertow/max-headers UndertowOptions/MAX_HEADERS)
(types/define-option :undertow/enable-http2 UndertowOptions/ENABLE_HTTP2)

(types/define-option :xnio/worker-io-threads Options/WORKER_IO_THREADS int)

(comment
  (types/as-option :undertow/enable-http2 true)
  (types/as-option UndertowOptions/ENABLE_HTTP2 true)
  (types/as-option :xnio/worker-io-threads 4)
  )

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
