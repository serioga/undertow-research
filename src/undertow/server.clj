(ns undertow.server
  (:require [undertow.builder :as builder]
            [undertow.handler :as handler])
  (:import (io.undertow Undertow Undertow$Builder)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn- apply-map
  ^Undertow$Builder
  [builder set-fn entries]
  (reduce set-fn builder entries))

(defn setup-builder
  [builder {:keys [ports, handler, buffer-size, io-threads, worker-threads, direct-buffers
                   server-options, socket-options, worker-options]}]
  (-> builder
      (apply-map builder/add-listener ports)
      (apply-map builder/set-server-option server-options)
      (apply-map builder/set-socket-option socket-options)
      (apply-map builder/set-worker-option worker-options)
      (cond->
        handler,,,,,,, (.setHandler (handler/as-http-handler handler))
        buffer-size,,, (.setBufferSize buffer-size)
        io-threads,,,, (.setIoThreads io-threads)
        worker-threads (.setWorkerThreads worker-threads)
        direct-buffers (.setDirectBuffers direct-buffers))))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start
  ^Undertow
  [{:keys [wrap-builder-fn instance-data] :as options}]
  (let [builder-fn (cond-> setup-builder wrap-builder-fn (wrap-builder-fn))
        server (-> (Undertow/builder)
                   (builder-fn options)
                   (builder/build))]
    (->> (doto server .start)
         (assoc instance-data ::undertow))))

(defn stop
  [{::keys [undertow]}]
  (.stop ^Undertow undertow))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,