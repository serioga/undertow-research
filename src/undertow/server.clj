(ns undertow.server
  (:require [undertow.api.builder :as builder]
            [undertow.api.types :as types])
  (:import (io.undertow Undertow Undertow$Builder UndertowOptions)
           (org.xnio Options)))

(set! *warn-on-reflection* true)

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn start
  ;; TODO: Document configuration map
  "Starts Undertow server given instance, builder or configuration map.

  "
  {:arglists '([{:keys [ports, handler,
                        buffer-size, io-threads, worker-threads, direct-buffers,
                        server-options, socket-options, worker-options]
                 ::keys
                 [fn-as-handler, wrap-builder-fn]}])}
  [config-or-server]
  (types/start-server config-or-server))

(defn stop
  "Stops server instance, returns nil. The instance can be an instance of
  `Undertow` or map with `::undertow` key."
  [instance]
  (types/stop-server instance))

(defmethod types/start-server :default
  [{::keys [fn-as-handler, wrap-builder-fn] :as config}]
  (binding [types/*fn-as-handler* (or fn-as-handler types/*fn-as-handler*)]
    (let [builder-fn (cond-> builder/configure wrap-builder-fn (wrap-builder-fn))
          server (-> (Undertow/builder)
                     (builder-fn config)
                     (builder/build))]
      (.start server)
      ;; TODO: Decide about instance as map.
      {::undertow server :type ::instance})))

(defmethod types/start-server ::instance
  [{::keys [undertow]}]
  (types/start-server undertow))

(defmethod types/stop-server ::instance
  [{::keys [undertow]}]
  (types/stop-server undertow))

(defmethod types/start-server Undertow$Builder
  [builder]
  (start {::wrap-builder-fn (constantly builder)}))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn set-fn-as-handler
  [f]
  (alter-var-root #'types/*fn-as-handler* (constantly f)))

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(defn define-option
  ([alias option] (define-option alias option identity))
  ([alias option coerce-fn]
   (defmethod types/as-option alias
     [_ v]
     [option (coerce-fn v)])))

;;;; Options

;;; XNIO workers

;; The number of IO threads to create. IO threads perform non blocking tasks,
;; and should never perform blocking operations because they are responsible for
;; multiple connections, so while the operation is blocking other connections
;; will essentially hang. Two IO threads per CPU core is a reasonable default.
(define-option :xnio/worker-io-threads
               Options/WORKER_IO_THREADS int)

;; The number of threads in the workers blocking task thread pool. When
;; performing blocking operations such as Servlet requests threads from this
;; pool will be used. In general it is hard to give a reasonable default for
;; this, as it depends on the server workload. Generally this should be
;; reasonably high, around 10 per CPU core.
(define-option :xnio/worker-task-core-threads
               Options/WORKER_TASK_CORE_THREADS int)

;;; Common Listener Options

;; The maximum size of a HTTP header block, in bytes. If a client sends more
;; data that this as part of the request header then the connection will be
;; closed. Defaults to 50k.
(define-option :undertow/max-header-size
               UndertowOptions/MAX_HEADER_SIZE int)

;; The default maximum size of a request entity. If entity body is larger than
;; this limit then a java.io.IOException will be thrown at some point when
;; reading the request (on the first read for fixed length requests, when too
;; much data has been read for chunked requests). This value is only the default
;; size, it is possible for a handler to override this for an individual request
;; by calling io.undertow.server.HttpServerExchange.setMaxEntitySize(long size).
;; Defaults to unlimited.
(define-option :undertow/max-entity-size
               UndertowOptions/MAX_ENTITY_SIZE long)

;; The default max entity size when using the Multipart parser. This will
;; generally be larger than MAX_ENTITY_SIZE. Having a separate setting for this
;; allows for large files to be uploaded, while limiting the size of other
;; requests.
(define-option :undertow/multipart-max-entity-size
               UndertowOptions/MULTIPART_MAX_ENTITY_SIZE long)

;; The maximum number of query parameters that are permitted in a request. If a
;; client sends more than this number the connection will be closed. This limit
;; is necessary to protect against hash based denial of service attacks.
;; Defaults to 1000.
(define-option :undertow/max-parameters
               UndertowOptions/MAX_PARAMETERS int)

;; The maximum number of headers that are permitted in a request. If a client
;; sends more than this number the connection will be closed. This limit is
;; necessary to protect against hash based denial of service attacks. Defaults
;; to 200.
(define-option :undertow/max-headers
               UndertowOptions/MAX_HEADERS int)

;; The maximum number of cookies that are permitted in a request. If a client
;; sends more than this number the connection will be closed. This limit is
;; necessary to protect against hash based denial of service attacks. Defaults
;; to 200.
(define-option :undertow/max-cookies
               UndertowOptions/MAX_COOKIES int)

;; The charset to use to decode the URL and query parameters. Defaults to UTF-8.
(define-option :undertow/url-charset
               UndertowOptions/URL_CHARSET)

;; Determines if the listener will decode the URL and query parameters, or
;; simply pass it through to the handler chain as is. If this is set url encoded
;; characters will be decoded to the charset specified in URL_CHARSET. Defaults
;; to true.
(define-option :undertow/decode-url
               UndertowOptions/DECODE_URL boolean)

;; If a request comes in with encoded / characters (i.e. %2F), will these be
;; decoded. This can cause security problems
;; (link:http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2007-0450) if a front
;; end proxy does not perform the same decoding, and as a result this is
;; disabled by default.
(define-option :undertow/allow-encoded-slash
               UndertowOptions/ALLOW_ENCODED_SLASH boolean)

;; If this is true then Undertow will allow non-escaped equals characters in
;; unquoted cookie values. Unquoted cookie values may not contain equals
;; characters. If present the value ends before the equals sign. The remainder
;; of the cookie value will be dropped. Defaults to false.
(define-option :undertow/allow-equals-in-cookie-value
               UndertowOptions/ALLOW_EQUALS_IN_COOKIE_VALUE boolean)

;; If the server should add a HTTP Date header to all response entities which do
;; not already have one. The server sets the header right before writing the
;; response, if none was set by a handler before. Unlike the DateHandler it will
;; not overwrite the header. The current date string is cached, and is updated
;; every second. Defaults to true.
(define-option :undertow/always-set-date
               UndertowOptions/ALWAYS_SET_DATE boolean)

;; If a HTTP Connection: keep-alive header should always be set, even for
;; HTTP/1.1 requests that are persistent by default. Even though the spec does
;; not require this header to always be sent it seems safer to always send it.
;; If you are writing some kind of super high performance application and are
;; worried about the extra data being sent over the wire this option allows you
;; to turn it off. Defaults to true.
(define-option :undertow/always-set-keep-alive
               UndertowOptions/ALWAYS_SET_KEEP_ALIVE boolean)

;; The maximum size of a request that can be saved in bytes. Requests are
;; buffered in a few situations, the main ones being SSL renegotiation and
;; saving post data when using FORM based auth. Defaults to 16,384 bytes.
(define-option :undertow/max-buffered-request-size
               UndertowOptions/MAX_BUFFERED_REQUEST_SIZE int)

;; If the server should record the start time of a HTTP request. This is
;; necessary if you wish to log or otherwise use the total request time, however
;; has a slight performance impact, as it means that System.nanoTime() must be
;; called for each request. Defaults to false.
(define-option :undertow/record-request-start-time
               UndertowOptions/RECORD_REQUEST_START_TIME boolean)

;; The amount of time a connection can be idle for before it is timed out. An
;; idle connection is a connection that has had no data transfer in the idle
;; timeout period. Note that this is a fairly coarse grained approach, and small
;; values will cause problems for requests with a long processing time.
(define-option :undertow/idle-timeout
               UndertowOptions/IDLE_TIMEOUT int)

;; How long a request can spend in the parsing phase before it is timed out.
;; This timer is started when the first bytes of a request are read, and
;; finishes once all the headers have been parsed.
(define-option :undertow/request-parse-timeout
               UndertowOptions/REQUEST_PARSE_TIMEOUT int)

;; The amount of time a connection can sit idle without processing a request,
;; before it is closed by the server.
(define-option :undertow/no-request-timeout
               UndertowOptions/NO_REQUEST_TIMEOUT int)

;;; HTTP Listener

;; If this is true then the connection can be processed as a HTTP/2 prior
;; knowledge connection. If a HTTP/2 client connects directly to the listener
;; with a HTTP/2 connection preface then the HTTP/2 protocol will be used
;; instead of HTTP/1.1.
(define-option :undertow/enable-http2
               UndertowOptions/ENABLE_HTTP2 boolean)

;;; HTTP2 Listener

;; The size of the header table that is used for compression. Increasing this
;; will use more memory per connection, but potentially decrease the amount of
;; data that is sent over the wire. Defaults to 4096.
(define-option :undertow/http2-settings-header-table-size
               UndertowOptions/HTTP2_SETTINGS_HEADER_TABLE_SIZE int)

;; If server push is enabled for this connection.
(define-option :undertow/http2-settings-enable-push
               UndertowOptions/HTTP2_SETTINGS_ENABLE_PUSH boolean)

;; The maximum number of streams a client is allowed to have open at any one
;; time.
(define-option :undertow/http2-settings-max-concurrent-streams
               UndertowOptions/HTTP2_SETTINGS_MAX_CONCURRENT_STREAMS int)

;; The initial flow control window size.
(define-option :undertow/http2-settings-initial-window-size
               UndertowOptions/HTTP2_SETTINGS_INITIAL_WINDOW_SIZE int)

;; The maximum frame size.
(define-option :undertow/http2-settings-max-frame-size
               UndertowOptions/HTTP2_SETTINGS_MAX_FRAME_SIZE int)

(comment
  (types/as-option :undertow/enable-http2 true)
  (types/as-option :undertow/enable-http2 nil)
  (types/as-option UndertowOptions/ENABLE_HTTP2 true)
  (types/as-option :xnio/worker-io-threads 4)
  )

;;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
