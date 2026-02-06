(export
  ;; Lifecycle
  webview-create webview-destroy! webview-run! webview-terminate!

  ;; Window
  webview-set-title! webview-set-size!

  ;; Content
  webview-navigate! webview-set-html! webview-eval! webview-init!

  ;; Binding
  webview-bind! webview-bind-async! webview-unbind!

  ;; Dispatch
  webview-dispatch!

  ;; Info
  webview-version

  ;; Constants
  WEBVIEW_HINT_NONE WEBVIEW_HINT_MIN WEBVIEW_HINT_MAX WEBVIEW_HINT_FIXED)

(import :gerbil-webview/libwebview
        :std/text/json
        :std/error)

;;; ---- Error checking ----

(def (check-error! code who)
  (unless (= code WEBVIEW_ERROR_OK)
    (error (string-append (symbol->string who) " failed") code: code)))

;;; ---- JSON helpers ----

(def (json-parse-args req-json)
  (call-with-input-string req-json read-json))

(def (json-serialize value)
  (call-with-output-string
   (lambda (p)
     (parameterize ((current-output-port p))
       (write-json value)))))

;;; ---- Lifecycle ----

(def (webview-create debug: (debug #f))
  (webview_create (if debug 1 0) #f))

(def (webview-destroy! wv)
  ;; Use foreign-release! to trigger the finalizer and mark as released,
  ;; preventing double-free when GC later collects the foreign object.
  (foreign-release! wv))

(def (webview-run! wv)
  (check-error! (webview_run wv) 'webview-run!))

(def (webview-terminate! wv)
  (check-error! (webview_terminate wv) 'webview-terminate!))

;;; ---- Window ----

(def (webview-set-title! wv title)
  (check-error! (webview_set_title wv title) 'webview-set-title!))

(def (webview-set-size! wv width height hint: (hint WEBVIEW_HINT_NONE))
  (check-error! (webview_set_size wv width height hint) 'webview-set-size!))

;;; ---- Content ----

(def (webview-navigate! wv url)
  (check-error! (webview_navigate wv url) 'webview-navigate!))

(def (webview-set-html! wv html)
  (check-error! (webview_set_html wv html) 'webview-set-html!))

(def (webview-eval! wv js)
  (check-error! (webview_eval wv js) 'webview-eval!))

(def (webview-init! wv js)
  (check-error! (webview_init wv js) 'webview-init!))

;;; ---- Binding ----

(def (webview-bind! wv name handler)
  ;; handler: (lambda (args-list) result-value)
  ;; JSON args from JS are parsed, return value is serialized as JSON
  (let ((id (register-bind-handler!
             (lambda (req-id req-json)
               (try
                (let* ((args (json-parse-args req-json))
                       (result (handler args))
                       (result-json (json-serialize result)))
                  (webview_return wv req-id 0 result-json))
                (catch (e)
                  (webview_return wv req-id 1
                                  (json-serialize
                                   (with-output-to-string
                                    (lambda () (display-exception e)))))))))))
    (check-error! (raw_webview_bind wv name id) 'webview-bind!)))

(def (webview-bind-async! wv name handler)
  ;; handler: (lambda (args respond!) ...)
  ;; respond! can be called from any thread
  (let ((id (register-bind-handler!
             (lambda (req-id req-json)
               (let ((args (json-parse-args req-json)))
                 (handler args
                          (lambda (result)
                            (webview_return wv req-id 0
                                            (json-serialize result)))))))))
    (check-error! (raw_webview_bind wv name id) 'webview-bind-async!)))

(def (webview-unbind! wv name)
  (check-error! (webview_unbind wv name) 'webview-unbind!))

;;; ---- Dispatch ----

(def (webview-dispatch! wv thunk)
  (let ((id (register-dispatch-handler! thunk)))
    (check-error! (raw_webview_dispatch wv id) 'webview-dispatch!)))

;;; ---- Info ----

(def (webview-version)
  (list (webview_version_major)
        (webview_version_minor)
        (webview_version_patch)))
