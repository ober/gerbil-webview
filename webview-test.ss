(import :std/test
        :std/text/json
        :gerbil-webview/libwebview
        :gerbil-webview/webview)

(export webview-test)

(def webview-test
  (test-suite "gerbil-webview"

    ;; ---- Unit tests (no window needed) ----

    (test-case "version returns valid list"
      (let ((v (webview-version)))
        (check (list? v) => #t)
        (check (length v) => 3)
        (check (car v) ? (lambda (x) (>= x 0)))
        (check (cadr v) ? (lambda (x) (>= x 0)))
        (check (caddr v) ? (lambda (x) (>= x 0)))))

    (test-case "hint constants are correct integers"
      (check WEBVIEW_HINT_NONE => 0)
      (check WEBVIEW_HINT_MIN => 1)
      (check WEBVIEW_HINT_MAX => 2)
      (check WEBVIEW_HINT_FIXED => 3))

    (test-case "error constants are correct integers"
      (check WEBVIEW_ERROR_OK => 0)
      (check WEBVIEW_ERROR_UNSPECIFIED => -1)
      (check WEBVIEW_ERROR_INVALID_ARGUMENT => -2)
      (check WEBVIEW_ERROR_INVALID_STATE => -3)
      (check WEBVIEW_ERROR_CANCELED => -4)
      (check WEBVIEW_ERROR_MISSING_DEPENDENCY => -5)
      (check WEBVIEW_ERROR_DUPLICATE => 1)
      (check WEBVIEW_ERROR_NOT_FOUND => 2))

    (test-case "native handle kind constants"
      (check WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW => 0)
      (check WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET => 1)
      (check WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER => 2))

    (test-case "callback registration and lookup"
      (let ((id (register-bind-handler!
                 (lambda (req-id req-json) 'ok))))
        (check (number? id) => #t)
        (check (hash-get *bind-handlers* id) ? values)
        (hash-remove! *bind-handlers* id)
        (check (hash-get *bind-handlers* id) => #f)))

    (test-case "dispatch handler registration (one-shot)"
      (let* ((fired (box #f))
             (id (register-dispatch-handler!
                  (lambda () (set-box! fired #t)))))
        (check (number? id) => #t)
        (check (hash-get *dispatch-handlers* id) ? values)
        ;; Simulate fire
        (let ((handler (hash-get *dispatch-handlers* id)))
          (hash-remove! *dispatch-handlers* id)
          (handler))
        (check (unbox fired) => #t)
        (check (hash-get *dispatch-handlers* id) => #f)))

    (test-case "JSON parse args - array of mixed types"
      (let ((args (call-with-input-string "[\"hello\", 42, true, null]" read-json)))
        (check (length args) => 4)
        (check (car args) => "hello")
        (check (cadr args) => 42)
        (check (caddr args) => #t)))

    (test-case "JSON serialize round-trip"
      (let* ((value (hash ("name" "test") ("count" 42)))
             (json-str (call-with-output-string
                        (lambda (p)
                          (parameterize ((current-output-port p))
                            (write-json value))))))
        (check (string? json-str) => #t)
        (let ((parsed (call-with-input-string json-str read-json)))
          (check (hash-ref parsed "name") => "test")
          (check (hash-ref parsed "count") => 42))))

    (test-case "JSON serialize primitives"
      (let ((serialize (lambda (v)
                         (call-with-output-string
                          (lambda (p)
                            (parameterize ((current-output-port p))
                              (write-json v)))))))
        (check (serialize 42) => "42")
        (check (serialize "hello") => "\"hello\"")
        (check (serialize #t) => "true")
        (check (serialize #f) => "false")
        (check (serialize [1 2 3]) => "[1,2,3]")))

    ;; ---- Integration tests (require DISPLAY) ----

    (test-case "create and destroy webview"
      (when (getenv "DISPLAY" #f)
        (let ((wv (webview-create)))
          (check (webview_t? wv) ? values)
          (webview-destroy! wv))))

    (test-case "set title and size"
      (when (getenv "DISPLAY" #f)
        (let ((wv (webview-create)))
          (webview-set-title! wv "Test Window")
          (webview-set-size! wv 640 480)
          (webview-set-size! wv 320 240 hint: WEBVIEW_HINT_MIN)
          (webview-destroy! wv))))

    (test-case "set HTML and navigate"
      (when (getenv "DISPLAY" #f)
        (let ((wv (webview-create)))
          (webview-set-html! wv "<html><body>Hello</body></html>")
          (webview-navigate! wv "data:text/html,<h1>Test</h1>")
          (webview-destroy! wv))))

    (test-case "eval JS"
      (when (getenv "DISPLAY" #f)
        (let ((wv (webview-create)))
          (webview-set-html! wv "<html><body id='main'>Hello</body></html>")
          (webview-eval! wv "document.title = 'Modified'")
          (webview-destroy! wv))))

    (test-case "bind and run with auto-terminate"
      (when (getenv "DISPLAY" #f)
        (let ((result (box #f))
              (wv (webview-create)))
          (webview-bind! wv "add"
            (lambda (args)
              (let ((r (apply + args)))
                (set-box! result r)
                r)))
          (webview-set-html! wv "<html><body>Bind Test</body></html>")
          (webview-init! wv
            (string-append
             "add(2, 3).then(function(r) {"
             "  window.__result = r;"
             "});"))
          (webview-init! wv
            "window.setTimeout(function() { __terminate(); }, 500);")
          (webview-bind! wv "__terminate"
            (lambda (args) (webview-terminate! wv) #!void))
          (webview-run! wv)
          (check (unbox result) => 5)
          (webview-destroy! wv))))))
