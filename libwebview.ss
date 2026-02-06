(export #t)
(import :std/foreign)

(begin-ffi
    (;; Types
     webview_t?

     ;; Hint constants
     WEBVIEW_HINT_NONE WEBVIEW_HINT_MIN WEBVIEW_HINT_MAX WEBVIEW_HINT_FIXED

     ;; Error codes
     WEBVIEW_ERROR_OK WEBVIEW_ERROR_UNSPECIFIED
     WEBVIEW_ERROR_INVALID_ARGUMENT WEBVIEW_ERROR_INVALID_STATE
     WEBVIEW_ERROR_CANCELED WEBVIEW_ERROR_MISSING_DEPENDENCY
     WEBVIEW_ERROR_DUPLICATE WEBVIEW_ERROR_NOT_FOUND

     ;; Native handle kinds
     WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW
     WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET
     WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER

     ;; Lifecycle
     webview_create webview_destroy webview_run webview_terminate

     ;; Window
     webview_set_title webview_set_size
     webview_get_window webview_get_native_handle

     ;; Content
     webview_navigate webview_set_html webview_init webview_eval

     ;; Binding (raw)
     raw_webview_bind webview_unbind webview_return

     ;; Dispatch (raw)
     raw_webview_dispatch

     ;; Version helpers
     webview_version_major webview_version_minor webview_version_patch

     ;; Callback management
     *bind-handlers* *dispatch-handlers* *next-callback-id*
     register-bind-handler! register-dispatch-handler!)

  (declare (not safe))

  (c-declare #<<END-C
#include "webview.h"

/* ---- Finalizer ---- */
static ___SCMOBJ ffi_webview_destroy(void *ptr)
{
  if (ptr) webview_destroy((webview_t)ptr);
  return ___FIX(___NO_ERR);
}

/* ---- Version helpers ---- */
static int ffi_webview_version_major(void) {
  return (int)webview_version()->version.major;
}
static int ffi_webview_version_minor(void) {
  return (int)webview_version()->version.minor;
}
static int ffi_webview_version_patch(void) {
  return (int)webview_version()->version.patch;
}

/* ---- Forward declarations for c-define callbacks ---- */
/* Note: char* (not const char*) to match Gambit c-define char-string type */
void ffi_call_bind_handler(long callback_id, char *id, char *req);
void ffi_call_dispatch_handler(long callback_id);

/* ---- Bind trampoline ---- */
static void ffi_bind_trampoline(const char *id, const char *req, void *arg)
{
  long callback_id = (long)arg;
  ffi_call_bind_handler(callback_id, (char*)id, (char*)req);
}

/* ---- Dispatch trampoline ---- */
static void ffi_dispatch_trampoline(webview_t w, void *arg)
{
  long callback_id = (long)arg;
  ffi_call_dispatch_handler(callback_id);
}

/* ---- C wrappers for bind/dispatch ---- */
static int ffi_webview_bind(webview_t w, const char *name, long callback_id)
{
  return webview_bind(w, name, ffi_bind_trampoline, (void*)callback_id);
}

static int ffi_webview_dispatch(webview_t w, long callback_id)
{
  return webview_dispatch(w, ffi_dispatch_trampoline, (void*)callback_id);
}
END-C
  )

  ;; ---- Type definitions ----
  (c-define-type webview_t "void")
  (c-define-type webview_t* (pointer webview_t (webview_t*) "ffi_webview_destroy"))

  (define-macro (define-c-type-predicate pred tag)
    `(define (,pred x)
       (and (##foreign? x)
            (##memq ',tag (foreign-tags x)))))

  (define-c-type-predicate webview_t? webview_t*)

  ;; ---- Constants ----
  ;; Hint constants
  (define-const WEBVIEW_HINT_NONE)
  (define-const WEBVIEW_HINT_MIN)
  (define-const WEBVIEW_HINT_MAX)
  (define-const WEBVIEW_HINT_FIXED)

  ;; Error codes
  (define-const WEBVIEW_ERROR_OK)
  (define-const WEBVIEW_ERROR_UNSPECIFIED)
  (define-const WEBVIEW_ERROR_INVALID_ARGUMENT)
  (define-const WEBVIEW_ERROR_INVALID_STATE)
  (define-const WEBVIEW_ERROR_CANCELED)
  (define-const WEBVIEW_ERROR_MISSING_DEPENDENCY)
  (define-const WEBVIEW_ERROR_DUPLICATE)
  (define-const WEBVIEW_ERROR_NOT_FOUND)

  ;; Native handle kinds
  (define-const WEBVIEW_NATIVE_HANDLE_KIND_UI_WINDOW)
  (define-const WEBVIEW_NATIVE_HANDLE_KIND_UI_WIDGET)
  (define-const WEBVIEW_NATIVE_HANDLE_KIND_BROWSER_CONTROLLER)

  ;; ---- Function bindings ----
  ;; Lifecycle
  (define-c-lambda webview_create (int (pointer void)) webview_t*
    "webview_create")
  (define-c-lambda webview_destroy (webview_t*) int
    "webview_destroy")
  (define-c-lambda webview_run (webview_t*) int
    "webview_run")
  (define-c-lambda webview_terminate (webview_t*) int
    "webview_terminate")

  ;; Window
  (define-c-lambda webview_set_title (webview_t* char-string) int
    "webview_set_title")
  (define-c-lambda webview_set_size (webview_t* int int int) int
    "webview_set_size")
  (define-c-lambda webview_get_window (webview_t*) (pointer void)
    "webview_get_window")
  (define-c-lambda webview_get_native_handle (webview_t* int) (pointer void)
    "webview_get_native_handle")

  ;; Content
  (define-c-lambda webview_navigate (webview_t* char-string) int
    "webview_navigate")
  (define-c-lambda webview_set_html (webview_t* char-string) int
    "webview_set_html")
  (define-c-lambda webview_init (webview_t* char-string) int
    "webview_init")
  (define-c-lambda webview_eval (webview_t* char-string) int
    "webview_eval")

  ;; Binding (via C wrapper)
  (define-c-lambda raw_webview_bind (webview_t* char-string long) int
    "ffi_webview_bind")
  (define-c-lambda webview_unbind (webview_t* char-string) int
    "webview_unbind")
  (define-c-lambda webview_return (webview_t* char-string int char-string) int
    "webview_return")

  ;; Dispatch (via C wrapper)
  (define-c-lambda raw_webview_dispatch (webview_t* long) int
    "ffi_webview_dispatch")

  ;; Version helpers
  (define-c-lambda webview_version_major () int "ffi_webview_version_major")
  (define-c-lambda webview_version_minor () int "ffi_webview_version_minor")
  (define-c-lambda webview_version_patch () int "ffi_webview_version_patch")

  ;; ---- Callback dispatch tables ----
  (define *bind-handlers* (make-hash-table))
  (define *dispatch-handlers* (make-hash-table))
  (define *next-callback-id* 0)

  (define (register-bind-handler! handler)
    (let ((id *next-callback-id*))
      (set! *next-callback-id* (+ id 1))
      (hash-put! *bind-handlers* id handler)
      id))

  (define (register-dispatch-handler! handler)
    (let ((id *next-callback-id*))
      (set! *next-callback-id* (+ id 1))
      (hash-put! *dispatch-handlers* id handler)
      id))

  ;; ---- c-define trampolines (Scheme functions callable from C) ----
  (c-define (ffi_call_bind_handler callback-id id req)
            (long char-string char-string) void
            "ffi_call_bind_handler" ""
    (let ((handler (hash-ref *bind-handlers* callback-id #f)))
      (when handler (handler id req))))

  (c-define (ffi_call_dispatch_handler callback-id)
            (long) void
            "ffi_call_dispatch_handler" ""
    (let ((handler (hash-ref *dispatch-handlers* callback-id #f)))
      (when handler
        (hash-remove! *dispatch-handlers* callback-id)
        (handler))))
) ;; end begin-ffi
