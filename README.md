# gerbil-webview

Cross-platform desktop GUI library for Gerbil Scheme using the [webview](https://github.com/webview/webview) C library. Embeds a native web view (WebKit on Linux/macOS, WebView2 on Windows) in a standalone application window.

Build desktop apps with HTML/CSS/JS frontends and Gerbil Scheme backends. No Electron, no browser chrome.

## Prerequisites

**Linux (Ubuntu/Debian)**:
```sh
sudo apt install libgtk-3-dev libwebkit2gtk-4.1-dev g++ pkg-config
```

**macOS**: WebKit.framework ships with macOS. Just need a C++ compiler:
```sh
xcode-select --install
```

## Installation

```sh
git clone <this-repo> gerbil-webview
cd gerbil-webview
make build
```

This builds the vendored `libwebview.so` and compiles the Gerbil modules.

## Quick Start

```scheme
(import :gerbil-webview/webview)

(def (main . args)
  (let ((wv (webview-create debug: #t)))
    (webview-set-title! wv "Hello")
    (webview-set-size! wv 400 300)
    (webview-set-html! wv "<h1>Hello from Gerbil!</h1>")
    (webview-run! wv)
    (webview-destroy! wv)))
```

## API Reference

### Lifecycle

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-create` | `debug: #f` | Create a webview instance. `debug: #t` enables dev tools. |
| `webview-destroy!` | `wv` | Destroy a webview and free resources. |
| `webview-run!` | `wv` | Run the event loop (blocks until terminated). |
| `webview-terminate!` | `wv` | Signal the event loop to exit. |

### Window

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-set-title!` | `wv title` | Set the window title. |
| `webview-set-size!` | `wv width height hint: WEBVIEW_HINT_NONE` | Set window size with optional hint. |

**Size hints**: `WEBVIEW_HINT_NONE` (default), `WEBVIEW_HINT_MIN`, `WEBVIEW_HINT_MAX`, `WEBVIEW_HINT_FIXED`

### Content

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-navigate!` | `wv url` | Navigate to a URL. |
| `webview-set-html!` | `wv html` | Set page content from an HTML string. |
| `webview-eval!` | `wv js` | Evaluate JavaScript in the webview. |
| `webview-init!` | `wv js` | Inject JS that runs on every page load. |

### Binding (JS-to-Scheme)

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-bind!` | `wv name handler` | Bind a synchronous Scheme function to JS. |
| `webview-bind-async!` | `wv name handler` | Bind an async Scheme function to JS. |
| `webview-unbind!` | `wv name` | Remove a binding. |

### Dispatch (Scheme-to-UI-thread)

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-dispatch!` | `wv thunk` | Schedule a thunk to run on the UI thread. |

### Info

| Function | Signature | Description |
|----------|-----------|-------------|
| `webview-version` | | Returns `(major minor patch)` version list. |

## Binding Guide

The binding system is the core of gerbil-webview. It creates a two-way bridge between JavaScript and Gerbil Scheme.

### Synchronous Bindings

`webview-bind!` creates a global JavaScript function that calls a Scheme handler. The handler receives a list of arguments (parsed from JSON) and returns a value (serialized to JSON).

```scheme
;; Scheme side: bind a function
(webview-bind! wv "add"
  (lambda (args)
    (apply + args)))

;; JavaScript side: call it (returns a Promise)
;; add(2, 3).then(result => console.log(result));  // => 5
```

**JSON marshaling rules**:
- JS `number` <-> Scheme `number`
- JS `string` <-> Scheme `string`
- JS `boolean` <-> Scheme `#t`/`#f`
- JS `null` <-> Scheme `#!void`
- JS `Array` <-> Scheme `list`
- JS `Object` <-> Scheme `hash-table`

**Error handling**: If the handler throws an exception, the JS Promise rejects with the error message.

### Async Bindings

`webview-bind-async!` is for handlers that need to do background work. The handler receives the args list and a `respond!` callback:

```scheme
(webview-bind-async! wv "fetchData"
  (lambda (args respond!)
    (spawn
     (lambda ()
       (let ((data (do-expensive-work (car args))))
         (respond! data))))))
```

### Architecture

```
[Gerbil Scheme]                 [WebView / JavaScript]

State (hash tables, lists)  <-> DOM rendering
handler (lambda)            <-  JS function call (Promise)
webview-eval! (JS string)   ->  DOM manipulation
```

## Threading Model

- `webview-run!` blocks the calling thread, running the native event loop.
- Bound Scheme handlers execute **on the event loop thread** when called from JS.
- `webview-dispatch!` schedules a thunk on the event loop thread (safe from any thread).
- `webview_return` (used internally) is thread-safe, so async bindings can respond from any thread.
- For background work, use `spawn` (Gerbil threads) and call `respond!` or `webview-dispatch!` when done.

## Demos

### Todo App

A self-contained todo application with HTML/CSS/JS frontend and Scheme backend:

```sh
make demo-todo
```

Features: add, toggle, delete todos. All state in Scheme, all rendering in JS.

### Dashboard

An interactive dashboard with system info, SVG charts (via gerbil-svg), and a Scheme evaluator:

```sh
make demo-dashboard
```

Requires [gerbil-svg](https://github.com/user/gerbil-svg) to be built and available.

## Testing

```sh
make test
```

Unit tests run without a display. Integration tests require `DISPLAY` to be set.

## Architecture

```
gerbil-webview/
  libwebview.ss    Raw FFI bindings (begin-ffi, c-define trampolines)
  webview.ss       High-level Gerbil API (JSON marshaling, error handling)
  vendor/
    webview.h      Amalgamated webview header (C++ with extern "C" API)
    libwebview.so  Compiled shared library
```

### FFI Design

The callback mechanism uses a dispatch table pattern:

1. Scheme closures are stored in a global hash table keyed by integer ID
2. A `c-define` trampoline receives callbacks from C with the ID encoded in `void* arg`
3. The trampoline looks up the closure by ID and calls it
4. Bind handlers are persistent (removed on unbind); dispatch handlers are one-shot

### Module Layout

- **`:gerbil-webview/libwebview`** — Raw C FFI. Types, constants, `define-c-lambda` wrappers, callback trampolines. Use this if you need direct access to the C API.
- **`:gerbil-webview/webview`** — High-level API. JSON marshaling, error checking, idiomatic Scheme names. Use this for application code.

## Platform Notes

| Feature | Linux | macOS |
|---------|-------|-------|
| Backend | WebKitGTK | WebKit.framework |
| Toolkit | GTK 3 | Cocoa |
| Dev tools | `debug: #t` | `debug: #t` |
| Dependencies | `libwebkit2gtk-4.1-dev` | None (ships with OS) |
| Library | `libwebview.so` | `libwebview.dylib` |

## License

Same as the webview library: MIT.
