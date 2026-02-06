#!/usr/bin/env gxi
;;; Todo App — self-contained demo using gerbil-webview
;;; All state lives in Scheme; all rendering in HTML/JS.

(import :gerbil-webview/webview
        :std/text/json
        :std/sugar)

;; ---- State ----
(def *todos* [])
(def *next-id* 0)

(def (make-todo text)
  (let ((id *next-id*))
    (set! *next-id* (+ id 1))
    (hash ("id" id) ("text" text) ("done" #f))))

;; ---- State operations ----
(def (add-todo! text)
  (set! *todos* (append *todos* [(make-todo text)])))

(def (toggle-todo! id)
  (set! *todos*
    (map (lambda (t)
           (if (= (hash-ref t "id") id)
             (let ((new-t (hash-copy t)))
               (hash-put! new-t "done" (not (hash-ref t "done")))
               new-t)
             t))
         *todos*)))

(def (delete-todo! id)
  (set! *todos*
    (filter (lambda (t) (not (= (hash-ref t "id") id))) *todos*)))

;; ---- Render todos as JS call to update DOM ----
(def (render-todos! wv)
  (let ((json (call-with-output-string
               (lambda (p)
                 (parameterize ((current-output-port p))
                   (write-json *todos*))))))
    (webview-eval! wv (string-append "renderTodos(" json ")"))))

;; ---- HTML/CSS/JS ----
(def todo-html #<<HTML
<!DOCTYPE html>
<html>
<head>
<style>
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    max-width: 500px; margin: 40px auto; padding: 0 20px;
    background: #f5f5f5; color: #333;
  }
  h1 { text-align: center; margin-bottom: 20px; color: #e74c3c; font-size: 2em; }
  .input-row {
    display: flex; gap: 8px; margin-bottom: 20px;
  }
  #todo-input {
    flex: 1; padding: 10px 14px; border: 2px solid #ddd; border-radius: 6px;
    font-size: 16px; outline: none; transition: border-color 0.2s;
  }
  #todo-input:focus { border-color: #e74c3c; }
  .btn {
    padding: 10px 20px; border: none; border-radius: 6px;
    font-size: 16px; cursor: pointer; transition: opacity 0.2s;
  }
  .btn:hover { opacity: 0.85; }
  .btn-add { background: #e74c3c; color: white; }
  .todo-list { list-style: none; }
  .todo-item {
    display: flex; align-items: center; gap: 12px;
    padding: 12px 14px; margin-bottom: 8px;
    background: white; border-radius: 6px;
    box-shadow: 0 1px 3px rgba(0,0,0,0.08);
  }
  .todo-item.done .todo-text { text-decoration: line-through; color: #aaa; }
  .todo-text { flex: 1; font-size: 16px; }
  .todo-check { width: 20px; height: 20px; cursor: pointer; accent-color: #e74c3c; }
  .btn-delete {
    background: none; border: none; color: #ccc; font-size: 20px;
    cursor: pointer; padding: 0 4px; transition: color 0.2s;
  }
  .btn-delete:hover { color: #e74c3c; }
  .status { text-align: center; margin-top: 16px; color: #888; font-size: 14px; }
</style>
</head>
<body>
  <h1>Todo App</h1>
  <div class="input-row">
    <input type="text" id="todo-input" placeholder="What needs to be done?"
           onkeydown="if(event.key==='Enter')addTodo()">
    <button class="btn btn-add" onclick="addTodo()">Add</button>
  </div>
  <ul class="todo-list" id="todo-list"></ul>
  <div class="status" id="status"></div>

  <script>
    async function addTodo() {
      var input = document.getElementById('todo-input');
      var text = input.value.trim();
      if (text) {
        await _addTodo(text);
        input.value = '';
        input.focus();
      }
    }

    async function toggleTodo(id) {
      await _toggleTodo(id);
    }

    async function deleteTodo(id) {
      await _deleteTodo(id);
    }

    function renderTodos(todos) {
      var list = document.getElementById('todo-list');
      var html = '';
      for (var i = 0; i < todos.length; i++) {
        var t = todos[i];
        var cls = t.done ? 'todo-item done' : 'todo-item';
        var chk = t.done ? 'checked' : '';
        html += '<li class="' + cls + '">'
             + '<input type="checkbox" class="todo-check" ' + chk
             + ' onchange="toggleTodo(' + t.id + ')">'
             + '<span class="todo-text">' + escapeHtml(t.text) + '</span>'
             + '<button class="btn-delete" onclick="deleteTodo(' + t.id + ')">&#10005;</button>'
             + '</li>';
      }
      list.innerHTML = html;
      var done = todos.filter(function(t) { return t.done; }).length;
      var total = todos.length;
      document.getElementById('status').textContent =
        total === 0 ? 'No todos yet — add one above!'
                    : done + ' of ' + total + ' completed';
    }

    function escapeHtml(s) {
      var div = document.createElement('div');
      div.appendChild(document.createTextNode(s));
      return div.innerHTML;
    }

    renderTodos([]);
  </script>
</body>
</html>
HTML
)

;; ---- Main ----
(def (main . args)
  (let ((wv (webview-create debug: #t)))
    (webview-set-title! wv "Todo App — gerbil-webview")
    (webview-set-size! wv 520 600)

    ;; Bind Scheme handlers for JS to call
    (webview-bind! wv "_addTodo"
      (lambda (args)
        (add-todo! (car args))
        (render-todos! wv)
        #!void))

    (webview-bind! wv "_toggleTodo"
      (lambda (args)
        (toggle-todo! (car args))
        (render-todos! wv)
        #!void))

    (webview-bind! wv "_deleteTodo"
      (lambda (args)
        (delete-todo! (car args))
        (render-todos! wv)
        #!void))

    (webview-set-html! wv todo-html)
    (webview-run! wv)
    (webview-destroy! wv)))
