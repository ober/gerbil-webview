#!/usr/bin/env gxi
;;; Dashboard Demo — interactive dashboard with gerbil-svg charts
;;; Shows system info, SVG chart generation, and async operations.

(import :gerbil-webview/webview
        :std/text/json
        :std/sugar
        :std/misc/process
        :gerbil-svg/svg)

;; ---- System info helpers ----
(def (get-hostname)
  (string-trim-right (with-output-to-string (lambda () (run-process ["hostname"])))))

(def (get-uptime)
  (string-trim-right (with-output-to-string (lambda () (run-process ["uptime" "-p"])))))

(def (get-loadavg)
  (let ((content (call-with-input-file "/proc/loadavg" read-line)))
    (let ((parts (string-split content #\space)))
      (list (car parts) (cadr parts) (caddr parts)))))

(def (get-meminfo)
  (let ((lines (call-with-input-file "/proc/meminfo"
                 (lambda (p)
                   (let loop ((acc []))
                     (let ((line (read-line p)))
                       (if (eof-object? line) (reverse acc)
                           (loop (cons line acc)))))))))
    (let ((total (parse-meminfo-kb "MemTotal:" lines))
          (avail (parse-meminfo-kb "MemAvailable:" lines)))
      (hash ("total_gb" (/ (exact->inexact total) 1048576.0))
            ("used_gb" (/ (exact->inexact (- total avail)) 1048576.0))
            ("avail_gb" (/ (exact->inexact avail) 1048576.0))))))

(def (parse-meminfo-kb key lines)
  (let loop ((ls lines))
    (if (null? ls) 0
        (let ((line (car ls)))
          (if (string-prefix? key line)
            (string->number
             (string-trim-both
              (string-trim-right
               (substring line (string-length key) (string-length line))
               (lambda (c) (or (char=? c #\space) (char=? c #\k) (char=? c #\B))))))
            (loop (cdr ls)))))))

(def (string-trim-both s)
  (string-trim-right (string-trim-left s)))

(def (string-trim-left s)
  (let loop ((i 0))
    (if (and (< i (string-length s)) (char=? (string-ref s i) #\space))
      (loop (+ i 1))
      (substring s i (string-length s)))))

(def (string-trim-right s . pred)
  (let ((pred (if (null? pred) char-whitespace? (car pred))))
    (let loop ((i (- (string-length s) 1)))
      (if (and (>= i 0) (pred (string-ref s i)))
        (loop (- i 1))
        (substring s 0 (+ i 1))))))

;; ---- SVG chart generation ----
(def (make-bar-chart data width: (width 400) height: (height 200))
  ;; data: list of (label . value)
  (let* ((n (length data))
         (max-val (apply max (map cdr data)))
         (bar-width (/ (* width 0.7) n))
         (gap (/ (* width 0.3) (+ n 1)))
         (bars (let loop ((i 0) (items data) (acc []))
                 (if (null? items) (reverse acc)
                     (let* ((item (car items))
                            (label (car item))
                            (val (cdr item))
                            (bar-h (* (/ val max-val) (* height 0.75)))
                            (x (+ gap (* i (+ bar-width gap))))
                            (y (- height bar-h 20)))
                       (loop (+ i 1) (cdr items)
                             (cons
                              (svg-g
                               [(svg-rect x y bar-width bar-h
                                          fill: "#e74c3c" rx: 3)
                                (svg-text (+ x (/ bar-width 2)) (+ y bar-h 14) label
                                          font-size: 11 text-anchor: "middle"
                                          fill: "#666" font-family: "sans-serif")
                                (svg-text (+ x (/ bar-width 2)) (- y 4)
                                          (number->string (inexact->exact (round val)))
                                          font-size: 10 text-anchor: "middle"
                                          fill: "#333" font-family: "sans-serif")])
                              acc)))))))
    (svg->string
     (make-svg-document width height
                        viewbox: (string-append "0 0 " (number->string width)
                                                " " (number->string height))
                        elements: bars))))

;; ---- HTML template ----
(def dashboard-html #<<HTML
<!DOCTYPE html>
<html>
<head>
<style>
  * { box-sizing: border-box; margin: 0; padding: 0; }
  body {
    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
    background: #1a1a2e; color: #eee; padding: 20px;
  }
  h1 { text-align: center; margin-bottom: 20px; color: #e94560; }
  .grid { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; max-width: 800px; margin: 0 auto; }
  .card {
    background: #16213e; border-radius: 10px; padding: 20px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.3);
  }
  .card h2 { color: #e94560; font-size: 14px; text-transform: uppercase;
             letter-spacing: 1px; margin-bottom: 12px; }
  .card.full { grid-column: 1 / -1; }
  .info-row { display: flex; justify-content: space-between; padding: 6px 0;
              border-bottom: 1px solid #1a1a2e; }
  .info-label { color: #888; }
  .info-value { color: #0f3460; font-weight: 600; color: #4ecca3; }
  .chart-container { text-align: center; }
  .chart-container svg { max-width: 100%; }
  .input-row { display: flex; gap: 8px; margin-top: 10px; }
  .input-row input {
    flex: 1; padding: 8px 12px; border: 1px solid #0f3460; border-radius: 6px;
    background: #1a1a2e; color: #eee; font-size: 14px; outline: none;
  }
  .input-row input:focus { border-color: #e94560; }
  .btn {
    padding: 8px 16px; border: none; border-radius: 6px; cursor: pointer;
    background: #e94560; color: white; font-size: 14px;
  }
  .btn:hover { opacity: 0.85; }
  #result { margin-top: 10px; padding: 10px; background: #1a1a2e;
            border-radius: 6px; font-family: monospace; min-height: 20px; }
  .mem-bar { height: 20px; background: #0f3460; border-radius: 10px; overflow: hidden; margin-top: 8px; }
  .mem-bar-fill { height: 100%; background: linear-gradient(90deg, #4ecca3, #e94560);
                  border-radius: 10px; transition: width 0.3s; }
</style>
</head>
<body>
  <h1>Dashboard</h1>
  <div class="grid">
    <div class="card">
      <h2>System Info</h2>
      <div id="sysinfo">Loading...</div>
    </div>
    <div class="card">
      <h2>Memory</h2>
      <div id="meminfo">Loading...</div>
    </div>
    <div class="card full">
      <h2>Load Average Chart</h2>
      <div class="chart-container" id="chart">Loading...</div>
    </div>
    <div class="card full">
      <h2>Scheme Evaluator</h2>
      <div class="input-row">
        <input type="text" id="expr-input" placeholder="Enter a Scheme expression..."
               onkeydown="if(event.key==='Enter')evalExpr()">
        <button class="btn" onclick="evalExpr()">Eval</button>
      </div>
      <div id="result"></div>
    </div>
  </div>

  <script>
    async function loadDashboard() {
      var info = await _getSysinfo();
      document.getElementById('sysinfo').innerHTML =
        '<div class="info-row"><span class="info-label">Hostname</span><span class="info-value">' + info.hostname + '</span></div>' +
        '<div class="info-row"><span class="info-label">Uptime</span><span class="info-value">' + info.uptime + '</span></div>' +
        '<div class="info-row"><span class="info-label">Load</span><span class="info-value">' + info.load.join(', ') + '</span></div>';

      var mem = await _getMeminfo();
      var pct = ((mem.used_gb / mem.total_gb) * 100).toFixed(1);
      document.getElementById('meminfo').innerHTML =
        '<div class="info-row"><span class="info-label">Total</span><span class="info-value">' + mem.total_gb.toFixed(1) + ' GB</span></div>' +
        '<div class="info-row"><span class="info-label">Used</span><span class="info-value">' + mem.used_gb.toFixed(1) + ' GB (' + pct + '%)</span></div>' +
        '<div class="info-row"><span class="info-label">Available</span><span class="info-value">' + mem.avail_gb.toFixed(1) + ' GB</span></div>' +
        '<div class="mem-bar"><div class="mem-bar-fill" style="width:' + pct + '%"></div></div>';

      var chart = await _getChart();
      document.getElementById('chart').innerHTML = chart;
    }

    async function evalExpr() {
      var input = document.getElementById('expr-input');
      var expr = input.value.trim();
      if (expr) {
        document.getElementById('result').textContent = 'Evaluating...';
        var result = await _evalScheme(expr);
        document.getElementById('result').textContent = '=> ' + result;
      }
    }

    loadDashboard();
  </script>
</body>
</html>
HTML
)

;; ---- Main ----
(def (main . args)
  (let ((wv (webview-create debug: #t)))
    (webview-set-title! wv "Dashboard — gerbil-webview")
    (webview-set-size! wv 840 680)

    ;; System info binding
    (webview-bind! wv "_getSysinfo"
      (lambda (args)
        (hash ("hostname" (get-hostname))
              ("uptime" (get-uptime))
              ("load" (get-loadavg)))))

    ;; Memory info binding
    (webview-bind! wv "_getMeminfo"
      (lambda (args) (get-meminfo)))

    ;; SVG chart binding
    (webview-bind! wv "_getChart"
      (lambda (args)
        (let ((load (get-loadavg)))
          (make-bar-chart
           (list (cons "1 min" (string->number (car load)))
                 (cons "5 min" (string->number (cadr load)))
                 (cons "15 min" (string->number (caddr load))))))))

    ;; Scheme eval binding
    (webview-bind! wv "_evalScheme"
      (lambda (args)
        (with-catch
         (lambda (e)
           (with-output-to-string (lambda () (display-exception e))))
         (lambda ()
           (let ((result (eval (call-with-input-string (car args) read))))
             (with-output-to-string (lambda () (write result))))))))

    (webview-set-html! wv dashboard-html)
    (webview-run! wv)
    (webview-destroy! wv)))
