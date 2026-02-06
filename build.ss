#!/usr/bin/env gxi
(import :std/build-script
        :std/make
        :std/misc/process)

;; Ensure pkg-config finds system packages (linuxbrew's pkg-config
;; only searches its own paths; add the system multiarch directory)
(let ((current (getenv "PKG_CONFIG_PATH" ""))
      (sysdir "/usr/lib/x86_64-linux-gnu/pkgconfig"))
  (when (and (file-exists? sysdir)
             (not (string-contains current sysdir)))
    (setenv "PKG_CONFIG_PATH"
            (if (string-empty? current) sysdir
                (string-append sysdir ":" current)))))

(def here (path-directory (this-source-file)))
(def vendor-dir (path-expand "vendor" here))
(def vendor-so (path-expand "libwebview.so" vendor-dir))
(def vendor-h (path-expand "webview.h" vendor-dir))

;; Build vendor/libwebview.so if it doesn't exist or is older than the header
(unless (and (file-exists? vendor-so)
             (>= (time->seconds (file-info-last-modification-time (file-info vendor-so)))
                 (time->seconds (file-info-last-modification-time (file-info vendor-h)))))
  (displayln "... compile vendor/libwebview.so")
  (run-process ["g++" "-shared" "-fPIC" "-o" vendor-so "-DWEBVIEW_BUILD_SHARED"
                "-x" "c++" vendor-h
                (string-split (cppflags "gtk+-3.0" "") #\space) ...
                (string-split (cppflags "webkit2gtk-4.1" "") #\space) ...
                (string-split (ldflags "gtk+-3.0" "-lgtk-3") #\space) ...
                (string-split (ldflags "webkit2gtk-4.1" "-lwebkit2gtk-4.1") #\space) ...]
               coprocess: void))

(defbuild-script
  `((gxc: "libwebview"
          "-cc-options" ,(string-append
                          "-I" vendor-dir " "
                          (cppflags "webkit2gtk-4.1" "")
                          " "
                          (cppflags "gtk+-3.0" ""))
          "-ld-options" ,(string-append
                          "-L" vendor-dir " -lwebview "
                          "-Wl,-rpath," vendor-dir " "
                          (ldflags "webkit2gtk-4.1" "-lwebkit2gtk-4.1")
                          " "
                          (ldflags "gtk+-3.0" "-lgtk-3")))
    "webview"))
