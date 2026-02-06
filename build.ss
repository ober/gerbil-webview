#!/usr/bin/env gxi
(import :std/build-script
        :std/make)

(def here (path-directory (this-source-file)))
(def vendor-dir (path-expand "vendor" here))

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
