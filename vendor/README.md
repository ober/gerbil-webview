# Vendored webview

This directory contains the amalgamated [webview](https://github.com/webview/webview) header and the compiled shared library.

## Contents

- `webview.h` — Amalgamated C/C++ header (generated from webview upstream)
- `libwebview.so` — Compiled shared library (built by `make`)

## Updating

To update the vendored header to a new webview release:

```sh
git clone --depth 1 --branch <TAG> https://github.com/webview/webview /tmp/webview-src
cd /tmp/webview-src
python3 scripts/amalgamate/amalgamate.py \
  --search core/include core/include/webview core/include/webview/detail \
  --output vendor/webview.h \
  core/include/webview/webview.h
```

Then rebuild:

```sh
make clean
make build
```
