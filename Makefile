export PKG_CONFIG_PATH := /usr/lib/x86_64-linux-gnu/pkgconfig:$(PKG_CONFIG_PATH)

WEBKIT_CFLAGS := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --cflags gtk+-3.0 webkit2gtk-4.1 2>/dev/null)
WEBKIT_LIBS   := $(shell PKG_CONFIG_PATH=$(PKG_CONFIG_PATH) pkg-config --libs gtk+-3.0 webkit2gtk-4.1 2>/dev/null)

.PHONY: build test clean demo-todo demo-dashboard

vendor/libwebview.so: vendor/webview.h
	g++ -shared -fPIC -o $@ -DWEBVIEW_BUILD_SHARED \
	    -x c++ vendor/webview.h $(WEBKIT_CFLAGS) $(WEBKIT_LIBS)

build: vendor/libwebview.so
	gerbil build

test: build
	LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gerbil test ./...

clean:
	gerbil clean
	rm -f vendor/libwebview.so

demo-todo: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi demo/todo-app.ss

demo-dashboard: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi demo/dashboard.ss
