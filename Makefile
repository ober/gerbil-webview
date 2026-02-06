export PKG_CONFIG_PATH := /usr/lib/x86_64-linux-gnu/pkgconfig:$(PKG_CONFIG_PATH)
export GERBIL_LOADPATH := $(HOME)/.gerbil/lib

.PHONY: build test clean install demo-todo demo-dashboard

build:
	gerbil build

install: build
	gerbil pkg install -g github.com/ober/gerbil-webview

test: build
	LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gerbil test ./...

clean:
	gerbil clean
	rm -f vendor/libwebview.so

demo-todo: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi demo/todo-app.ss

demo-dashboard: build
	GERBIL_LOADPATH=$(CURDIR)/.gerbil/lib:$(GERBIL_LOADPATH) LD_LIBRARY_PATH=$(CURDIR)/vendor:$$LD_LIBRARY_PATH gxi demo/dashboard.ss
