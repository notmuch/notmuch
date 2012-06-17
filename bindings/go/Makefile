# Makefile for the go bindings of notmuch

export GOPATH      ?= $(shell pwd)
export CGO_CFLAGS  ?= -I../../../../lib
export CGO_LDFLAGS ?= -L../../../../lib

GO         ?= go
GOFMT      ?= gofmt

all: notmuch notmuch-addrlookup

.PHONY: notmuch
notmuch:
	$(GO) install notmuch

.PHONY: goconfig
goconfig:
	if [ ! -d src/github.com/kless/goconfig/config ]; then \
	    $(GO) get github.com/kless/goconfig/config; \
	fi

.PHONY: notmuch-addrlookup
notmuch-addrlookup: notmuch goconfig
	$(GO) install notmuch-addrlookup

.PHONY: format
format:
	$(GOFMT) -w=true $(GOFMT_OPTS) src/notmuch
	$(GOFMT) -w=true $(GOFMT_OPTS) src/notmuch-addrlookup

.PHONY: check-format
check-format:
	$(GOFMT) -d=true $(GOFMT_OPTS) src/notmuch
	$(GOFMT) -d=true $(GOFMT_OPTS) src/notmuch-addrlookup

.PHONY: clean
clean:
	$(GO) clean notmuch
	$(GO) clean notmuch-addrlookup
	rm -rf pkg bin
