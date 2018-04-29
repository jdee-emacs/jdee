EMACS ?= emacs
EMACSFLAGS = -L . --debug
CASK ?= cask
#VERSION = $(shell git describe --tags --abbrev=0 | sed 's/^v//')
VERSION = 2.4.2
PACKAGE_NAME = jdee-$(VERSION)

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

elpa:
	$(CASK) install
	$(CASK) update
	touch $@

.PHONY: build
build : elpa $(OBJECTS)

.PHONY: test
test : clean
	$(CASK) exec ert-runner $(EMACSFLAGS)

.PHONY: clean
clean :
	rm -f $(OBJECTS)

all: jdee-nrepl jdee-maven-nrepl jdee-live jdee-sample jdee-lisp

nrepl:
	$(MAKE) -C jdee-sample nrepl

nrepl-client:
	lein repl :connect 12345

nrepl-client-exit:
	echo "(System/exit 0)" | lein repl :connect 12345

jdee-sample:
	$(MAKE) -C jdee-sample compile
.PHONY: package
package : test
	$(CASK) package


jdee-sbt-nrepl:
	cd jdee-sbt-nrepl && sbt package

jdee-maven-nrepl:
	$(MAKE) -C jdee-maven-nrepl install

jdee-nrepl:
	$(MAKE) -C jdee-nrepl install

jdee-live:
# Nothing to make here currently.
#	$(MAKE) -C jdee-live package

jdee-lisp:
	$(MAKE) -C jdee-lisp build

.PHONY: jdee-nrepl jdee-maven-nrepl jdee-live jdee-sample jdee-lisp test

test:
	$(MAKE) -C jdee-lisp test
.PHONY: elpaclean
elpaclean : clean
	rm -f elpa
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) build

.PHONY: run-jdee
run-jdee: elpa
	cask exec emacs -Q -L . --eval "(require 'jdee)"
