EMACS ?= emacs
EMACSFLAGS = -L .
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
	$(CASK) exec ert-runner -L .

.PHONY: clean
clean :
	rm -f $(OBJECTS)

.PHONY: elpaclean
elpaclean : clean
	rm -f elpa
	rm -rf .cask # Clean packages installed for development

%.elc : %.el
	$(CASK) build

.PHONY: run-jdee run-jde
run-jdee: elpa
	cask exec emacs -Q -L . --eval "(require 'jdee)"

run-jde: run-jdee
