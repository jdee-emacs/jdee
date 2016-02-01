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
