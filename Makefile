all: jdee-nrepl jdee-maven-nrepl jdee-live jdee-sample

nrepl:
	$(MAKE) -C jdee-sample nrepl

nrepl-client:
	lein repl :connect 12345

nrepl-client-exit:
	echo "(System/exit 0)" | lein repl :connect 12345

jdee-sample:
	$(MAKE) -C jdee-sample compile

jdee-maven-nrepl:
	$(MAKE) -C jdee-maven-nrepl install

jdee-nrepl:
	$(MAKE) -C jdee-nrepl install

jdee-live:
	$(MAKE) -C jdee-live package

.PHONY: jdee-nrepl jdee-maven-nrepl jdee-live jdee-sample
