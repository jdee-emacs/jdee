all:
	$(MAKE) -C .. all

compile:
	mvn compile


nrepl: compile
	mvn jdee:jdee-maven-nrepl:java -Dexec.mainClass="clojure.main" \
	-Dexec.args="../jdee-live/clj/jdee-launch-nrepl.clj" \
	-Dexec.includePluginsDependencies=true

