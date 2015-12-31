# Migration from SourceForge version to GH version
- All JDEE functions and variables have been renamed from *jde* to *jdee*, so do so accordingly in your config file
- JDEE have been split into two parts:
  - JDEE-elisp: installable from MELPA
  - JDEE server: JVM part that have to build manually from http://github.com/jdee-emacs/jdee-server repository
- JDEE elisp needs ```jdee-server-dir``` which should point to JAR built from JDEE-Server project

# nREPL integration
- For projects using maven, JDEE will launch a clojure nREPL via maven to configure itself from the pom.
- Currently will get the classpath and the sourcepath.
- Starting the nREPL happens in the background, so there is no delay waiting for it to start

- All issues should be reported here in http://github.com/jdee-emacs/jdee/issues
- Pull Requests are more than welcomed! :-)
