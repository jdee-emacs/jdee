# Migration from SourceForge version to GH version
- All JDEE functions and variables have been renamed from *jde* to *jdee*, so do so accordingly in your config file
- JDEE have been split into two parts:
  - JDEE-elisp: installable from MELPA
  - JDEE server: JVM part that have to build manually from http://github.com/jdee-emacs/jdee-server repository
- JDEE elisp needs ```jdee-server-dir``` which should point to JAR built from JDEE-Server project
- All issues should be reported here in http://github.com/jdee-emacs/jdee/issues
- Pull Requests are more than welcomed! :-)
