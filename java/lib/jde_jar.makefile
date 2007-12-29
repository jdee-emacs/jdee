# Make the JDE jar file.

JAVA_HOME = c:/java/jdk1.3.1
JAR = $(JAVA_HOME)/bin/jar
MAKE = make
JDEDIR = $(JDEDEV)/jde
CLASSDIR = $(JDEDIR)/java/classes

CLASSES = $(CLASSDIR)/jde/debugger/*.class \
	  $(CLASSDIR)/jde/debugger/command/*.class \
	  $(CLASSDIR)/jde/debugger/expr/*.class \
	  $(CLASSDIR)/jde/debugger/spec/*.class \
	  $(CLASSDIR)/jde/util/*.class \
	  $(CLASSDIR)/jde/wizards/*.class \


all: 
	$(MAKE) build \
		-C $(JDEDEV)/jde/java/src/jde \
		-f $(JDEDEV)/jde/java/src/jde/makefile
	$(MAKE) jde.jar -f jde_jar.makefile


jde.jar : $(CLASSES)
	$(JAR) -cvf jde.jar -C "$(shell cygpath -w $(JDEDIR))/java/classes" jde