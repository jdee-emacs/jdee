# This makefile builds the JDE distributable.
# tr -d '\015\032' < input > output 

#
# Lisp Directory Target
#
LISPFILES = jde.el \
	    jde-autoload.el \
            jde-run.el \
	    jde-compile.el \
	    jde-db.el \
	    jde-jdb.el \
	    jde-bug.el \
	    jde-dbs.el \
	    jde-dbo.el \
	    jde-make.el \
	    jde-gen.el \
	    jde-wiz.el \
	    jde-parse.el \
	    jde-java-grammar.el \
	    beanshell.el \
	    jde-help.el \
	    jde-widgets.el \
	    jde-complete.el \
	    jde-javadoc.el \
	    jde-javadoc-gen.el \
	    jde-java-font-lock.el \
	    jde-java-font-lock.api \
	    jde-which-method.el \
	    jde-imenu.el \
	    jde-import.el \
	    jde-stat.el \
	    jde-package.el \
	    jde-project-file.el \
	    jde-custom.el \
	    jde-plugins.el \
	    jde-ant.el \
	    jde-util.el \
	    jde-checkstyle.el \
	    jde-ejb.el \
	    jde-xemacs.el \
	    setnu.el \
            jde-tree-widget.el \
	    jde-open-source.el \
	    jde-compat.el \
	    efc.el \
	    efc-xemacs.el \
	    jde-xref.el \
            jde-parse-class.el \
	    jde-class.el \
	    jde-sregex.el \
	    jde-junit.el \
	    java.bnf  \
	    jtags \
	    jtags.csh \
	    makefile.sample \
	    makefile \
	    ReleaseNotes.txt \
	    regress.el \
	    ChangeLog

all: $(LISPFILES)

%.el : $(JDEDIR)/lisp/%.el
	$(CP) $< .
	$(NEWLINE) $@

jde-java-font-lock.api : $(JDEDIR)/lisp/jde-java-font-lock.api
	$(CP) $< .
	$(NEWLINE) $@

java.bnf : $(JDEDIR)/lisp/java.bnf
	$(CP) $< .
	$(NEWLINE) $@

jtags : $(JDEDIR)/lisp/jtags
	$(CP) $< .
	$(NEWLINE) $@

jtags.csh : $(JDEDIR)/lisp/jtags.csh
	$(CP) $< .
	$(NEWLINE) $@

makefile : $(JDEDIR)/lisp/makefile
	$(CP) $< .
	$(NEWLINE) $@

makefile.sample : $(JDEDIR)/lisp/makefile.sample
	$(CP) $< .
	$(NEWLINE) $@

ReleaseNotes.txt : $(JDEDIR)/lisp/ReleaseNotes.txt
	$(CP) $< .
	$(NEWLINE) $@

ChangeLog : $(JDEDIR)/lisp/ChangeLog
	$(CP) $< .
	$(NEWLINE) $@



