# This makefile builds the JDE distributable.
# tr -d '\015\032' < input > output 

export DRIVE = c
PERL = perl
CP = cp -r
RM = rm
COPY = cp
GMAKE = gmake
MKDIR = mkdir
TAR = tar
GZIP = gzip
ZIP = zip -v -r
JAR = jar
VERSION = 2.2.7beta2
RELEASENAME = jde-$(VERSION)
export DEVDIR = //$(DRIVE)/Applications/cygwin/home/jde-dev
export DEVDIRDOS = $(DRIVE):/Applications/cygwin/home/jde-dev/
export JDEDIR = $(DEVDIR)/jde
export JDEDIRDOS = $(DEVDIRDOS)\jde
RELEASEDIR = $(DEVDIR)/distrib/zip/$(RELEASENAME)
RELEASEDIRDOS = $(DEVDIRDOS)/distrib/zip/$(RELEASENAME)
DOS2UNIX = $(PERL) $(DEVDIRDOS)/dos2unix.pl
NEWLINE = //c/applications/winutils/newline /r /1
EIEIODIR = $(HOME)/emacs/site/eieio-dev
XEMACSDRIVE = c
XEMACSNAME = xemacs-jde
XEMACSDIR = /$(XEMACSDRIVE)/$(XEMACSNAME)
XEMACSDIRDOS = $(XEMACSDRIVE):/$(XEMACSNAME)

distributable:
	$(MAKE) distributable1 \
		-C $(DEVDIR)/.. \
		-f $(DEVDIR)/jde/makefile	

distributable1:
#
#	Remove any existing instance of the release directory.
#
	$(RM) -rf $(RELEASEDIR)
#
#       Lisp code
#
	$(MKDIR) $(RELEASEDIR)
	$(MKDIR) $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde.el $(RELEASEDIR)/lisp/jde.el
	$(CP) $(JDEDIR)/lisp/jde-run.el $(RELEASEDIR)/lisp/jde-run.el
	$(CP) $(JDEDIR)/lisp/jde-compile.el $(RELEASEDIR)/lisp/jde-compile.el
	$(CP) $(JDEDIR)/lisp/jde-db.el $(RELEASEDIR)/lisp/jde-db.el
	$(CP) $(JDEDIR)/lisp/jde-bug.el $(RELEASEDIR)/lisp/jde-bug.el
	$(CP) $(JDEDIR)/lisp/jde-dbs.el $(RELEASEDIR)/lisp/jde-dbs.el
	$(CP) $(JDEDIR)/lisp/jde-dbo.el $(RELEASEDIR)/lisp/jde-dbo.el
	$(CP) $(JDEDIR)/lisp/jde-make.el $(RELEASEDIR)/lisp/jde-make.el
	$(CP) $(JDEDIR)/lisp/jde-gen.el $(RELEASEDIR)/lisp/jde-gen.el
	$(CP) $(JDEDIR)/lisp/jde-wiz.el $(RELEASEDIR)/lisp/jde-wiz.el
	$(CP) $(JDEDIR)/lisp/jde-parse.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-java-grammar.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/beanshell.el $(RELEASEDIR)/lisp/beanshell.el
	$(CP) $(JDEDIR)/lisp/jde-help.el $(RELEASEDIR)/lisp/jde-help.el
	$(CP) $(JDEDIR)/lisp/jde-widgets.el $(RELEASEDIR)/lisp/jde-widgets.el
	$(CP) $(JDEDIR)/lisp/jde-complete.el $(RELEASEDIR)/lisp/jde-complete.el
	$(CP) $(JDEDIR)/lisp/jde-javadoc.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-javadoc-gen.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-java-font-lock.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-java-font-lock.api $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-which-method.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-imenu.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-import.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-stat.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-package.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/setnu.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/senator.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/senator-isearch.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/java.bnf $(RELEASEDIR)/lisp/java.bnf
	$(CP) $(JDEDIR)/lisp/jtags $(RELEASEDIR)/lisp/jtags
	$(CP) $(JDEDIR)/lisp/jtags.csh $(RELEASEDIR)/lisp/jtags.csh
	$(CP) $(JDEDIR)/lisp/makefile.sample $(RELEASEDIR)/lisp/makefile.sample
	$(CP) $(JDEDIR)/lisp/makefile $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/ReleaseNotes.txt $(RELEASEDIR)
	$(CP) $(JDEDIR)/lisp/ChangeLog $(RELEASEDIR)/lisp/ChangeLog
	$(NEWLINE) $(RELEASEDIRDOS)/lisp
#
#	Make changelog.
#

#
#       JDE Documentation Directory
#
	$(MKDIR) $(RELEASEDIR)/doc
#
#       Documentation Source Directory
#
	$(MKDIR) $(RELEASEDIR)/doc/src
#
#       JDE User Guide Source
#
	$(MKDIR) $(RELEASEDIR)/doc/src/jde-ug
	$(CP) $(JDEDIR)/doc/src/jde-ug/jde-ug.html $(RELEASEDIR)/doc/src/jde-ug
	$(CP) $(JDEDIR)/doc/src/jde-ug/jde-ug-content.xml $(RELEASEDIR)/doc/src/jde-ug
	$(NEWLINE) $(RELEASEDIRDOS)/doc/src/jde-ug

	$(MKDIR) $(RELEASEDIR)/doc/src/jde-ug/images
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/JdeMenu.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/KeyBindings.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/OverrideMeth1.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/OverrideMeth2.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/OverrideMeth3.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/OverrideMeth4.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/OverrideMeth5.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/RegisterTemplate.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/TemplateList.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/WorkingDirectory.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/speedbar1.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/speedbar2.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/speedbar3.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/speedbar4.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/speedbar5.gif $(RELEASEDIR)/doc/src/jde-ug/images/
	$(CP) $(JDEDIR)/doc/src/jde-ug/images/classes_menu1.gif $(RELEASEDIR)/doc/src/jde-ug/images/
#
#	Make JDE User Guides TOC
#
	$(MAKE) -f $(JDEDIR)/doc/src/jde-ug/makefile \
		-C $(JDEDIR)/doc/src/jde-ug all
#
#       HTML Cascading Stylesheets
#
	$(MKDIR) $(RELEASEDIR)/doc/src/css
	$(CP) $(JDEDIR)/doc/src/css/jde_style.css $(RELEASEDIR)/doc/src/css/
#
#	XSL Stylesheets
#
	$(MKDIR) $(RELEASEDIR)/doc/src/styles
	$(MKDIR) $(RELEASEDIR)/doc/src/styles/html
	$(CP) $(JDEDIR)/doc/src/styles/html/jdebook.xsl $(RELEASEDIR)/doc/src/styles/html
	$(CP) $(JDEDIR)/doc/src/styles/html/jdebook_html_toc.xsl $(RELEASEDIR)/doc/src/styles/html
	$(CP) $(JDEDIR)/doc/src/styles/html/jdebook_toc.xsl $(RELEASEDIR)/doc/src/styles/html
#
#	HTML Documentation
#
	$(MKDIR) $(RELEASEDIR)/doc/html
#
#	Copy cascading style sheet.
#
	$(MKDIR) $(RELEASEDIR)/doc/html/css
	$(CP) $(JDEDIR)/doc/src/css/*.css $(RELEASEDIR)/doc/html/css
#
#       JDE User's Guide
#
	$(MKDIR) $(RELEASEDIR)/doc/html/jde-ug
	$(CP) $(JDEDIR)/doc/html/jde-ug/jde-ug-toc.html $(RELEASEDIR)/doc/html/jde-ug/jde-ug-toc.html
	$(CP) $(JDEDIR)/doc/html/jde-ug/jde-ug-content.html $(RELEASEDIR)/doc/html/jde-ug/jde-ug-content.html
	$(CP) $(JDEDIR)/doc/html/jde-ug/jde-ug.html $(RELEASEDIR)/doc/html/jde-ug/jde-ug.html
	$(NEWLINE) $(RELEASEDIRDOS)/doc/html/jde-ug

	$(MKDIR) $(RELEASEDIR)/doc/html/jde-ug/images
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/JdeMenu.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/KeyBindings.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/OverrideMeth1.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/OverrideMeth2.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/OverrideMeth3.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/OverrideMeth4.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/OverrideMeth5.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/RegisterTemplate.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/TemplateList.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/WorkingDirectory.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/debug1.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/debug2.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/fig2.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/fig3.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/speedbar1.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/speedbar2.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/speedbar3.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/speedbar4.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/speedbar5.gif $(RELEASEDIR)/doc/html/jde-ug/images/
	$(CP) $(JDEDIR)/doc/html/jde-ug/images/classes_menu1.gif $(RELEASEDIR)/doc/html/jde-ug/images/
#
#       JDB User's Guide
#
	$(MKDIR) $(RELEASEDIR)/doc/html/jdb-ug
	$(CP) $(JDEDIR)/doc/html/jdb-ug/jdb-guide.html $(RELEASEDIR)/doc/html/jdb-ug/jdb-guide.html
#
#       Beanshell User's Guide
#
	$(MKDIR) $(RELEASEDIR)/doc/html/bsh-ug
	$(CP) $(JDEDIR)/doc/html/bsh-ug/bsh-ug.html $(RELEASEDIR)/doc/html/bsh-ug
	$(CP) $(JDEDIR)/doc/html/bsh-ug/bsh-ug-content.html $(RELEASEDIR)/doc/html/bsh-ug
	$(CP) $(JDEDIR)/doc/html/bsh-ug/bsh-ug-toc.html $(RELEASEDIR)/doc/html/bsh-ug

	$(MKDIR) $(RELEASEDIR)/doc/html/bsh-ug/images
	$(CP) $(JDEDIR)/doc/html/bsh-ug/images/BeanShellBuffer.gif $(RELEASEDIR)/doc/html/bsh-ug/images/
	$(CP) $(JDEDIR)/doc/html/bsh-ug/images/BshMultiLineEx.gif $(RELEASEDIR)/doc/html/bsh-ug/images/

#
#       JDEBug User's Guide
#
	$(MKDIR) $(RELEASEDIR)/doc/html/jdebug-ug
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/jdebug-ug.html $(RELEASEDIR)/doc/html/jdebug-ug
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/jdebug-ug-content.html $(RELEASEDIR)/doc/html/jdebug-ug
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/jdebug-ug-toc.html $(RELEASEDIR)/doc/html/jdebug-ug
	$(NEWLINE) $(RELEASEDIRDOS)/doc/html/jdebug-ug

	$(MKDIR) $(RELEASEDIR)/doc/html/jdebug-ug/images
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/images/enable_jdebug.gif  $(RELEASEDIR)/doc/html/jdebug-ug/images
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/images/jdebug_menu.gif  $(RELEASEDIR)/doc/html/jdebug-ug/images
	$(CP) $(JDEDIR)/doc/html/jdebug-ug/images/window_config.gif $(RELEASEDIR)/doc/html/jdebug-ug/images
#
# Tree List Applet
#
	$(MKDIR) $(RELEASEDIR)/doc/tli_rbl
	$(CP) $(JDEDIR)/doc/tli_rbl/tli_rbl.jar $(RELEASEDIR)/doc/tli_rbl
	$(MKDIR) $(RELEASEDIR)/doc/tli_rbl/au
	$(CP) $(JDEDIR)/doc/tli_rbl/au/*.au $(RELEASEDIR)/doc/tli_rbl/au
	$(MKDIR) $(RELEASEDIR)/doc/tli_rbl/img
	$(CP) $(JDEDIR)/doc/tli_rbl/img/*.gif $(RELEASEDIR)/doc/tli_rbl/img
	$(MKDIR) $(RELEASEDIR)/doc/tli_rbl/txt
	$(CP) $(JDEDIR)/doc/tli_rbl/txt/jde-ug-toc.txt $(RELEASEDIR)/doc/tli_rbl/txt
	$(CP) $(JDEDIR)/doc/tli_rbl/txt/jdebug-ug-toc.txt $(RELEASEDIR)/doc/tli_rbl/txt
#
#       Make jdebug
#
	$(MAKE) -f $(JDEDIR)/java/src/jde/debugger/makefile \
	-C $(JDEDIR)/java/src/jde/debugger all
#
#       Make utilities
#
	$(MAKE) -f $(JDEDIR)/java/src/jde/util/makefile \
	-C $(JDEDIR)/java/src/jde/util all
#
#       Make wizards
#
	$(MAKE) -f $(JDEDIR)/java/src/jde/wizards/makefile \
	-C $(JDEDIR)/java/src/jde/wizards all
#
# Make classes directory.
#
	$(MKDIR) $(RELEASEDIR)/java
	$(MKDIR) $(RELEASEDIR)/java/classes
	$(MKDIR) $(RELEASEDIR)/java/classes/jde
	$(MKDIR) $(RELEASEDIR)/java/classes/jde/wizards
	$(MKDIR) $(RELEASEDIR)/java/classes/jde/parser
	$(MKDIR) $(RELEASEDIR)/java/classes/jde/util
#
#
#       Jar files
#
	$(MKDIR) $(RELEASEDIR)/java/lib
	$(CP) $(JDEDIR)/java/lib/bsh.jar $(RELEASEDIR)/java/lib/
	$(JAR) -cvf $(JDEDIRDOS)/java/lib/jde.jar -C $(JDEDIRDOS)/java/classes jde
	$(CP) $(JDEDIR)/java/lib/jde.jar $(RELEASEDIR)/java/lib/
#
#       Wizards
#
	$(MKDIR) $(RELEASEDIR)/java/src
	$(MKDIR) $(RELEASEDIR)/java/src/jde
	$(MKDIR) $(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/ClassRegistry.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DefaultNameFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/InterfaceFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodOverrideFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/NameFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/Signature.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/ImportWizard.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DelegateFactory.java \
			$(RELEASEDIR)/java/src/jde/wizards
	$(NEWLINE) $(RELEASEDIRDOS)/java/src/jde/wizards

# Copy debugger
	$(CP) $(JDEDIR)/java/src/jde/debugger/ $(RELEASEDIR)/java/src/jde/
	$(RM) -r $(RELEASEDIR)/java/src/jde/debugger/cvs
	$(RM) -r $(RELEASEDIR)/java/src/jde/debugger/expr/cvs
	$(RM) -r $(RELEASEDIR)/java/src/jde/debugger/spec/cvs
	$(RM) -r $(RELEASEDIR)/java/src/jde/debugger/command/cvs

# Copy parser.
	$(MKDIR) $(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParser.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ASCII_UCodeESC_CharStream.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Java1.1.jj \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserConstants.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserTokenManager.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/jtb.out.jj \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParseException.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParserMain.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Token.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/TokenMgrError.java \
			$(RELEASEDIR)/java/src/jde/parser
	$(NEWLINE) $(RELEASEDIRDOS)/java/src/jde/parser

	$(MKDIR) $(RELEASEDIR)/java/src/jde/parser/syntaxtree
	$(CP) $(JDEDIR)/java/src/jde/parser/syntaxtree/*.java \
			$(RELEASEDIR)/java/src/jde/parser/syntaxtree
	$(NEWLINE) $(RELEASEDIRDOS)/java/src/jde/parser/syntaxtree

	$(MKDIR) $(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/DepthFirstVisitor.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeDumper.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeFormatter.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/Visitor.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariables.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariablesVisitor.java \
			$(RELEASEDIR)/java/src/jde/parser/visitor
	$(NEWLINE) $(RELEASEDIRDOS)/java/src/jde/parser/visitor

# Copy util.
	$(MKDIR) $(RELEASEDIR)/java/src/jde/util
	$(CP) $(JDEDIR)/java/src/jde/util/JdeUtilities.java \
			$(RELEASEDIR)/java/src/jde/util
	$(CP) $(JDEDIR)/java/src/jde/util/Completion.java \
			$(RELEASEDIR)/java/src/jde/util

	$(NEWLINE) $(RELEASEDIRDOS)/java/src/jde/util

# Copy Beanshell scripts
	$(MKDIR) $(RELEASEDIR)/java/bsh-commands
	$(MKDIR) $(RELEASEDIR)/java/bsh-commands/bsh
	$(MKDIR) $(RELEASEDIR)/java/bsh-commands/bsh/commands
	$(CP) $(JDEDIR)/java/bsh-commands/bsh/commands/*.* \
			$(RELEASEDIR)/java/bsh-commands/bsh/commands

	$(NEWLINE) $(RELEASEDIRDOS)/java/bsh-commands/bsh/commands
#
#	Zip Distribution
#
	$(TAR) -cvf $(DEVDIR)/distrib/$(RELEASENAME).tar -C $(DEVDIR)/distrib/zip $(RELEASENAME)
	$(GZIP) --force $(DEVDIR)/distrib/$(RELEASENAME).tar

	$(MAKE) zip \
		-C $(DEVDIR)/distrib/zip \
		-f $(JDEDIR)/makefile	
zip:
	rm -f $(DEVDIR)/distrib/$(RELEASENAME).zip
	$(ZIP) $(DEVDIR)/distrib/$(RELEASENAME).zip .




jde-jar:
	$(MAKE) jde-jar1 \
		-C $(DRIVE):/ \
		-f $(DRIVE):/jde-dev/jde/makefile	

jde-jar1:
	$(JAR) -cvf $(JDEDIRDOS)/java/lib/jde.jar -C $(JDEDIRDOS)/java/classes jde


xemacs:
	$(MAKE) xemacs1 \
		-C $(DRIVE):/ \
		-f $(DRIVE):/jde-dev/jde/makefile	
xemacs1:
#
#       Lisp code
#
	$(CP) $(JDEDIR)/lisp/jde.el $(XEMACSDIR)/lisp/jde.el
	$(CP) $(JDEDIR)/lisp/jde-run.el $(XEMACSDIR)/lisp/jde-run.el
	$(CP) $(JDEDIR)/lisp/jde-compile.el $(XEMACSDIR)/lisp/jde-compile.el
	$(CP) $(JDEDIR)/lisp/jde-db.el $(XEMACSDIR)/lisp/jde-db.el
	$(CP) $(JDEDIR)/lisp/jde-bug.el $(XEMACSDIR)/lisp/jde-bug.el
	$(CP) $(JDEDIR)/lisp/jde-dbs.el $(XEMACSDIR)/lisp/jde-dbs.el
	$(CP) $(JDEDIR)/lisp/jde-dbo.el $(XEMACSDIR)/lisp/jde-dbo.el
	$(CP) $(JDEDIR)/lisp/jde-make.el $(XEMACSDIR)/lisp/jde-make.el
	$(CP) $(JDEDIR)/lisp/jde-gen.el $(XEMACSDIR)/lisp/jde-gen.el
	$(CP) $(JDEDIR)/lisp/jde-wiz.el $(XEMACSDIR)/lisp/jde-wiz.el
	$(CP) $(JDEDIR)/lisp/jde-parse.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-java-grammar.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/beanshell.el $(XEMACSDIR)/lisp/beanshell.el
	$(CP) $(JDEDIR)/lisp/jde-help.el $(XEMACSDIR)/lisp/jde-help.el
	$(CP) $(JDEDIR)/lisp/jde-widgets.el $(XEMACSDIR)/lisp/jde-widgets.el
	$(CP) $(JDEDIR)/lisp/jde-complete.el $(XEMACSDIR)/lisp/jde-complete.el
	$(CP) $(JDEDIR)/lisp/jde-javadoc.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-javadoc-gen.el $(RELEASEDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-java-font-lock.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-which-method.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-imenu.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-import.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/jde-stat.el $(XEMACSDIR)/lisp/jde-stat.el
	$(CP) $(JDEDIR)/lisp/setnu.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/senator.el $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/java.bnf $(XEMACSDIR)/lisp/java.bnf
	$(CP) $(JDEDIR)/lisp/jtags $(XEMACSDIR)/lisp/jtags
	$(CP) $(JDEDIR)/lisp/jtags.csh $(XEMACSDIR)/lisp/jtags.csh
	$(CP) $(JDEDIR)/lisp/makefile.sample $(XEMACSDIR)/lisp/makefile.sample
	$(CP) $(JDEDIR)/lisp/makefile $(XEMACSDIR)/lisp
	$(CP) $(JDEDIR)/lisp/ReleaseNotes.txt $(XEMACSDIR)
#	$(CP) $(JDEDIR)/lisp/ChangeLog $(XEMACSDIR)/lisp/ChangeLog
	$(CP) $(EIEIODIR)/* $(XEMACSDIR)/lisp
	$(NEWLINE) $(XEMACSDIRDOS)/lisp
#
#       User's guide
#
	$(CP) $(JDEDIR)/doc/ug/bsh-ug.html $(XEMACSDIR)/doc/ug/bsh-ug.html
	$(CP) $(JDEDIR)/doc/ug/bsh-ug-content.html $(XEMACSDIR)/doc/ug/bsh-ug-content.html
	$(CP) $(JDEDIR)/doc/ug/bsh-ug-toc.html $(XEMACSDIR)/doc/ug/bsh-ug-toc.html
	$(CP) $(JDEDIR)/doc/ug/jde-ug-toc.html $(XEMACSDIR)/doc/ug/jde-ug-toc.html
	$(CP) $(JDEDIR)/doc/ug/jde-ug-content.html $(XEMACSDIR)/doc/ug/jde-ug-content.html
	$(CP) $(JDEDIR)/doc/ug/jde-ug.html $(XEMACSDIR)/doc/ug/jde-ug.html
	$(CP) $(JDEDIR)/doc/ug/jdb-guide.html $(XEMACSDIR)/doc/ug/jdb-guide.html
	$(NEWLINE) $(XEMACSDIRDOS)/doc/ug
	$(CP) $(JDEDIR)/doc/ug/images/BeanShellBuffer.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/BshMultiLineEx.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/JdeMenu.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/KeyBindings.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/OverrideMeth1.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/OverrideMeth2.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/OverrideMeth3.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/OverrideMeth4.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/OverrideMeth5.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/RegisterTemplate.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/TemplateList.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/WorkingDirectory.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/debug1.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/debug2.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/fig2.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/fig3.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/speedbar1.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/speedbar2.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/speedbar3.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/speedbar4.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/speedbar5.gif $(XEMACSDIR)/doc/ug/images/
	$(CP) $(JDEDIR)/doc/ug/images/classes_menu1.gif $(XEMACSDIR)/doc/ug/images/
#
#       JDE User's Guide - XML
#
	$(MKDIR) $(XEMACSDIR)/doc/src
	$(MKDIR) $(XEMACSDIR)/doc/src/ug
	$(CP) $(JDEDIR)/doc/src/ug/ug.xml $(XEMACSDIR)/doc/src/ug
	$(NEWLINE) $(XEMACSDIRDOS)/doc/ug
	$(MKDIR) $(XEMACSDIR)/doc/src/ug/images
	$(CP) $(JDEDIR)/doc/src/ug/images/BeanShellBuffer.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/BshMultiLineEx.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/JdeMenu.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/KeyBindings.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/OverrideMeth1.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/OverrideMeth2.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/OverrideMeth3.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/OverrideMeth4.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/OverrideMeth5.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/RegisterTemplate.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/TemplateList.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/WorkingDirectory.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/debug1.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/debug2.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/fig2.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/fig3.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/speedbar1.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/speedbar2.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/speedbar3.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/speedbar4.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/speedbar5.gif $(XEMACSDIR)/doc/src/ug/images/
	$(CP) $(JDEDIR)/doc/src/ug/images/classes_menu1.gif $(XEMACSDIR)/doc/src/ug/images/
#
#       JDEBug User's Guide
#
	$(CP) $(JDEDIR)/doc/jdebug/ug/jdebug-ug.html $(XEMACSDIR)/doc/jdebug/ug/jdebug-ug.html
	$(CP) $(JDEDIR)/doc/jdebug/ug/jdebug-ug-content.html \
		$(XEMACSDIR)/doc/jdebug/ug/jdebug-ug-content.html
	$(CP) $(JDEDIR)/doc/jdebug/ug/jdebug-ug-toc.html \
		$(XEMACSDIR)/doc/jdebug/ug/jdebug-ug-toc.html
	$(NEWLINE) $(XEMACSDIRDOS)/doc/jdebug/ug
	$(CP) $(JDEDIR)/doc/jdebug/ug/images/enable_jdebug.gif \
		$(XEMACSDIR)/doc/jdebug/ug/images/enable_jdebug.gif
	$(CP) $(JDEDIR)/doc/jdebug/ug/images/jdebug_menu.gif \
		$(XEMACSDIR)/doc/jdebug/ug/images/jdebug_menu.gif
	$(CP) $(JDEDIR)/doc/jdebug/ug/images/window_config.gif \
		$(XEMACSDIR)/doc/jdebug/ug/images/window_config.gif
#
# Tree List Applet
#
	$(CP) $(JDEDIR)/doc/tli_rbl/tli_rbl.jar $(XEMACSDIR)/doc/tli_rbl
	$(CP) $(JDEDIR)/doc/tli_rbl/au/*.au $(XEMACSDIR)/doc/tli_rbl/au
	$(CP) $(JDEDIR)/doc/tli_rbl/img/*.gif $(XEMACSDIR)/doc/tli_rbl/img
	$(CP) $(JDEDIR)/doc/tli_rbl/txt/jde-ug-toc.txt $(XEMACSDIR)/doc/tli_rbl/txt
	$(CP) $(JDEDIR)/doc/tli_rbl/txt/jdebug-ug-toc.txt $(XEMACSDIR)/doc/tli_rbl/txt
#
#       Make jdebug
#
	$(MAKE) -f $(JDEDIR)/java/src/jde/debugger/makefile \
	-C $(JDEDIR)/java/src/jde/debugger all
#
#
#       Jar files
#
	$(CP) $(JDEDIR)/java/lib/bsh.jar $(XEMACSDIR)/java/lib/
	$(JAR) -cvf $(JDEDIRDOS)/java/lib/jde.jar -C $(JDEDIRDOS)/java/classes jde
	$(CP) $(JDEDIR)/java/lib/jde.jar $(XEMACSDIR)/java/lib/
#
#        Java Source documentation.
#
	$(CP) $(JDEDIR)/java/doc/jde/debugger/*.* $(XEMACSDIR)/java/doc/jde/debugger
	$(CP) $(JDEDIR)/java/doc/jde/debugger/command/*.* $(XEMACSDIR)/java/doc/jde/debugger/command
	$(CP) $(JDEDIR)/java/doc/jde/debugger/expr/*.* $(XEMACSDIR)/java/doc/jde/debugger/expr
	$(CP) $(JDEDIR)/java/doc/jde/debugger/expr/*.* $(XEMACSDIR)/java/doc/jde/debugger/spec
#
#       Wizards
#
	$(CP) $(JDEDIR)/java/src/jde/wizards/ClassRegistry.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DefaultNameFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/InterfaceFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodOverrideFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/NameFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/Signature.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/ImportWizard.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DelegateFactory.java \
			$(XEMACSDIR)/java/src/jde/wizards
	$(NEWLINE) $(XEMACSDIRDOS)/java/src/jde/wizards

# Copy debugger
	$(CP) $(JDEDIR)/java/src/jde/debugger/*.* $(XEMACSDIR)/java/src/jde/debugger
	$(CP) $(JDEDIR)/java/src/jde/debugger/command/*.* $(XEMACSDIR)/java/src/jde/debugger/command
	$(CP) $(JDEDIR)/java/src/jde/debugger/expr/*.* $(XEMACSDIR)/java/src/jde/debugger/expr
	$(CP) $(JDEDIR)/java/src/jde/debugger/spec/*.* $(XEMACSDIR)/java/src/jde/debugger/spec

# Copy parser.
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParser.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ASCII_UCodeESC_CharStream.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Java1.1.jj \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserConstants.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserTokenManager.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/jtb.out.jj \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParseException.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParserMain.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Token.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/TokenMgrError.java \
			$(XEMACSDIR)/java/src/jde/parser
	$(NEWLINE) $(XEMACSDIRDOS)/java/src/jde/parser

	$(CP) $(JDEDIR)/java/src/jde/parser/syntaxtree/*.java \
			$(XEMACSDIR)/java/src/jde/parser/syntaxtree
	$(NEWLINE) $(XEMACSDIRDOS)/java/src/jde/parser/syntaxtree

	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/DepthFirstVisitor.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeDumper.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeFormatter.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/Visitor.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariables.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariablesVisitor.java \
			$(XEMACSDIR)/java/src/jde/parser/visitor
	$(NEWLINE) $(XEMACSDIRDOS)/java/src/jde/parser/visitor

# Copy util.
	$(CP) $(JDEDIR)/java/src/jde/util/JdeUtilities.java \
			$(XEMACSDIR)/java/src/jde/util
	$(CP) $(JDEDIR)/java/src/jde/util/Completion.java \
			$(XEMACSDIR)/java/src/jde/util
	$(NEWLINE) $(XEMACSDIRDOS)/java/src/jde/util

# Copy Beanshell scripts
	$(CP) $(JDEDIR)/java/bsh-commands/bsh/commands/*.* \
			$(XEMACSDIR)/java/bsh-commands/bsh/commands

	$(NEWLINE) $(XEMACSDIRDOS)/java/bsh-commands/bsh/commands

#
#
#       Jar files
#
	$(CP) $(JDEDIR)/java/lib/bsh.jar $(XEMACSDIR)/etc/java/lib/
	$(CP) $(JDEDIR)/java/lib/jde.jar $(XEMACSDIR)/etc/java/lib/
#
#        Java Source documentation.
#
	$(CP) $(JDEDIR)/java/doc/jde/debugger/*.* $(XEMACSDIR)/etc/java/doc/jde/debugger
	$(CP) $(JDEDIR)/java/doc/jde/debugger/command/*.* $(XEMACSDIR)/etc/java/doc/jde/debugger/command
	$(CP) $(JDEDIR)/java/doc/jde/debugger/expr/*.* $(XEMACSDIR)/etc/java/doc/jde/debugger/expr
	$(CP) $(JDEDIR)/java/doc/jde/debugger/expr/*.* $(XEMACSDIR)/etc/java/doc/jde/debugger/spec
#
#       Wizards
#
	$(CP) $(JDEDIR)/java/src/jde/wizards/ClassRegistry.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DefaultNameFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/InterfaceFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/MethodOverrideFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/NameFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/Signature.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/ImportWizard.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(CP) $(JDEDIR)/java/src/jde/wizards/DelegateFactory.java \
			$(XEMACSDIR)/etc/java/src/jde/wizards
	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/src/jde/wizards

# Copy debugger
	$(CP) $(JDEDIR)/java/src/jde/debugger/*.* $(XEMACSDIR)/etc/java/src/jde/debugger
	$(CP) $(JDEDIR)/java/src/jde/debugger/command/*.* $(XEMACSDIR)/etc/java/src/jde/debugger/command
	$(CP) $(JDEDIR)/java/src/jde/debugger/expr/*.* $(XEMACSDIR)/etc/java/src/jde/debugger/expr
	$(CP) $(JDEDIR)/java/src/jde/debugger/spec/*.* $(XEMACSDIR)/etc/java/src/jde/debugger/spec

# Copy parser.
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParser.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ASCII_UCodeESC_CharStream.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Java1.1.jj \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserConstants.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/JavaParserTokenManager.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/jtb.out.jj \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParseException.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/ParserMain.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/Token.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(CP) $(JDEDIR)/java/src/jde/parser/TokenMgrError.java \
			$(XEMACSDIR)/etc/java/src/jde/parser
	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/src/jde/parser

	$(CP) $(JDEDIR)/java/src/jde/parser/syntaxtree/*.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/syntaxtree
	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/src/jde/parser/syntaxtree

	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/DepthFirstVisitor.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeDumper.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/TreeFormatter.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/Visitor.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariables.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(CP) $(JDEDIR)/java/src/jde/parser/visitor/GetVariablesVisitor.java \
			$(XEMACSDIR)/etc/java/src/jde/parser/visitor
	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/src/jde/parser/visitor

# Copy util.
	$(CP) $(JDEDIR)/java/src/jde/util/JdeUtilities.java \
			$(XEMACSDIR)/etc/java/src/jde/util
	$(CP) $(JDEDIR)/java/src/jde/util/Completion.java \
			$(XEMACSDIR)/etc/java/src/jde/util
	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/src/jde/util

# Copy Beanshell scripts
	$(CP) $(JDEDIR)/java/bsh-commands/bsh/commands/*.* \
			$(XEMACSDIR)/etc/java/bsh-commands/bsh/commands

	$(NEWLINE) $(XEMACSDIRDOS)/etc/java/bsh-commands/bsh/commands



