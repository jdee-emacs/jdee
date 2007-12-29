# Build the Tree List Table of Contents for the JDE User Guide.

JAVA = java
SAXON_JAR = c:/home/jde-dev/lib/saxon.jar
SAXON_CLASS = net.sf.saxon.Transform
DOCDIR = ../..
TREEVIEW_DIR = $(DOCDIR)/tli_rbl
CP = cp
STYLES_DIR = $(DOCDIR)/src/styles

all: 
	$(MAKE) treelist \
		-C $(TREEVIEW_DIR)/txt \
		-f $(DOCDIR)/src/jde-ug/tltoc.makefile \
		DOCDIR=$(DOCDIR)


treelist: jde-ug-toc.txt

jde-ug-toc.txt : $(DOCDIR)/src/jde-ug/jde-ug-content.xml
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) \
		-o $@ $< $(STYLES_DIR)/html/jdebook_toc.xsl
