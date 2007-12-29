# Build the Tree List Table of Contents for the JDB User Guide.

JAVA = java
SAXON_JAR = $(XAEDEV)/xae/xsl-engines/saxon/saxon.jar
SAXON_CLASS = com.icl.saxon.StyleSheet
DOCDIR = ../..
TREEVIEW_DIR = $(DOCDIR)/tli_rbl
CP = cp
STYLES_DIR = $(DOCDIR)/src/styles

all: 
	$(MAKE) treelist \
		-C $(TREEVIEW_DIR)/txt \
		-f $(DOCDIR)/src/jdb-ug/tltoc.makefile \
		DOCDIR=$(DOCDIR)


treelist: jdb-ug-toc.txt

jdb-ug-toc.txt : $(DOCDIR)/src/jdb-ug/jdb-ug.xml
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) \
		-o $@ $< $(STYLES_DIR)/html/jdb-ug-toc.xsl
