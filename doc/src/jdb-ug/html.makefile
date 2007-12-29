# Build the HTML version of the JDB User Guide.

JAVA = java
SAXON_JAR = $(XAEDEV)/xae/xsl-engines/saxon/saxon.jar
SAXON_CLASS = com.icl.saxon.StyleSheet
DOCDIR = ../..
CP = cp
HTML_DIR = $(DOCDIR)/html
STYLES_DIR = $(DOCDIR)/src/styles

HTMLFILES = jdb-ug.html \
            jdb-ug-toc.html

all: 
	$(MAKE) html \
	        -C $(HTML_DIR)/jdb-ug \
		-f $(DOCDIR)/src/jdb-ug/html.makefile \
		DOCDIR=$(DOCDIR)
	$(MAKE) -f $(DOCDIR)/src/jdb-ug/tltoc.makefile
	$(MAKE)  \
		-C $(HTML_DIR)/jdb-ug/images \
		-f ../../../src/jdb-ug/images/html.makefile


html: $(HTMLFILES)


jdb-ug.html : $(DOCDIR)/src/jdb-ug/jdb-ug.xml
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) -w0 -a -o $@ $<

jdb-ug-toc.html : $(DOCDIR)/src/jdb-ug/jdb-ug.xml
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) -w0 \
		-o $@  $< $(STYLES_DIR)/html/jdb-ug-html-toc.xsl