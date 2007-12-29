# Build the HTML version of the JDE User Guide.

JAVA = java
SAXON_JAR = $(XAEDEV)/xae/xsl-engines/saxon/saxon.jar
SAXON_CLASS = com.icl.saxon.StyleSheet
DOCDIR = ../..
CP = cp
HTML_DIR = $(DOCDIR)/html
STYLES_DIR = $(DOCDIR)/src/styles

HTMLFILES = jde-ug-content.html \
            jde-ug-toc.html

all: 
	$(MAKE) html \
	        -C $(HTML_DIR)/jde-ug \
		-f $(DOCDIR)/src/jde-ug/html.makefile \
		DOCDIR=$(DOCDIR)
	$(MAKE) -f $(DOCDIR)/src/jde-ug/tltoc.makefile
	$(MAKE)  \
		-C $(HTML_DIR)/jde-ug/images \
		-f ../../../src/jde-ug/images/html.makefile


html: $(HTMLFILES)


jde-ug-content.html : $(DOCDIR)/src/jde-ug/jde-ug-content.xml $(STYLES_DIR)/html/jdebook.xsl
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) -w0 -a -o $@ $<

jde-ug-toc.html : $(DOCDIR)/src/jde-ug/jde-ug-content.xml $(STYLES_DIR)/html/jdebook_html_toc.xsl
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) -w0 \
		-o $@  $< $(STYLES_DIR)/html/jdebook_html_toc.xsl