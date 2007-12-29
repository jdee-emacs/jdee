JAVA = java
SAXON_JAR = d:/tpe-dev/tpe/java/lib/saxon.jar
SAXON_CLASS = com.icl.saxon.StyleSheet
DOC_DIR = ../..
TREE_VIEW_DIR = $(DOC_DIR)/tli_rbl
CP = cp
HTML_DIR = $(DOC_DIR)/html
STYLES_DIR = $(DOC_DIR)/src/styles


all:
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) \
		-a -o $(HTML_DIR)/jde-ug/jde-ug-content.html \
		jde-ug-content.xml
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) \
		-o $(TREE_VIEW_DIR)/txt/jde-ug-toc.txt \
		jde-ug-content.xml \
		$(STYLES_DIR)/html/jdebook_toc.xsl
	$(JAVA) -classpath  $(SAXON_JAR) $(SAXON_CLASS) \
		-o $(HTML_DIR)/jde-ug/jde-ug-toc.html \
		jde-ug-content.xml \
		$(STYLES_DIR)/html/jdebook_html_toc.xsl
	$(CP) jde-ug.html $(HTML_DIR)/jde-ug
	$(CP) ./images/*.* $(HTML_DIR)/jde-ug/images
