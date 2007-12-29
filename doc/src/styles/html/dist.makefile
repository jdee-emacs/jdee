# Build section of JDE distribution containing 
# XSL stylesheets for JDE documentation.

STYLESHEETS = jdebook.xsl \
	      jdebook_html_toc.xsl \
	      jdebook_toc.xsl

all: $(STYLESHEETS)

%.xsl : $(JDEDIR)/doc/src/styles/html/%.xsl
	$(CP) $< .
	$(NEWLINE) $@
