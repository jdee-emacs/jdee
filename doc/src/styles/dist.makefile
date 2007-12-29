# Build XSL stylesheet portion of JDE distribution.

HTMLSTYLEDIR = html
STYLEDIRS = $(HTMLSTYLEDIR)

all: $(STYLEDIRS)
#
#	HTML Styles
#	
	$(MAKE)  \
		-C $(HTMLSTYLEDIR) \
		-f $(JDEDIR)/doc/src/styles/$(HTMLSTYLEDIR)/dist.makefile

$(HTMLSTYLEDIR):
	$(MKDIR) $@
