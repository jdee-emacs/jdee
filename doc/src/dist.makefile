# Build the documentation source portion of the JDEE
# distribution.

JDEUGDIR = jde-ug
JDBUGDIR = jdb-ug
CSSDIR = css
STYLESDIR = styles

DOCSRCDIRS = $(JDEUGDIR) \
	     $(JDBUGDIR) \
	     $(CSSDIR) \
	     $(STYLESDIR)


all: $(DOCSRCDIRS)
#
#	JDEE User's Guide
#	
	$(MAKE)  \
		-C $(JDEUGDIR) \
		-f $(JDEDIR)/doc/src/$(JDEUGDIR)/dist.makefile

#
#	JDB User's Guide
#	
	$(MAKE)  \
		-C $(JDBUGDIR) \
		-f $(JDEDIR)/doc/src/$(JDBUGDIR)/dist.makefile
#
#	Cascading Style Sheets
#	
	$(MAKE)  \
		-C $(CSSDIR) \
		-f $(JDEDIR)/doc/src/$(CSSDIR)/dist.makefile
#
#	XSL Style Sheets
#	
	$(MAKE)  \
		-C $(STYLESDIR) \
		-f $(JDEDIR)/doc/src/$(STYLESDIR)/dist.makefile



$(JDEUGDIR):
	$(MKDIR) $@

$(JDBUGDIR):
	$(MKDIR) $@


$(CSSDIR):
	$(MKDIR) $@

$(STYLESDIR):
	$(MKDIR) $@



