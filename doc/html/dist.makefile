# Builds the html documentation portion of the JDE distribution.

HTMLDEVDIR = $(JDEDIR)/doc/html
JDEUGDIR = jde-ug
JDEBUGUGDIR = jdebug-ug
JDBUGDIR = jdb-ug
BSHUGDIR = bsh-ug
CSSDIR = css
SUBDIRS = $(JDEUGDIR) \
	  $(JDEBUGUGDIR) \
	  $(JDBUGDIR) \
	  $(BSHUGDIR) \
	  $(CSSDIR)

all: $(SUBDIRS)
#
#	JDE User's Guide
#	
	$(MAKE)  \
		-C $(JDEUGDIR) \
		-f $(HTMLDEVDIR)/$(JDEUGDIR)/dist.makefile
#
#	JDEbug User's Guide
#	
	$(MAKE)  \
		-C $(JDEBUGUGDIR) \
		-f $(HTMLDEVDIR)/$(JDEBUGUGDIR)/dist.makefile
#
#	Jdb User's Guide
#	
	$(MAKE)  \
		-C $(JDBUGDIR) \
		-f $(HTMLDEVDIR)/$(JDBUGDIR)/dist.makefile
#
#	Beanshell User's Guide
#	
	$(MAKE)  \
		-C $(BSHUGDIR) \
		-f $(HTMLDEVDIR)/$(BSHUGDIR)/dist.makefile
#
#	Cascading Style Sheets
#	
	$(MAKE)  \
		-C $(CSSDIR) \
		-f $(HTMLDEVDIR)/$(CSSDIR)/dist.makefile



$(JDEUGDIR):
	$(MKDIR) $@

$(JDEBUGUGDIR):
	$(MKDIR) $@

$(JDBUGDIR):
	$(MKDIR) $@

$(BSHUGDIR):
	$(MKDIR) $@

$(CSSDIR):
	$(MKDIR) $@



