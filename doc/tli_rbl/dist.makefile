# Builds the Tree List section of the JDE
# distributable directory.

TLDEVDIR = $(JDEDIR)/doc/tli_rbl
AUDIODIR = au
IMAGEDIR = img
TEXTDIR = txt

SUBDIRS = $(AUDIODIR) \
	  $(IMAGEDIR) \
	  $(TEXTDIR)

all: $(SUBDIRS) tli_rbl.jar
#
#	Audio directory
#	
	$(MAKE)  \
		-C $(AUDIODIR) \
		-f $(TLDEVDIR)/$(AUDIODIR)/dist.makefile
#
#	Images directory
#	
	$(MAKE)  \
		-C $(IMAGEDIR) \
		-f $(TLDEVDIR)/$(IMAGEDIR)/dist.makefile
#
#	Text directory
#	
	$(MAKE) \
		-C $(JDEDIR)/doc/src/jde-ug \
		-f $(JDEDIR)/doc/src/jde-ug/tltoc.makefile
	$(MAKE)  \
		-C $(TEXTDIR) \
		-f $(TLDEVDIR)/$(TEXTDIR)/dist.makefile

%.jar : $(TLDEVDIR)/%.jar
	$(CP) $< .

$(AUDIODIR):
	$(MKDIR) $@

$(IMAGEDIR):
	$(MKDIR) $@


$(TEXTDIR):
	$(MKDIR) $@

