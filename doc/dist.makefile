# Build the documentation subdirectory of the JDE
# distribution.

DOCDEVDIR = $(JDEDIR)/doc
SRCDIR = src
HTMLDIR = html
TLDIR = tli_rbl

SUBDIRS = $(SRCDIR) \
	  $(HTMLDIR) \
	  $(TLDIR)

all: .nosearch $(SUBDIRS)
#
#	Documentation source directory
#	
	$(MAKE)  \
		-C $(SRCDIR) \
		-f $(DOCDEVDIR)/$(SRCDIR)/dist.makefile
#
#	HTML directory
#	
	$(MAKE)  \
		-C $(HTMLDIR) \
		-f $(DOCDEVDIR)/$(HTMLDIR)/dist.makefile
#
#	Tree List Directory
#	
	$(MAKE)  \
		-C $(TLDIR) \
		-f $(DOCDEVDIR)/$(TLDIR)/dist.makefile



$(SRCDIR):
	$(MKDIR) $@

$(HTMLDIR):
	$(MKDIR) $@

$(TLDIR):
	$(MKDIR) $@

.nosearch : $(DOCDEVDIR)/.nosearch
	$(CP) $< .



