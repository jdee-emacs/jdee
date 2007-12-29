# Build the jde package source directory.

DEBUGGERDIR = debugger
PARSERDIR = parser
UTILDIR = util
WIZARDSDIR = wizards

SUBDIRS = $(DEBUGGERDIR) $(UTILDIR) $(WIZARDSDIR)

all: $(SUBDIRS)
#
#	debugger package
#	
	$(MAKE)  \
		-C $(DEBUGGERDIR) \
		-f $(JDEDIR)/java/src/jde/$(DEBUGGERDIR)/dist.makefile
#
#	parser package
#	
#	$(MAKE)  \
#		-C $(PARSERDIR) \
#		-f $(JDEDIR)/java/src/jde/$(PARSERDIR)/dist.makefile
#
#	util package
#	
	$(MAKE)  \
		-C $(UTILDIR) \
		-f $(JDEDIR)/java/src/jde/$(UTILDIR)/dist.makefile
#
#	wizards package
#	
	$(MAKE)  \
		-C $(WIZARDSDIR) \
		-f $(JDEDIR)/java/src/jde/$(WIZARDSDIR)/dist.makefile


$(DEBUGGERDIR) :
	$(MKDIR) $@

$(PARSERDIR) :
	$(MKDIR) $@

$(UTILDIR) :
	$(MKDIR) $@

$(WIZARDSDIR) :
	$(MKDIR) $@



