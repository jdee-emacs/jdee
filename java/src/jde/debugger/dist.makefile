# Build the debugger package portion of the JDE distributable.

DEVDIR = $(JDEDIR)/java/src/jde/debugger
CMDDIR = command
GUIDIR = gui
EXPRDIR = expr
SPECDIR = spec

SUBDIRS = $(CMDDIR) $(GUIDIR) $(EXPRDIR) $(SPECDIR)

SRC =	CommandStream.java \
	Debugger.java \
	DebuggeeSIO.java \
	DisplayableValue.java \
	Etc.java \
	EventHandler.java  \
	JDE.java \
	JDEbug.java \
	JDEException.java \
	JDENumberFormatException.java \
	Main.java \
	ObjectStore.java  \
	ProcessRegistry.java \
	Protocol.java \
	Rep.java \
	SessionManager.java \
	VMUtil.java


all: $(SRC) $(SUBDIRS)
#
#	command package
#	
	$(MAKE)  \
		-C $(CMDDIR) \
		-f $(DEVDIR)/$(CMDDIR)/dist.makefile
#
#	GUI package
#	
	$(MAKE)  \
		-C $(CMDDIR) \
		-f $(DEVDIR)/$(GUIDIR)/dist.makefile
#
#	expression evaluator package
#	
	$(MAKE)  \
		-C $(EXPRDIR) \
		-f $(DEVDIR)/$(EXPRDIR)/dist.makefile
#
#	spec package
#	
	$(MAKE)  \
		-C $(SPECDIR) \
		-f $(DEVDIR)/$(SPECDIR)/dist.makefile

$(CMDDIR) :
	$(MKDIR) $@

$(GUIDIR) :
	$(MKDIR) $@

$(EXPRDIR) :
	$(MKDIR) $@

$(SPECDIR) :
	$(MKDIR) $@

%.java : $(JDEDIR)/java/src/jde/debugger/%.java
	$(CP) $< .
	$(NEWLINE) $@




