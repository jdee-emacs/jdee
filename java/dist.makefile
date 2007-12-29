# Build the java subdirectory of the JDE
# distribution.

JAVADEVDIR = $(JDEDIR)/java
SRCDIR = src
LIBDIR = lib
BSH-COMMANDS-DIR = bsh-commands
CLASSDIR = classes

SUBDIRS = $(SRCDIR) \
	  $(LIBDIR) \
	  $(CLASSDIR) \
	  $(BSH-COMMANDS-DIR)

all: .nosearch $(SUBDIRS) 
#
#	Source directory
#	
	$(MAKE)  \
		-C $(SRCDIR) \
		-f $(JAVADEVDIR)/$(SRCDIR)/dist.makefile
#
#	Lib directory
#	
	$(MAKE)  \
		-C $(LIBDIR) \
		-f $(JAVADEVDIR)/$(LIBDIR)/dist.makefile
#
#	BeanShell Commands Directory
#	
	$(MAKE)  \
		-C $(BSH-COMMANDS-DIR) \
		-f $(JAVADEVDIR)/$(BSH-COMMANDS-DIR)/dist.makefile



$(SRCDIR):
	$(MKDIR) $@

$(LIBDIR):
	$(MKDIR) $@

$(CLASSDIR):
	$(MKDIR) $@

$(BSH-COMMANDS-DIR):
	$(MKDIR) $@

.nosearch : $(JAVADEVDIR)/.nosearch
	$(CP) $< .



