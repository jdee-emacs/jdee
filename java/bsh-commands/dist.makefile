# Build Beanshell Commands subdirectory of the JDE distribution.

CMDSDIR = bsh/commands

all: ./$(CMDSDIR)
	$(MAKE) \
		-C ./$(CMDSDIR) \
		-f $(JDEDEV)/jde/java/bsh-commands/$(CMDSDIR)/dist.makefile

./$(CMDSDIR):
	$(MKDIR) $@
