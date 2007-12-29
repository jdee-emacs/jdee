# Builds Beanshell commands subdirectory of the JDE 
# distribution.

CMDS = 	exploreClass.bsh \
	beanInfoMaker.bsh \
	whichClass.bsh

all: $(CMDS)

%.bsh : $(JDEDEV)/jde/java/bsh-commands/bsh/commands/%.bsh
	$(CP) $< .
