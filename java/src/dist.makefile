# Build the java src subdirectory of the JDE
# distribution.

JDEPKGDIR = jde
SUBDIRS = $(JDEPKGDIR)

all: $(SUBDIRS)
#
#	jde package
#	
	$(MAKE)  \
		-C $(JDEPKGDIR) \
		-f $(JDEDIR)/java/src/$(JDEPKGDIR)/dist.makefile

$(JDEPKGDIR) :
	$(MKDIR) $@

