# This makefile builds the JDE distributable.
# tr -d '\015\032' < input > output 

export DRIVE = c
export PERL = perl
export CP = cp -r
export RM = rm
export MKDIR = mkdir -p
export TAR = tar
export GZIP = gzip
export ZIP = zip -v -r
export JAR = jar
export VERSION = 2.3.5.1
export RELEASENAME = jde-$(VERSION)
export DEVDIR = $(JDEDEV)
DEVDIRDOS = $(subst \,/,$(shell cygpath -w $(DEVDIR)))
export JDEDIR = $(DEVDIR)/jde
export JDEDIRDOS = $(DEVDIRDOS)\jde
export RELEASEDIR = $(DEVDIR)/distrib/zip/$(RELEASENAME)
export RELEASEDIRDOS = $(DEVDIRDOS)/distrib/zip/$(RELEASENAME)
export DOS2UNIX = $(PERL) $(DEVDIRDOS)/dos2unix.pl
export NEWLINE = /cygdrive/c/applications/winutils/newline /r /1

RELEASEDIRS = $(RELEASEDIR) \
	      $(RELEASEDIR)/lisp \
	      $(RELEASEDIR)/doc \
	      $(RELEASEDIR)/plugins \
	      $(RELEASEDIR)/java 


all: $(RELEASEDIRS)
#
#	Lisp directory
#
	$(MAKE) \
		-C $(RELEASEDIR)/lisp \
		-f $(JDEDIR)/lisp/dist.makefile
#
#	Documentation directory
#	
	$(MAKE)  \
		-C $(RELEASEDIR)/doc \
		-f $(JDEDIR)/doc/dist.makefile
#
#	Java directory
#	
	$(MAKE)  \
		-C $(RELEASEDIR)/java \
		-f $(JDEDIR)/java/dist.makefile
#
#
#
	$(MAKE) zipfile \
		-C $(DEVDIR)/distrib/zip \
		-f $(DEVDIR)/jde/dist.makefile	
#
#	Tarball
#
	$(TAR) -cvf $(DEVDIR)/distrib/$(RELEASENAME).tar -C $(DEVDIR)/distrib/zip $(RELEASENAME)
	$(GZIP) --force $(DEVDIR)/distrib/$(RELEASENAME).tar

zipfile:
	rm -f $(DEVDIR)/distrib/$(RELEASENAME).zip
	$(ZIP) $(RELEASENAME).zip $(RELEASENAME)
	mv zip.zip ../$(RELEASENAME).zip


$(RELEASEDIR):
	$(MKDIR) $@

$(RELEASEDIR)/lisp: $(RELEASEDIR)
	$(MKDIR) $@

$(RELEASEDIR)/doc: $(RELEASEDIR)
	$(MKDIR) $@

$(RELEASEDIR)/java: $(RELEASEDIR)
	$(MKDIR) $@

$(RELEASEDIR)/plugins: $(RELEASEDIR)
	$(MKDIR) $@





