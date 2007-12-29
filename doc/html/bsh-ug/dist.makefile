# Build the section of the JDE distribution
# containing the HTML version of the Beanshell
# user guide.

IMAGESDIR = images
TEXT = bsh-ug.html bsh-ug-content.html bsh-ug-toc.html



all:
	$(MAKE) text -f $(JDEDIR)/doc/html/bsh-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/html/bsh-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/html/bsh-ug/images/dist.makefile


%.html : $(JDEDIR)/doc/html/bsh-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
