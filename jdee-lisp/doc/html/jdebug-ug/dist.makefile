# Build the section of the JDE distribution
# containing the HTML version of the JDEbug
# user guide.

IMAGESDIR = images
TEXT = jdebug-ug.html jdebug-ug-content.html jdebug-ug-toc.html



all:
	$(MAKE) text -f $(JDEDIR)/doc/html/jdebug-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/html/jdebug-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/html/jdebug-ug/images/dist.makefile


%.html : $(JDEDIR)/doc/html/jdebug-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
