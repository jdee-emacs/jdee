# Build the section of the JDE distribution
# containing the HTML version of the JDE
# user guide.

IMAGESDIR = images
TEXT = jde-ug.html jde-ug-content.html jde-ug-toc.html



all:
	$(MAKE) \
		-C $(JDEDIR)/doc/src/jde-ug \
		-f $(JDEDIR)/doc/src/jde-ug/html.makefile
	$(MAKE) text -f $(JDEDIR)/doc/html/jde-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/html/jde-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/html/jde-ug/images/dist.makefile


%.html : $(JDEDIR)/doc/html/jde-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
