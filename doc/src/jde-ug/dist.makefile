# Builds the JDE user guide portion of the JDE distribution.

IMAGESDIR = images
TEXT = jde-ug.html jde-ug-content.xml



all:
	$(MAKE) text -f $(JDEDIR)/doc/src/jde-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/src/jde-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/src/jde-ug/images/dist.makefile


%.xml : $(JDEDIR)/doc/src/jde-ug/%.xml
	$(CP) $< .
	$(NEWLINE) $@

%.html : $(JDEDIR)/doc/src/jde-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
