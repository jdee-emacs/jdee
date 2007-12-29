# Builds the jdb user guide portion of the JDEE distribution.

IMAGESDIR = images
TEXT = jdb-ug-frame.html jdb-ug.xml



all:
	$(MAKE) text -f $(JDEDIR)/doc/src/jdb-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/src/jdb-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/src/jdb-ug/images/dist.makefile


%.xml : $(JDEDIR)/doc/src/jdb-ug/%.xml
	$(CP) $< .
	$(NEWLINE) $@

%.html : $(JDEDIR)/doc/src/jdb-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
