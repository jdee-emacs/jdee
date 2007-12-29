# Build the section of the JDEE distribution
# containing the HTML version of the JDB User's Guide.

IMAGESDIR = images
TEXT = jdb-ug-frame.html jdb-ug.html jdb-ug-toc.html



all:
	$(MAKE) \
		-C $(JDEDIR)/doc/src/jdb-ug \
		-f $(JDEDIR)/doc/src/jdb-ug/html.makefile
	$(MAKE) text -f $(JDEDIR)/doc/html/jdb-ug/dist.makefile
	$(MAKE) imagesdir -f $(JDEDIR)/doc/html/jdb-ug/dist.makefile

text: $(TEXT)

imagesdir: $(IMAGESDIR)
	$(MAKE)  \
		-C images \
		-f $(JDEDIR)/doc/html/jdb-ug/images/dist.makefile


%.html : $(JDEDIR)/doc/html/jdb-ug/%.html
	$(CP) $< .
	$(NEWLINE) $@

$(IMAGESDIR):
	$(MKDIR) $@
