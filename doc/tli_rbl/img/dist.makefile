# Build images section of Tree List
# directory.

IMAGES = icon.gif icon2.gif icon3.gif photo.gif

all: $(IMAGES)

%.gif : $(JDEDIR)/doc/tli_rbl/img/%.gif
	$(CP) $< .