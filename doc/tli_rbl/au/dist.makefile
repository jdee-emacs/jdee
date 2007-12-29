# Build audio section of Tree List
# directory.

AUDIOFILES = link.au tree.au

all: $(AUDIOFILES)

%.au : $(JDEDIR)/doc/tli_rbl/au/%.au
	$(CP) $< .