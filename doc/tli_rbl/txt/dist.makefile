# Build text section of Tree List
# directory.

TEXTFILES = jde-ug-toc.txt \
	    jdebug-ug-toc.txt \
	    jdb-ug-toc.txt

all: $(TEXTFILES)

%.txt : $(JDEDIR)/doc/tli_rbl/txt/%.txt
	$(CP) $< .