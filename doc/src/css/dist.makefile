# Build cascading stylesheet section of the JDE
# distribution.

STYLESHEETS = jde_style.css

all: $(STYLESHEETS)

%.css : $(JDEDIR)/doc/src/css/%.css
	$(CP) $< .
	$(NEWLINE) $@
