# Build the secton of the JDE distribution containing the
# images for the HTML version of the JDEbug User's Guide.


IMAGES =  BeanShellBuffer.gif \
          BshMultiLineEx.gif \
	  bshclassloading.gif

all: $(IMAGES)

%.gif : $(JDEDIR)/doc/html/bsh-ug/images/%.gif
	$(CP) $< .