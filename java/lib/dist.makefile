# Build the java lib directory of the JDE 
# distribution.

JARFILES = bsh.jar jde.jar checkstyle-all.jar junit.jar

all: 
	$(MAKE)  \
		-C $(JDEDIR)/java/lib \
		-f $(JDEDIR)/java/lib/jde_jar.makefile
	$(MAKE) jars  -f $(JDEDIR)/java/lib/dist.makefile
	$(CP) $(JDEDIR)/java/lib/LICENSE.checkstyle .
	$(CP) $(JDEDIR)/java/lib/LICENSE.apache .
	$(CP) $(JDEDIR)/java/lib/RIGHTS.antlr .
	$(CP) $(JDEDIR)/java/lib/sun_checks.xml .



jars : $(JARFILES)

%.jar : $(JDEDIR)/java/lib/%.jar
	$(CP) $< .
