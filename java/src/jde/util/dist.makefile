# Build section of distributable containing the Java source for
# utility classes.

SRC =   ClassPathDir.java \
	ClassPathEntry.java \
	ClassPathZip.java \
	CompileServer.java \
	Completion.java \
	ClassInfo.java \
	DynamicClassLoader.java \
	ImmutableClassPathEntry.java \
	JdeUtilities.java \
	MultiValueMap.java \
        AntServer.java \
	ProjectClasses.java

all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/util/%.java
	$(CP) $< .
	$(NEWLINE) $@


