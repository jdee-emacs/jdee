# Build section of distributable containing the Java source for
# debugger commands.

SRC =   AccessWatchpointSpec.java \
	BreakpointSpec.java \
	EventRequestSpec.java \
	EventRequestSpecList.java \
	ExceptionSpec.java \
	LineBreakpointSpec.java \
	MethodBreakpointSpec.java \
	ModificationWatchpointSpec.java \
	PatternReferenceTypeSpec.java \
	ReferenceTypeSpec.java \
	SourceNameReferenceTypeSpec.java \
	WatchpointSpec.java
 

all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/debugger/spec/%.java
	$(CP) $< .
	$(NEWLINE) $@


