# Build section of distributable containing the Java source for
# debugger GUI.

SRC =   ArrayModel.java \
	ObjectModel.java \
	ReferenceModel.java \
	NullModel.java \
	LVTreeNode.java   \
	PrimitiveTreeNode.java \
	LocalVariableDisplay.java \
	ReferenceTreeNode.java \
	GUI.java

 

all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/debugger/gui/%.java
	$(CP) $< .
	$(NEWLINE) $@


