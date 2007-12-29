# Build section of distributable containing the Java source for
# wizard classes.

SRC =   ClassRegistry.java \
	DefaultNameFactory.java \
	DelegateFactory.java \
	ImportWizard.java \
	InterfaceFactory.java \
	AbstractClassFactory.java \
	MethodFactory.java \
	MethodOverrideFactory.java \
	NameFactory.java \
	Signature.java \
	SignatureContainer.java \
	SignatureVisitor.java

all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/wizards/%.java
	$(CP) $< .
	$(NEWLINE) $@


