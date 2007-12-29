# Build section of distributable containing the Java source for
# debugger commands.

SRC =  	ASCII_UCodeESC_CharStream.java \
	Expr.jj \
	ExpressionParser.java \
	ExpressionParserConstants.java \
	ExpressionParserTokenManager.java \
	LValue.java \
	ParseException.java \
	Token.java \
	TokenMgrError.java 


all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/debugger/expr/%.java
	$(CP) $< .
	$(NEWLINE) $@

%.jj : $(JDEDIR)/java/src/jde/debugger/expr/%.jj
	$(CP) $< .
	$(NEWLINE) $@

