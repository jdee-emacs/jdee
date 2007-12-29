# Build section of distributable containing the Java source for
# debugger commands.

SRC =   AttachShmem.java AttachSocket.java Break.java CancelTraceClasses.java CancelTraceMethods.java CancelTraceThreads.java Clear.java CommandRegistry.java DebugCommand.java DebugCommandFactory.java DebugProcessCommand.java DebugSessionCommand.java DebugThread.java EvaluateExpression.java Finish.java GetArray.java GetLoadedClasses.java GetLocals.java GetObject.java GetObjectMonitors.java GetPathInfo.java GetString.java GetThis.java GetThread.java GetThreads.java Interrupt.java KillThread.java LaunchApplication.java ListenShmem.java ListenSocket.java Quit.java Resume.java Run.java Step.java Suspend.java TraceClasses.java TraceExceptions.java TraceMethods.java TraceThreads.java Watch.java

all: $(SRC)

%.java : $(JDEDIR)/java/src/jde/debugger/command/%.java
	$(CP) $< .
	$(NEWLINE) $@
