package jde.debugger;

/**
 * Protocol.java
 * <p>
 * Repository of all commands sent from jde to jdebug, and all lisp functions
 * sent from jdebug to jde
 * <p>
 *
 * <b>Nomenclature:</b>
 * 
 * <dt> jdebug: </dt>
 * <dd> The java side of the debugger </dd>
 *
 * <dt> jde: </dt>
 * <dd> The emacs side of the debugger. Also used in general terms to
 *      mean JDE itself. </dd>
 *
 * <dt> JDE: </dt>
 * <dd> The java development environment </dd>
 *
 * <dt> debugger: </dt>
 * <dd> Usually the java side, also used in general
 *      terms to mean the full debugging framework </dd>
 *
 * <dt> debugee: </dt>
 * <dd> The application being debugged. Usually means
 *      the VM of the application, as against the VM
 *      of the debugger. </dd>
 *
 * <dt> application: </dt>
 * <dd> Same as 'debugee' </dd>
 *
 * <dt> VM: </dt>
 * <dd> The java virtual machine. During debugging,
 *      the debugger VM connects to the debuggee VM
 *      through a socket/shared memory (the latter is
 *      possible in case of Win9x/NT)
 * <p>
 *
 *
 * <b> Commands </b>
 * <p>
 * The commands will be ASCII strings delimited by spaces, and
 * will end with a  BR .
 * <pre>
 *      debugee_vm_id command_id command [optional_args]
 * </pre>
 *
 * <dt> debugee_vm_id </dt>
 * <dd> a number that is assigned during handshake
 *      between the emacs and the jde sides,
 *      representing a specific debugee vm.
 *      <p>
 *      if (id == -1), the command is not specific to
 *      any debugee vm.
 *      <p>
 *      'app_id', 'vm_id' 'id' & 'debugee_vm_id' are
 *      synonymous. </dd>
 *
 * <dt> command_id </dt>
 * <dd> an id that helps match a command and its response.
 *      <p>
 *      'cmd_id' 'cid' & 'command_id' are synonymous. </dd>
 *
 * <dt> command </dt>
 * <dd> plain text command </dd>
 *
 * <dt> optional_args </dt>
 * <dd> arguments for the command. can use "" for
 *      arguments with white spaces. </dd>
 * <p>
 *
 *
 * <b> Replies </b>
 * <p>
 * The replies to these commands will be ASCII too, in lisp
 * forms, and will also end with a  BR . They will either by
 * "result" or "error":
 * <pre>
 *      (jde-dbo-command-result command_id [optional_args])
 *      (jde-dbo-command-error command_id reason)
 * </pre>
 *
 * <dt> debugee_vm_id, command_id </dt>
 * <dd> as above </dd>
 *
 * <dt> optional_args </dt>
 * <dd> if required </dd>
 * 
 * <dt> reason </dt>
 * <dd> a string, containing the error message </dd>
 * <p>
 *
 *
 * <b> Event Sets </b>
 * See {@link EventHandler here}
 * <p>
 *
 * <p>
 * Created: Thu Jul  8 13:44:10 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public interface Protocol  {

    /*
     *
     * Some useful constants
     *
     */

    /** line break, platform dependent */
    public static String BR = System.getProperty("line.separator");


    /*
     *
     * Supported Commands from jde to jdebug
     *
     */

    
    /*
     * Specifying the debuggee application
     */

    /** @see jde.debugger.command.LaunchApplication */
    public final static String LAUNCH = "launch";
    
    /** @see jde.debugger.command.AttachSocket */
    public final static String ATTACH_SOCKET = "attach_socket";

    /** @see jde.debugger.command.AttachShmem */
    public final static String ATTACH_SHMEM = "attach_shmem";
    
    /** @see jde.debugger.command.ListenSocket */
    public final static String LISTEN_SOCKET = "listen_socket";

    /** @see jde.debugger.command.ListenShmem */
    public final static String LISTEN_SHMEM = "listen_shmem";

    /*
     * Others
     */
    
    /** @see jde.debugger.command.Quit */
    public final static String QUIT = "quit";
    /** @see jde.debugger.command.Quit */
    public final static String EXIT = "exit";

    /*
     * Following commands are application specific: ie are directed
     * towards an application once it has been launched/attached to/ etc.
     */

    /** @see jde.debugger.command.Run */
    public final static String RUN = "run";

    /** @see jde.debugger.command.Finish */
    public final static String FINISH = "finish";

    /** @see jde.debugger.command.TraceClasses */
    public final static String TRACE_CLASSES = "trace_classes";
    /** @see jde.debugger.command.CancelTraceClasses */
    public final static String CANCEL_TRACE_CLASSES = "cancel_trace_classes";


    /** @see jde.debugger.command.TraceExceptions */
    public final static String TRACE_EXCEPTIONS = "trace_exceptions";
    /** @see jde.debugger.command.Watch */
    public final static String WATCH = "watch";
    /** @see jde.debugger.command.Break */
    public final static String BREAK = "break";
    /** @see jde.debugger.command.Clear */
    public final static String CLEAR = "clear";

    /** @see jde.debugger.command.Step */
    public final static String STEP = "step";

    /** @see jde.debugger.command.Suspend */
    public final static String SUSPEND = "suspend";
    /** @see jde.debugger.command.Resume */
    public final static String RESUME = "resume";
    /** @see jde.debugger.command.Interrupt */
    public final static String INTERRUPT = "interrupt";
    /** @see jde.debugger.command.KillThread */
    public final static String KILL_THREAD = "kill_thread";

    /** @see jde.debugger.command.GetThreads */
    public final static String GET_THREADS = "get_threads";
    /** @see jde.debugger.command.GetThread */
    public final static String GET_THREAD = "get_thread";
    /** @see jde.debugger.command.GetObjectMonitors */
    public final static String GET_OBJECT_MONITORS = "get_object_monitors";
    /** @see jde.debugger.command.TraceThreads */
    public final static String TRACE_THREADS = "trace_threads";
    /** @see jde.debugger.command.CancelTraceThreads */
    public final static String CANCEL_TRACE_THREADS = "cancel_trace_threads";

    /** @see jde.debugger.command.TraceMethods */
    public final static String TRACE_METHODS = "trace_methods";
    /** @see jde.debugger.command.CancelTraceMethods */
    public final static String CANCEL_TRACE_METHODS = "cancel_trace_methods";
    
    /** @see jde.debugger.command.GetObject */
    public final static String GET_OBJECT = "get_object";

  /** @see jde.debugger.command.GetArray */
  public final static String GET_ARRAY = "get_array";

    /** @see jde.debugger.command.GetString */
    public final static String GET_STRING = "get_string";

    /** @see jde.debugger.command.GetLocals */
    public final static String GET_LOCALS = "get_locals";

    /** @see jde.debugger.command.GetLoadedClasses */
    public final static String GET_LOADED_CLASSES = "get_loaded_classes";
    /** @see jde.debugger.command.GetPathInfo */
    public final static String GET_PATH_INFORMATION = "get_path_information";

    /** @see jde.debugger.command.EvaluateExpression */
    public final static String EVALUATE = "evaluate";


    /*
     *
     * Commands end
     *
     */




    

    // lisp functions sent from jdebug to jde

    /** arbitrary lisp functions passed to jde start with this*/
    public final static String JDE_BUG =
	"jde-dbo-";

    /** the very first function, indicating that jdebug is up and running */
    public final static String JDE_INIT_DEBUG_SESSION =
	JDE_BUG+"init-debug-session";

    /** The command executed properly, returns the result */
    public final static String COMMAND_RESULT =
	"command-result";
    /** There was an error executing the command, returns the error */
    public final static String COMMAND_ERROR =
	"command-error";

    /** A message to be displayed on the JDE UI */
    public final static String MESSAGE =
	"message";
    /** A warning */
    public final static String WARNING =
	"warning";
    /** an error */
    public final static String ERROR =
	"error";

    /** a debug message */
    public final static String DEBUG =
	"debug";

    /** Event sets caused by the jpda. */
    public final static String EVENTSET =
	"event-set";

    /** used to construct messages about invalid breakpoints etc. */
    public final static String INVALID =
	"invalid-";

    /**
     * tell jdebug to inform jdebug about references to objects being
     * currently used. might dissapear soon...
     */
    public final static String REPORT_IDS_IN_USE =
	"report-ids-in-use";

    /**
     * Notifies that a connection to a vm was successfully made
     */
    public final static String CONNECTED_TO_VM =
	"connected-to-vm";

    /**
     * Notifies that a spec was resolved properly
     */
    public final static String SPEC_RESOLVED =
	"spec-resolved";


    /*
     *
     * Events that are raised by the JDI implementation
     *
     */
     

    /** @see EventHandler#breakpointEvent(BreakpointEvent) */
    public final static String EVENT_BREAKPOINT_HIT =
	JDE_BUG+"breakpoint-hit-event";
    /** @see EventHandler#stepEvent(StepEvent) */
    public final static String EVENT_STEP_COMPLETED =
	JDE_BUG+"step-event";

    /** @see EventHandler#watchpointEvent(WatchpointEvent) */
    public final static String EVENT_WATCHPOINT_HIT =
	JDE_BUG+"watchpoint-hit-event";

    /** @see EventHandler#classPrepareEvent(ClassPrepareEvent) */
    public final static String EVENT_CLASS_PREPARE =
	JDE_BUG+"class-prepare-event";
    /** @see EventHandler#classUnloadEvent(ClassUnloadEvent) */
    public final static String EVENT_CLASS_UNLOAD =
	JDE_BUG+"class-unload-event";

    /** @see EventHandler#exceptionEvent(ExceptionEvent) */
    public final static String EVENT_EXCEPTION =
	JDE_BUG+"exception-event";

    /** @see EventHandler#threadStartEvent(ThreadStartEvent) */
    public final static String EVENT_THREAD_START =
	JDE_BUG+"thread-start-event";
    /** @see EventHandler#threadDeathEvent(ThreadDeathEvent) */
    public final static String EVENT_THREAD_DEATH =
	JDE_BUG+"thread-death-event";

    /** @see EventHandler#methodEntryEvent(MethodEntryEvent) */
    public final static String EVENT_METHOD_ENTRY =
	JDE_BUG+"method-entry-event";
    /** @see EventHandler#methodExitEvent(MethodExitEvent) */
    public final static String EVENT_METHOD_EXIT =
	JDE_BUG+"method-exit-event";

    /** @see EventHandler#vmStartEvent(Event) */
    public final static String EVENT_VM_START =
	JDE_BUG+"vm-start-event";
    /** @see EventHandler#vmDeathEvent(Event) */
    public final static String EVENT_VM_DEATH =
	JDE_BUG+"vm-death-event";
    /** @see EventHandler#vmDisconnectEvent(Event) */
    public final static String EVENT_VM_DISCONNECT =
	JDE_BUG+"vm-disconnected-event";

    /** @see EventHandler#otherEvent(Event) */
    public final static String EVENT_OTHER =
	JDE_BUG+"event-other";

    /*
     *
     * Events end
     *
     */

    // Constants for signalling through the JDE class
    public final static boolean QUOTE   = true;
    public final static boolean NOQUOTE = false;
    public final static boolean CMD_OK  = true;
    public final static boolean CMD_NOK = false;


    /** 
     * No trace output
     */
    public final static int NONE      = 0;
    /**
     * Trace application I/O - CURRENTLY NOT USED
     */
    public final static int APP_IO    = 1;
    /**
     * Trace pipe between Emacs and Java - CURRENTLY NOT USED
     */
    public final static int JDE_PIPE  = 2;
    /**
     * Trace anything related to events.
     */
    public final static int EVENTS    = 4;
    /**
     * Trace exceptions
     */
    public final static int EXCEPTION = 8;
    /**
     * Trace the functioning of the framework: debugging process
     * administration, etc.
     */
    public final static int FRAMEWORK = 16;
    /**
     * Trace command execution
     */
    public final static int COMMANDS  = 32;
    /**
     * Trace GUI
     */
    public final static int GUI       = 64;

} // Protocol

/*
 * $Log: Protocol.java,v $
 * Revision 1.3  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 */
