package jde.debugger;

import com.sun.jdi.ReferenceType;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.event.EventQueue;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequest;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import jde.debugger.ObjectStore;
import jde.debugger.spec.BreakpointSpec;
import jde.debugger.spec.EventRequestSpec;
import jde.debugger.spec.EventRequestSpecList;
import jde.debugger.spec.ExceptionSpec;
import jde.debugger.spec.WatchpointSpec;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.ThreadGroupReference;


/**
 * Class of debuggee processes.
 *
 * @author Amit Kumar
 * @since 0.1
 * @author Paul Kinnucan
 * @since 1.5
 */
public class DebuggeeProcess implements Protocol {

     
  /********************************************************************
   * CONSTRUCTORS                                                     *
   ********************************************************************/

  /**
   * Creates an instance of a process object.
   *
   * @param jdebug {@link Jdebug} class
   * @param procID Identifier used to specify this app int commands.
   * @param vm Virtual machine in which this process is running.
   */
  public DebuggeeProcess(Integer procID, VirtualMachine vm) {
    
    this.procID = procID;

    this.vm = vm;

    store = new ObjectStore(this);

    eventHandler = new EventHandler(this);

    eventRequestSpecs = new EventRequestSpecList(this);

    // we need to raise all class prepare events to 
    // make sure we resolve the corresponding specs.
    ClassPrepareRequest cprequest =
      vm.eventRequestManager().createClassPrepareRequest();

    // this (hack?) is used to identify if the user itself specified
    // a class prepare request, or the event was raised because of
    // this request.
    cprequest.putProperty("default", "default");
    cprequest.setSuspendPolicy(EventRequest.SUSPEND_ALL);
    cprequest.enable();

    debuggeeSIO = new DebuggeeSIO(this);

  }

  /********************************************************************
   * METHODS                                                          *
   ********************************************************************/

  /*
   * Gets the virtual machine that is running this application.
   * @return Process running this vm.
   */
  public final VirtualMachine getVM() { return vm; }

  /*
   * Gets the ID used by commands to specify this application.
   *
   * @return application ID
   */
  public final Integer getId() { return procID; }

  /*
   * Generates an ID used by commands to reference objects.
   * 
   * @return new object ID
   */
  public final Long generateObjectID() {
    // synchronize on any static object
    synchronized (Jdebug.debuggerID) {
      return new Long(objIdCounter++);
    }
  }

  public final ObjectStore getStore() { return store; }

  public final EventQueue getEventQueue() { return vm.eventQueue(); }


  public final void shutdown() {
    // we need to be a little considerate to the output. given
    // there are all kinds of threads running, we make sure we
    // wait for all the output to complete.

    shuttingDown = true;
	
    // isolate the process first
    Process process = null;
    if (vm != null) 
      process = vm.process();
    try {
      if (vm != null) {
	vm.dispose();
	vm = null;
	eventHandler.shutdown();
      }
      debuggeeSIO.shutdown();
    } catch (Exception ex) {
      // do nothing
    } finally {
      if (process != null) {
	process.destroy();
	// XXX sun's jdb implementation works a lot to make sure
	// the stderr and stdout are dumped before killing
	// things. i'm not sure how important it is, or even how
	// well it works (given i can't test it)
	// sooo, if the reader finds bugs with the output handling
	// on finish, lemme know.
      }
      procRegistry.removeProcess(procID);
    }
  }

    
    
  /*
   * FUNCTIONS TO MANAGE Standard IO STREAMS
   */   

 

    
  /*
   *
   * USEFUL FUNCTIONS
   *
   */


    
  /**
   * Return a list of ReferenceType objects for all
   * currently loaded classes and interfaces whose name
   * matches the given pattern.  The pattern syntax is
   * open to some future revision, but currently consists
   * of a fully-qualified class name in which the first
   * component may optionally be a "*" character, designating
   * an arbitrary prefix.
   */
  public List findClassesMatchingPattern(String pattern)
    throws JDEException {
    if (vm == null) return null;
    List result = new ArrayList();  //### Is default size OK?
    if (pattern.startsWith("*.")) {
      // Wildcard matches any leading package name.
      pattern = pattern.substring(1);
      List classes = vm.allClasses();
      Iterator iter = classes.iterator();
      while (iter.hasNext()) {
	ReferenceType type = ((ReferenceType)iter.next());
	if (type.name().endsWith(pattern)) {
	  result.add(type);
	}
      }
      return result;
    } else {
      // It's a class name.
      return vm.classesByName(pattern);
    }
  }

    /**
     * Returns the thread corresponding to this name
     */
    public ThreadReference getThread(String name) {
	
	List list = vm.allThreads();
	Iterator it = list.iterator();

	ThreadReference thread;
	while (it.hasNext()) {
	    thread = (ThreadReference)it.next();
	    if (thread.name().equals(name)) return thread;
	}

	return null;
    }


  /**
   * Returns a representation of all the threads and threadgroups
   * in the VM. For example:
   * <pre>
   *              ThreadGroup-1
   *                  +- ThreadGroup-2
   *                  |        +- ThreadGroup-3
   *                  |        |        \- Thread-1
   *                  |        +- ThreadGroup-4
   *                  |        |        +- Thread-2
   *                  |        |        \- Thread-3
   *                  |        \- Thread-4
   *                  \- Thread-5
   *              ThreadGroup-5
   *                  +- Thread-6
   *
   *
   *          (list
   *            (list "ThreadGroup" <tgID> "ThreadGroup-1"
   *              (list 
   *                (list "Thread" <tID> "Thread-5" ...))
   *              (list 
   *                (list "ThreadGroup" <tgID> "ThreadGroup-2"
   *                  (list 
   *                    (list "Thread" <tID> "Thread-4"))
   *                  (list 
   *                    (list "ThreadGroup" <tgID> "ThreadGroup-3"
   *                      (list)
   *                      (list 
   *                        (list "Thread" <tID> "Thread-1" ...)))
   *                    (list "ThreadGroup" <tgID> "ThreadGroup-4"
   *                      (list)
   *                        (list
   *                          (list "Thread" <tID> "Thread-2" ...)
   *                          (list "Thread" <tID> "Thread-3" ...)))))))
   *          (list "ThreadGroup" <tgID> "ThreadGroup-5"
   *            (list)
   *              (list
   *                (list "Thread" <tID> "Thread-6" ...))))
   * </pre>
   * <b>Syntax:</b>
   * <pre>
   * (list [{@link Rep#getThreadGroupRep top-level thread group}]*)
   * </pre>
   *
   * @param vm The virtual machine itself
   * @param store The object store where you should keep references to
   * the thread ids. For details, see {@link ObjectStore}
   */
  public LispForm getAllThreadsInformation() {
      List l = vm.topLevelThreadGroups();
	
      String info = "(list ";
      Iterator it = l.iterator();

      while (it.hasNext()) {
	info +=  BR +Rep.getThreadGroupRep((ThreadGroupReference)it.next(), store);
      }

      info += ")";
      return new LispForm(info);
    }


  /* spec related functions */

    
  /**
   * This method is executed whenever a new reference type is prepared.
   * If any outstanding specs match, they get resolved in the process
   *
   * @see EventRequestSpecList#resolve(ReferenceType)
   */
  public void resolve(ReferenceType ref) {
    eventRequestSpecs.resolve(ref);
  }

  /**
   * Inform jde on a successful spec resolution
   */
  public void informJDEInstallSuccessful(EventRequestSpec spec) {
    jde.signal(procID, SPEC_RESOLVED, spec.getID());
  }
    
  /**
   * Removes a Spec from the specList, and informs jde.
   * If there is an error while resolving a spec, indicating that it
   * cannot be resolved (ie even at a later time when more classes are
   * prepared), this function is called to remove it from the list, and
   * inform the jde about this error
   */
  public void removeSpecAndInformJDE(EventRequestSpec spec, String problem) {
    if (spec instanceof BreakpointSpec) {
      jde.signal(procID, INVALID+BREAK, new LispForm(spec.getID()
					 +" \""+problem+"\""));
    } else if (spec instanceof WatchpointSpec) {
      jde.signal(procID, INVALID+WATCH, new LispForm(spec.getID()
					 +" \""+problem+"\""));
    } else if (spec instanceof ExceptionSpec) {
      jde.signal(procID, INVALID+TRACE_EXCEPTIONS,
	     new LispForm(spec.getID()+" \""+problem+"\""));
    }
    eventRequestSpecs.delete(spec);
  }

  public EventRequestSpecList getEventRequestSpecs() {
    return eventRequestSpecs;
  }

  public DebuggeeSIO getSIO() {
    return debuggeeSIO;
  }


  /********************************************************************
   * FIELDS                                                           *
   ********************************************************************/

  /** The ID that uniquely identifies this process in jdebug. */
  final Integer procID;	

  /**
   * The {@link EventHandler} manages the events received from the
   * debugee vm
   */
  EventHandler eventHandler;

  /** The virtual machine that is running this application. */
  VirtualMachine vm;	        

  /**
   * {@link jde.debugger.spec.EventRequestSpecList} is responsible for
   * keeping track of the events that the user is interested in. To do
   * this, it maintains a list of "eventRequestSpec"s.
   *
   * @see jde.debugger.spec.EventRequestSpecList
   * @see jde.debugger.spec.EventRequestSpec
   */
  EventRequestSpecList eventRequestSpecs;


  /**
   * A store of all the objects jde knows about.
   * @see ObjectStore
   */
  ObjectStore store;

  /**
   * Some classes require a unique ID with which to refer to objects
   * they are tracking: for instance eventRequestSpecs, which need a spec
   * ID with which to identify the specs, and identifiableSpecRequests
   * (in ProcessCommands)
   * <p>
   * This variable keeps a monotonically increasing count, and can be
   * used to generate a new id, using {@link #generateObjectID}
   */
  private long objIdCounter = 0;

  /**
   * keeps track of the state of the application: exceptions/error messages
   * will not be raised if we're shutting down.
   */
  private boolean shuttingDown = false;
  public boolean isShuttingDown() { return shuttingDown; }

  DebuggeeSIO debuggeeSIO;

  ProcessRegistry procRegistry = ProcessRegistry.getRegistry();
  JDE jde = JDE.getJDE();
     
} // Process


/*
 * $Log: DebuggeeProcess.java,v $
 * Revision 1.1  2001/03/24 05:33:17  paulk
 * Initial version.
 *
 *
 */


// End of DebuggeeProcess.java
