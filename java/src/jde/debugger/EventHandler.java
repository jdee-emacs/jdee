package jde.debugger;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.sun.jdi.BooleanValue;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.VMDisconnectedException;
import com.sun.jdi.Value;
import com.sun.jdi.event.BreakpointEvent;
import com.sun.jdi.event.ClassPrepareEvent;
import com.sun.jdi.event.ClassUnloadEvent;
import com.sun.jdi.event.Event;
import com.sun.jdi.event.EventIterator;
import com.sun.jdi.event.EventQueue;
import com.sun.jdi.event.EventSet;
import com.sun.jdi.event.ExceptionEvent;
import com.sun.jdi.event.LocatableEvent;
import com.sun.jdi.event.MethodEntryEvent;
import com.sun.jdi.event.MethodExitEvent;
import com.sun.jdi.event.StepEvent;
import com.sun.jdi.event.ThreadDeathEvent;
import com.sun.jdi.event.ThreadStartEvent;
import com.sun.jdi.event.VMDeathEvent;
import com.sun.jdi.event.VMDisconnectEvent;
import com.sun.jdi.event.VMStartEvent;
import com.sun.jdi.event.WatchpointEvent;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.ExceptionRequest;
import jde.debugger.Debugger;
import jde.debugger.EventSetEvent;
import jde.debugger.spec.EventRequestSpec;
import jde.debugger.spec.WatchpointSpec;





/**
 * Each Debugger has an event handler thread on the jdebug side
 * associated with it that receives all the events from the debugee
 * vm. In turn, the event handler thread passes the events on to the
 * jde, indicating if the vm/current thread was suspended.
 * <p>
 * Created: Tue Jul  6 14:08:44 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @see Debugger
 * @version $Revision: 1.7 $
 */

public class EventHandler extends Thread implements Protocol {
  /** milliseconds to wait for a new event from the VM before timing out */
  static final int EVTQ_TIMEOUT = 2000;

  /** Are we connected to the VM? */
  private boolean m_connected = true;

  /** The debugger for which we're the event handler */
  private final Debugger m_debugger;

  /** Keeping track of if the thread is over yet or not */
  boolean m_stopRequested = false;

  /**
   * Used by the event handlers to indicate if the app should be
   * resumed after the event occured.
   */
  boolean resumeApp;

    /** List of EventSetListeners to notify */
  private final Collection m_eventSetListeners;

  public EventHandler(Debugger debugger) {
    super("Event Handler for process " + debugger.getProcID());

    m_debugger = debugger;
    m_eventSetListeners = new LinkedList();
    addEventSetListener(new EvtListener());
  }

  /**
   * Indicates that the thread executing the event handling should
   * stop as the first opportunity.
   */
  public void shutdown() {
    m_stopRequested = true;
    // XXX - if the event queue timeout is increased beyond the
    // 500 msecs that it is set to at the moment, it's a good idea
    // to interrupt the thread in some way. Otherwise, the
    // shutdown could be a bit sluggish
  }

  /**
   * The thread reads an eventset at a time from the application
   * queue, and processes it.<p>
   *
   * First, it notifies all <code>EventSetListener</code> that an
   * event set has arrived.  The listeners can set a flag in the event
   * to indicate if the JVM should be resumed.  The old method of
   * setting the resumeApp variable is also supported for the old
   * code.<p>
   *
   * Next, all listeners are again notified whether we are suspending
   * or resuming the JVM.  If we are resuming the JVM, the listener
   * registered in this class' constructor does the resume, so all
   * other listeners will be notified after the JVM has been resumed.
   *
   *      This two-tier suspend-policy handling might be simplified to
   *      some extent once all commands support setting of a suspend
   *      policy, with that and that alone being used to decide on what
   *      to do with the vm.
   *      <p>
   *      A problem with that approach is that since we do a second pass
   *      on the events when we get the eventset to determine if the
   *      events should be sent to the user (eg. a breakpoint-hit is still
   *      not interesting if it doesn't occur on the "right" thread), the
   *      latter approach might not work well at all.
   * </ul>
   */
  public void run() {
    EventQueue queue = m_debugger.getVM().eventQueue();

    while (!m_stopRequested && m_connected) {
      try {
        EventSet eventSet = queue.remove(EVTQ_TIMEOUT);

        // eventSet is null if the remove() operation timed out.
        if (eventSet == null) {
          JDE.debug(FRAMEWORK, "EventHandler " + m_debugger.getProcID() + " timed out");
          continue;
        }

        // OK, we have a real event set, so process it.
        resumeApp = true;

	// Notify all listeners that we got an event set.
        ThreadReference eventThread = getCurrentThread(eventSet);
	EventSetEvent event = new EventSetEvent(eventSet,
						eventThread,
						!resumeApp);
	JDE.debug(FRAMEWORK, "EventHandler: sending event " + event);

	for (Iterator iter = m_eventSetListeners.iterator();
	     iter.hasNext();
	     /* */) {
	    EventSetListener listener = (EventSetListener) iter.next();
	    listener.eventSetReceived(event);
	} // for each EventSetListener

        if (resumeApp) {
	  // Notify all listeners that we'll be resuming the VM
	  for (Iterator iter = m_eventSetListeners.iterator();
	       iter.hasNext();
	       /* */) {
	    EventSetListener listener = (EventSetListener) iter.next();
	    JDE.debug(FRAMEWORK, "EventHandler: sending resume event to " + listener);
	    try {
	      listener.debuggerResumed(event);
	    } catch (RuntimeException exc) {
	      JDE.signalException(exc);
          }
	  } // for each EventSetListener
	JDE.debug(FRAMEWORK, "EventHandler: VM resumed");
        } else {

	JDE.debug(FRAMEWORK, "EventHandler: VM not resumed");
	    // Notify all listeners that we'll be suspending the VM
	  // and not restarting it
	    for (Iterator iter = m_eventSetListeners.iterator();
		 iter.hasNext();
		 /* */) {
		EventSetListener listener = (EventSetListener) iter.next();
		JDE.debug(FRAMEWORK,
			  "EventHandler: sending suspended event to " +
			  listener);
		listener.debuggerSuspended(event);
	    } // for each EventSetListener
        }

      } catch (InterruptedException ex) {
        // should not happen...
        JDE.debug(EXCEPTION, ex.toString());
      } catch (VMDisconnectedException ex) {
        // this should also not happen, unless there's a bug in the
        // JDEbug Java implementation
        JDE.debug(EXCEPTION, ex.toString());
        handleDisconnectedException(queue);
      } catch (RuntimeException ex) {
	JDE.debug(EXCEPTION, ex.toString());
	ex.printStackTrace();
      }
    }
    JDE.debug(FRAMEWORK, "event handler: " + m_debugger.getProcID() + " terminated");

  }



  /**
   * Handles the events that happened
   *
   * @param event One of the events in the event set.
   * @return A String that should be sent to jde. This
   * should be of the form "(list jde-dbo-EVENT-event [args])". Each
   * of the functions called herein returns that type of string.
   */
  private String handleEvent(Event event) {
    if (event instanceof BreakpointEvent) {
      return breakpointEvent((BreakpointEvent)event);
    } else if (event instanceof StepEvent) {
      return stepEvent((StepEvent)event);
    } else if (event instanceof WatchpointEvent) {
      return watchpointEvent((WatchpointEvent)event);
    } else if (event instanceof ExceptionEvent) {
      return exceptionEvent((ExceptionEvent)event);
    }
    else if (event instanceof ThreadStartEvent) {
      return threadStartEvent((ThreadStartEvent)event);
    } else if (event instanceof ThreadDeathEvent) {
      return threadDeathEvent((ThreadDeathEvent)event);
    }
    else if (event instanceof MethodEntryEvent) {
      return methodEntryEvent((MethodEntryEvent)event);
    } else if (event instanceof MethodExitEvent) {
      return methodExitEvent((MethodExitEvent)event);
    }
    else if (event instanceof ClassPrepareEvent) {
      return classPrepareEvent((ClassPrepareEvent)event);
    }
    else if (event instanceof VMStartEvent) {
      return vmStartEvent((VMStartEvent)event);
    } else if (event instanceof VMDeathEvent) {
      return vmDeathEvent((VMDeathEvent)event);
    } else if (event instanceof VMDisconnectEvent) {
      return vmDisconnectEvent((VMDisconnectEvent)event);
    } else {
      return otherEvent(event);
    }
  }

  /**
   * Duh... we don't recognize this event... or, we choose not to do
   * anything about it
   */
  private String otherEvent(Event event) {
    resumeApp &= true;
    return "(list '"+EVENT_OTHER+")";
  }

  /**
   * What we should do if we get disconnected while we're doing something
   * else.
   */
  private void handleDisconnectedException(EventQueue queue) {
    // disconnected while handling some other event. flush queue
    // and deal with disconnectEvent and deathEvents

    int numberOfTimeoutsBeforeQuit = 5;
    int currentNumberOfTimeouts    = 0;

    JDE.debug(FRAMEWORK, "handling disconnected exception");

    while (m_connected) {
      try {
        JDE.debug(FRAMEWORK, "handling disconnected exception loop");
        EventSet eventSet = queue.remove(EVTQ_TIMEOUT);

        if (eventSet == null) {
          currentNumberOfTimeouts++;
          // this could be problematic if there's no vmDisconnectEvent in the
          // queue.. I guess it shouldn't happen, but.. / Petter
          if (currentNumberOfTimeouts >= numberOfTimeoutsBeforeQuit) {
            JDE.signal(m_debugger.getProcID(), ERROR,
                       "no vmDisconnectEvent found in handleDisconnectedException after " +
                       currentNumberOfTimeouts +
                       " timeouts, terminating anyway!", QUOTE);
            m_connected = false;
          }
          // we did not get any event set, so try for another one (or quit, if we've tried
          // too many times)
          continue;
        }

        JDE.debug(FRAMEWORK, "disc. exc: got an event set");

        EventIterator iter = eventSet.eventIterator();
        while (iter.hasNext()) {
          Event evt = (Event)iter.next();
          if (evt instanceof VMDeathEvent) {
            vmDeathEvent(evt);
          } else if (evt instanceof VMDisconnectEvent) {
            vmDisconnectEvent(evt);
          }
        }
      } catch (InterruptedException ex) {
        JDE.debug(EXCEPTION, ex.toString());
        // ignore
      }
    }
  }

  /**
   * Get the current thread of this event set. It's not necessary for
   * an event set to have a thread associated with it: in those cases
   * just return null.
   *
   * @param eventSet The event set that occurred
   * @return <code>null</code> if there is no thread associated, otherwise
   * the ThreadReference of the Thread
   */
  private ThreadReference getCurrentThread(EventSet eventSet) {
    ThreadReference thread;
    if (eventSet.size() > 0) {
      /*
       * If any event in the set has a thread associated with it,
       * they all will, so just grab the first one.
       */
      Event event = (Event)eventSet.iterator().next(); // Is there a better way?
      thread = getEventThread(event);
    } else {
      thread = null;
    }
    return thread;
  }


  /**
   * Black magic to divine the ThreadReference of the
   * event. Question: can we use "Black magic" and "divine" in the same
   * sentence?
   *
   * @param event An event from the event set: any event will do
   * @return The ThreadReference of the thread
   */
  private ThreadReference getEventThread(Event event) {
    if (event instanceof ClassPrepareEvent) {
      return ((ClassPrepareEvent)event).thread();
    } else if (event instanceof LocatableEvent) {
      return ((LocatableEvent)event).thread();
    } else if (event instanceof ThreadStartEvent) {
      return ((ThreadStartEvent)event).thread();
    } else if (event instanceof ThreadDeathEvent) {
      return ((ThreadDeathEvent)event).thread();
    } else if (event instanceof VMStartEvent) {
      return ((VMStartEvent)event).thread();
    } else {
      return null;
    }
  }


  /**
   * For events with corresponding specs (eg. watchpoints), checks to see
   * if the event thread matches the thread constraint in the spec.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * nil                              # If no such constraint
   *                                  #   was found in the spec
   * "Error mesg"                     # Constraint found, but error
   *                                  #   during evaluation
   * (list "on_thread_id" threadID)   # Constraint found, satisfied
   * (list "on_thread_name" "name")
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The above are strings: so the '"'s above are actually quoted
   *      in the string returned.
   * <li> Note that if the Constraint is <b>found but does not satisfy</b>
   *      'null' is returned: this is tested in the handler routines.
   * </ul>
   */
  private String threadMatch(Event event) {
    Object thread =
      event.request().getProperty(EventRequestSpec.threadKey);
    if (thread == null) {
      return "nil";
    } else if (thread instanceof Long) {
      ThreadReference t = getEventThread(event);
      if (t.uniqueID() == ((Long)thread).longValue()) {
        return "(list \"on_thread_id\" "+t.uniqueID()+")";
      } else {
        return null;
      }
    } else if (thread instanceof String) {
      ThreadReference tRef =
        m_debugger.getThreadReference(thread.toString());
      ThreadReference t = getEventThread(event);

      if (t.equals(tRef)) {
        return "(list \"on_thread_name\""
          +" \""+thread.toString()+"\")";
      } else {
        return null;
      }
    } else {
      return "\"Error matching thread\"";
    }
  }

  /**
   * For events with corresponding specs (eg. watchpoints), evaluates the
   * expression stored in the spec, if any, to check if the event is
   * interesting to the user or not. The constraint is thus the expression.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * nil                              # If no such constraint
   *                                  #   was found in the spec
   * "Error mesg"                     # Constraint found, but error
   *                                  #   during evaluation
   * (list "expression")              # Constraint found, satisfied
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The above are strings: so the '"'s above are actually quoted
   *      in the string returned.
   * <li> Note that if the Constraint is <b>found but does not satisfy</b>
   *      'null' is returned: this is tested in the handler routines.
   * </ul>
   */
  private String expressionSuccess(Event event) {
    Object exprObject =
      event.request().getProperty(EventRequestSpec.expressionKey);
    if (exprObject != null) {
      String expr = exprObject.toString();
      try {
        StackFrame frame = getEventThread(event).frame(0);
        Value val = Etc.evaluate(expr, frame);
        if (!val.type().name().equals("boolean")) {
          return "\"Expression evaluates to non-boolean\"";
        } else {
          BooleanValue boolValue = (BooleanValue)val;
          if (boolValue.value() == true) {
            return "(list \""+expr+"\")";
          } else {
            return null;
          }
        }
      } catch (Exception ex) {
        JDE.debug(EXCEPTION, ex.toString());
        return "\"Expression didn't evaluate correctly\"";
      }
    }
    return "nil";
  }


  /**
   * For watchpoints, when the object-id specified during the command
   * should match the object id of the event.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * nil                              # if no such constraint was in spec
   * "Error mesg"                     # Constraint found, error in eval.
   * (list <u>object-id</u>)                 # Constraint found, satisfied.
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The above are strings: so the '"'s above are actually quoted
   *      in the string returned.
   * <li> Note that if the Constraint is <b>found but does not satisfy</b>
   *      'null' is returned: this is tested in the handler routines.
   * <li> <u>object-id</u> is the object id that was specified in the
   *      command
   * </ul>
   */
  private String objectIDMatches(WatchpointEvent event) {
    String objectIDString;
    Object idObject =
      event.request().getProperty(WatchpointSpec.objectIDKey);
    if (idObject == null) {
      return "nil";
    } else if (idObject instanceof Long) {
      Long id = (Long)idObject;
      if (event.object() == null) {
        return "(list "+id+")";
      } else if (event.object().uniqueID() == id.longValue()) {
        return "(list "+id+")";
      } else {
        return null;
      }
    } else {
      return "\"Object ID was not a Long\"";
    }
  }


  /**
   * A Breakpoint was hit. We check (based on information in the event
   * request; put in when the spec was resolved; that's sent along with
   * the event) if the user wants to know about this event. If so, we
   * return a lispform, else return null.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_BREAKPOINT_HIT breakpoint-hit-function} specID {@link Rep#getLocationRep(Location) location} {@link #threadMatch thread-string} {@link #expressionSuccess expression-string})
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> If one of the constraints returns null instead of a string, it
   *      means the constraint isn't satisfied. We return null, indicating
   *      that the user is not interested in the event of this event :-)
   * </ul>
   */
  private String breakpointEvent(BreakpointEvent event) {
    JDE.debug(EVENTS, "got a BreakpointEvent");

    Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();

    // Keep track of this thread, since the debugger will ask for more info about it.
    ThreadReference thread = event.thread();

    if (thread != null) {
      m_debugger.getStore().put(thread);
    }

    // check if the current thread is ok.
    String threadString = threadMatch(event);
    if (threadString == null) {
      resumeApp &= true;
      return null;
    }

    // check if the expression matches
    String exprString = expressionSuccess(event);
    if (exprString == null) {
      resumeApp &= true;
      return null;
    }

    resumeApp &= false;
    return "(list '"+EVENT_BREAKPOINT_HIT
      +" "+specID
      + BR +Rep.getLocationRep(event.location())
      +" "+threadString
      +" "+exprString+")";

  }


  /**
   * A step event occured.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_STEP_COMPLETED step-function} {@link Rep#getLocationRep(Location) location-string})
   * </pre>
   */
  private String stepEvent(StepEvent event) {
    JDE.debug(EVENTS, "got a STEP event");

    resumeApp &= false;
    return "(list '"
      +EVENT_STEP_COMPLETED
      +" "+Rep.getLocationRep(event.location())
      +")";
  }

  /**
   * A Watchpoint occured. We check (based on information in the event
   * request; put in when the spec was resolved; that's sent along with
   * the event) if the user wants to know about this event. If so, we
   * return a lispform, else return null.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_WATCHPOINT_HIT watchpoint-hit-function} specID {@link Rep#getObjectRep(ObjectReference) object-on-which-hit} {@link Rep#getFieldValueRep(Field, Value) field-and-value} {@link Rep#getLocationRep(Location) location}
   *       {@link #objectIDMatches object-id-string} {@link #threadMatch thread-string} {@link #expressionSuccess expression-string})
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> If one of the constraints returns null instead of a string, it
   *      means the constraint isn't satisfied. We return null, indicating
   *      that the user is not interested in the event of this event :-)
   * </ul>
   */
  private String watchpointEvent(WatchpointEvent event) {
    JDE.debug(EVENTS, "got a WATCHPOINT event");

    Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();

    //	System.out.println("Suspend policy of WatchpointEvent: "+((event.request().suspendPolicy() == EventRequest.SUSPEND_NONE)?"none":"not none"));

    // check if the current thread is ok.
    String threadString = threadMatch(event);
    if (threadString == null) {
      resumeApp &= true;
      return null;
    }

    // check if the expression matches
    String exprString = expressionSuccess(event);
    if (exprString == null) {
      resumeApp &= true;
      return null;
    }

    // check if the object id (if specified) matches the event object
    String objectIDString = objectIDMatches(event);
    if (objectIDString == null) {
      resumeApp &= true;
      return null;
    }

    String fieldValueString = Rep.getFieldValueRep(event.field(), event.valueCurrent());
    String objectString     = Rep.getObjectRep(event.object());

    resumeApp &= false;
    return "(list '"+EVENT_WATCHPOINT_HIT
      +" "+specID
      + BR +objectString
      + BR +fieldValueString
      + BR +Rep.getLocationRep(event.location())
      + BR +objectIDString
      +" "+threadString
      +" "+exprString+")";

  }


  /**
   * An exception event occured. Depending on the command, it could occur
   * for both caught and uncaught exceptions.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_EXCEPTION exception-function} specID {@link Rep#getObjectRep(ObjectReference) exception} {@link #threadMatch thread-string})
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> If one of the constraints returns null instead of a string, it
   *      means the constraint isn't satisfied. We return null, indicating
   *      that the user is not interested in the event of this event :-)
   * </ul>
   */
  private String exceptionEvent(ExceptionEvent event) {
    JDE.debug(EVENTS, "got an EXCEPTION event");

    if (event.request() == null) {
      resumeApp &= true;
      return null;
    }

    Long specID = ((EventRequestSpec)event.request().getProperty(EventRequestSpec.specPropertyKey)).getID();

    ExceptionRequest request =
      (ExceptionRequest)event.request();

    // check if the current thread is ok.
    String threadString = threadMatch(event);
    if (threadString == null) {
      resumeApp &= true;
      return null;
    }

    resumeApp &= false;
    return "(list '"
      +EVENT_EXCEPTION
      +" "+specID
      + BR +Rep.getObjectRep(event.exception())
      + BR +threadString+")";
  }


  /**
   * A method was entered.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_METHOD_ENTRY method-entry-function} {@link Rep#getMethodRep method})
   * </pre>
   */
  private String methodEntryEvent(MethodEntryEvent event) {
    JDE.debug(EVENTS, "got a METHOD ENTRY event");

    resumeApp &= false;
    return "(list '"
      + EVENT_METHOD_ENTRY
      + BR +Rep.getMethodRep(event.method())
      +")";
  }

  /**
   * A method was exit.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_METHOD_EXIT method-exit-function} {@link Rep#getMethodRep method})
   * </pre>
   */
  private String methodExitEvent(MethodExitEvent event) {
    JDE.debug(EVENTS, "got a METHOD EXIT event");

    resumeApp &= false;
    return "(list '"
      + EVENT_METHOD_EXIT
      + BR +Rep.getMethodRep(event.method())
      +")";
  }


  /**
   * A thread started
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_THREAD_START thread-start-function} {@link Rep#getThreadRep thread})
   * </pre>
   */
  private String threadStartEvent(ThreadStartEvent event) {
    JDE.debug(EVENTS, "got a THREAD START event");

    // Keep track of this thread for future reference
    m_debugger.getStore().put(event.thread());

    resumeApp &= false;
    return "(list '"
      + EVENT_THREAD_START
      + BR +Rep.getThreadRep(event.thread())
      +")";
  }

  /**
   * A thread died.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_THREAD_DEATH thread-death-function} {@link Rep#getThreadRep thread})
   * </pre>
   */
  private String threadDeathEvent(ThreadDeathEvent event) {
    JDE.debug(EVENTS, "got a THREAD DEATH event");

    resumeApp &= false;
    return "(list '"
      + EVENT_THREAD_DEATH
      + BR +Rep.getThreadRep(event.thread())
      +")";
  }


  /**
   * A class was prepared. The user might not have even requested for
   * this event: we set it up by default in {@link Debugger#start}
   * because we need it for resolution of specs.
   * <p>
   * If a user also requests for this event, a particular property is
   * likely not set, and that is how we know that we're supposed to
   * handle the event for the user.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_CLASS_PREPARE class-prepare-function} reference-type)
   * </pre>
   */
  private String classPrepareEvent(ClassPrepareEvent event) {
    // XXX - too many of these. JDE.debug(EVENTS, "got a CLASS PREPARE event");

    m_debugger.getEventRequestSpecList().resolve(event.referenceType());
    // now find out if this event was also requested by the user.
    // it will be, if the "default" property does NOT exists in the
    // corresponding request.
    EventRequest request = event.request();
    if (request.getProperty("default") == null) {
      // Event was requested by the user

      resumeApp &= false;
      return "(list '"+EVENT_CLASS_PREPARE
        +" \""+event.referenceType().name()+"\")";
    } else {
      // Not requested by the user, so don't return anything, but do
      // make sure the application is resumed.

      resumeApp &= true;
      return null;
    }
  }

  /**
   * A class was unloaded.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list {@link Protocol#EVENT_CLASS_UNLOAD class-unload-function} reference-type)
   * </pre>
   */
  private String classUnloadEvent(ClassUnloadEvent event) {
    JDE.debug(EVENTS, "got a CLASS UNLOAD event");

    resumeApp &= false;
    return "(list '"+EVENT_CLASS_UNLOAD
      +" \""+event.className()+"\")";
  }

  private String vmStartEvent(Event event) {
    JDE.debug(EVENTS, "got a VM START event");

    // Make sure that we keep track of the main thread. Other threads
    // will be tracked by the threadStart events (?? - may have to
    // subscribe to those events first... Otherwise, an approach is the
    // old one, of always writing the thread that reports an event to the
    // object store, but that's sooo ugly) / Petter
    ThreadReference thread = getEventThread(event);
    m_debugger.getStore().put(thread);

    resumeApp &= false;
    return "(list '"+EVENT_VM_START+")";
  }

  private String vmDeathEvent(Event event) {
    JDE.debug(EVENTS, "got a VM DEATH event");

    resumeApp &= true;
    return "(list '"+EVENT_VM_DEATH+")";
  }

  private String vmDisconnectEvent(Event event) {
    JDE.debug(EVENTS, "got a VM DISCONNECT event");

    m_connected = false;
    resumeApp &= true;
    try {
      m_debugger.shutdown();
    }
    catch (JDEException e) {
      JDE.signal(m_debugger.getProcID(), ERROR, "EventHandler.vmDisconnectEvent() caught exception: " + e, QUOTE);
    }
    return "(list '"+EVENT_VM_DISCONNECT+")";
  }

    /** Add an EventSetListener.  If the listener is already in the
     * list, nothing is done */
    void addEventSetListener(EventSetListener listener) {
	if (m_eventSetListeners.contains(listener))
	    return;
	m_eventSetListeners.add(listener);
    }

    /** Remove an EventSetListener.  If the listener is already in the
     * list, nothing is done */
    void removeEventSetListener(EventSetListener listener) {
	m_eventSetListeners.remove(listener);
    }

    /** This class determines whether to suspend the JVM based on the
     * event, and send the appropriate message to JDEE.  It used to be
     * the bulk of the { @link #run } method in this class. <p>
     *
     * Essentially, one by one, each event of the eventset
     * is sent to the corresponding "handler", and if at least one of them
     * returns a non-null value, we pass on the eventset (and the values
     * returned by the handlers) to jde. Otherwise, we just resume the vm
     * (regardless of the suspend policy) and wait for the next eventset.
     * <p>
     * This is the syntax of the event set:
     * <pre>
     * (JDE_BUG_EVENTSET <u>suspend-state</u> <u>thread</u> [<u>event-string</u>]+)
     * </pre>
     *
     * <ul>
     * <li> <u>suspend-state</u> is one of "all", "none" and "thread"
     * <li> <u>thread</u> is either nil, or the thread corresponding to the
     *      event set.
     * <li> <u>event-string</u> is the reply from a handler method. See the
     *      individual handler methods.
     * <li> The suspend policy is as follows. Some of the commands (see
     *      the {@link Protocol protocol}) allow for specifying the suspend
     *      policy for the corresponding event. For example, we might specify
     *      that the vm resume automatically after a  method entry event is
     *      reported.
     *      <p>
     *      Firstly, when multiple events are sent (in an event-set), the
     *      suspend policy of the entire event set is the one that would
     *      suspend the most threads. So, if a breakpoint-hit is sent with
     *      the method-entry event, and the breakpoint-hit wants to suspend
     *      the vm, while method-entry wants to resume, overall, the event
     *      set suspend policy will be to suspend
     *      <p>
     *      Further, some of the events might occur that haven't been
     *      explicitly requested, and hence we haven't set their suspend
     *      policy anyway. In these cases, and even otherwise, another
     *      decision is made at the time of handling of events regarding the
     *      suspension/resumption of the vm.
     *      <p>
     *      So, when an event set is received, each event is handled (via
     *      it's handler method). Each event ANDs to a variable resumeApp
     *      what it thinks should be done to the app: for example, the
     *      vmDeathEvent handler wants to resume, so it does
     *      <code>resumeApp &= true;</code>. In the same way, when a
     *      breakpoint is hit (and matches the constraints), it does a
     *      <code>resumeApp &= false:</code>.
     *      <p>
     *      After all events have been handled, we check the value of
     *      resumeApp. If it's true, we resume the vm. If not, we check
     *      the suspend-policy of the event set, and take appropriate action.
     *      <p>
     */
  private class EvtListener implements EventSetListener {
    // XXX changed from ArrayList, since LinkedList should be faster with
    // no indexed access needed. / Petter
    private final List events = new LinkedList();
    private String suspendStateString;

    public void eventSetReceived(EventSetEvent evt) {
      events.clear();
      EventIterator it = evt.getEventSet().eventIterator();

      while (it.hasNext()) {
	String eventLispDescription = handleEvent(it.nextEvent());
	// if the handler thinks the user doesn't <i>deserve</i>
	// the event, it'll return null. another microsoftism
	// in the code... :-(
	if (eventLispDescription != null) {
	  events.add(eventLispDescription);
	}
      }
    } // eventSetReceived

    public void debuggerSuspended(EventSetEvent evt) {
      switch(evt.getEventSet().suspendPolicy()) {
      case EventRequest.SUSPEND_ALL:
	suspendStateString = "all";
	break;

      case EventRequest.SUSPEND_EVENT_THREAD:
	suspendStateString = "thread";
	break;

      case EventRequest.SUSPEND_NONE:
	suspendStateString = "none";
	evt.getEventSet().resume();
	break;

      default:
	suspendStateString = "invalid";
	break;
      }
      finishEventSet(evt);
    } // debuggerSuspended

    public void debuggerResumed(EventSetEvent evt) {
      // this resume does a vm resume or a thread resume
      // depending on how it was suspended
      suspendStateString = "none";
      finishEventSet(evt);
      evt.getEventSet().resume();
      JDE.debug(FRAMEWORK, "EventHandler: VM resumed");
    }

    private void finishEventSet(EventSetEvent evt) {
      // Don't bother doing anything unless there are commands to send
      // to emacs
      if (events.size() > 0) {
	StringBuffer eventSetString = new StringBuffer("\"");
	eventSetString.append(suspendStateString);
	eventSetString.append("\"");

	ThreadReference eventThread = evt.getThreadReference();
	if (eventThread == null) {
	  eventSetString.append(" nil");
	} else {
	  eventSetString.append(BR);
	  eventSetString.append(Rep.getThreadRep(eventThread));
	}

	Iterator iter = events.iterator();
	while (iter.hasNext()) {
	  eventSetString.append(BR);
	  eventSetString.append(iter.next().toString());
	}

	// finally, we send the events to The Man. (or woman...)
	JDE.signal(m_debugger.getProcID(),
		   EVENTSET,
		   eventSetString.toString(),
		   NOQUOTE);
      } // if events
    } // finishEventSet
  } // private class EvtListener

} // EventHandler

/*
 * $Log: EventHandler.java,v $
 * Revision 1.7  2004/12/24 16:05:13  troy
 * Add window to display threads and stacks
 *
 * Revision 1.6  2003/04/29 16:51:56  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.5  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of EventHandler.java
