package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.event.*;
import com.sun.jdi.request.*;

import java.util.*;
import jde.debugger.Protocol;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.SessionManager;

/**
 * EventRequestSpec.java
 * <p>
 * A request specification. This is used for watchpoints, exception-catches,
 * and breakpoints, and provides a mechanism for implementing deferral.
 * <p>
 * The intuition is that the user should be allowed to specify things like
 * breakpoints, even though the corresponding classes haven't been loaded
 * yet.
 * <p>
 * When the user does a, for example, "break on_line test.Test 42", jdebug
 * tries to find if test.Test has been loaded. If it has, it tries to set
 * the breakpoint, and sends an error on failure.
 * <p>
 * If, however, no class matching test.Test exists, jdebug places this
 * "spec" in a list, and each time a class is prepared, matches the class
 * with the spec. If the spec matches, it tries to set the breakpoint /
 * watchpoint / exception-catch. If it works, fine, else it sends the
 * error over to jde.
 * <p>
 * This also allows for neat things like setting breakpoints on source file
 * + line number combinations, since each reference type (given it was
 * compiled with debug info) also contains the source file name in it.
 * <p>
 * Information that would normally be stuck right into the actual requests,
 * for example a thread filter, is stored in the spec until the time it can
 * resolve the request. At that time, it is set in {@link #setRequest}.
 * <p>
 * XXX
 * <p>
 * Note that as of now, when the doc is being written, there is no way of
 * ascertaining if the user mistyped the referencetype name/pattern, since
 * jdebug will just wait <i>ad infinitum</i> for that class to be prepared.
 * <p>
 * Created: Thu Jul 15 12:17:34 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version 
 */

abstract public class EventRequestSpec implements Protocol {

  private final Long m_ID;


  /**
   * While setting some specs, the user is allowed to specify a boolean
   * expression that must evaluate to true if the event is to be passed
   * on to the user. This expression is stored in the EventRequest object
   * as a property. On an event, the EventRequest object is also passed,
   * and the property can then be extracted, evaluated, and
   * handled correspondingly
   */
  public static final Object expressionKey = "expr";
  private String expr = null;

  public EventRequestSpec(ReferenceTypeSpec refSpec) {
    this.refSpec = refSpec;
    
    m_ID = SessionManager.generateObjectID();
  }


  public void setExpression(String expr) {
    this.expr = expr;
    if (request != null)
      request.putProperty(expressionKey, expr);
  }

  /**
   * For specs that allow for it,
   * the {@link #thread thread} object is either null, a Long, or a
   * String. Depending on the type, it is matched at the time the
   * breakpoint is hit. If it matches the thread, the breakpoint is
   * deemed non-hit.
   */
  public static final Object threadKey = "thread";
  private Object thread = null;
  public void setThread(Object thread) {
    this.thread = thread;
    if (request != null)
      request.putProperty(threadKey, thread);
  }

  /**
   * Determines the suspend policy for the corresponding event. See
   * {@link jde.debugger.EventHandler EventHandler}  for more details
   * <p>
   * Note that the request needs to be disabled for us to be able to
   * do this
   */
  private int suspendPolicy = EventRequest.SUSPEND_ALL; // the default
  public void setSuspendPolicy(int policy) {
    this.suspendPolicy = policy;
    if (request != null) {
      request.setSuspendPolicy(policy);
      //	    System.out.println("XXX:"+((request.suspendPolicy() == EventRequest.SUSPEND_NONE)?"none":"not none"));
    }
  }

  /**
   * Stores a list of class filters that are to be applied to the event
   * request when it gets resolved. This will restrict any events from
   * being reported only if they match the class filters.
   * <p>
   * Not all event-requests support class filters. This filters will be
   * silently ignored for event-requests that do not support them.
   */
  private List classFilters = null;
  public void setClassFilters(List filters) {
    this.classFilters = filters;
    if (request != null)
      installClassFilters(request);
  }

  /**
   * Install class filters.
   * Note that the request needs to be disabled for us to be able to
   * do this
   */
  private void installClassFilters(EventRequest request) {
    if (classFilters == null) return;
    Iterator iter = classFilters.iterator();
    while (iter.hasNext()) {
      String f = iter.next().toString();
      if (request instanceof ClassPrepareRequest) {
        ((ClassPrepareRequest)request).addClassFilter(f);
      } else if (request instanceof ClassUnloadRequest) {
        ((ClassUnloadRequest)request).addClassFilter(f);
      } else if (request instanceof ExceptionRequest) {
        ((ExceptionRequest)request).addClassFilter(f);
      } else if (request instanceof WatchpointRequest) {
        ((WatchpointRequest)request).addClassFilter(f);
      }
    }
  }


  /**
   * Stores a list of class exclusion filters that are to be applied to
   * the event
   * request when it gets resolved. This will restrict any events from
   * being reported only if they do <b>not</b> match the class ex-filters.
   * <p>
   * Not all event-requests support class ex-filters. This filters will be
   * silently ignored for event-requests that do not support them.
   */
  private List classExFilters = null;
  public void setClassExFilters(List filters) {
    this.classExFilters = filters;
    if (request != null)
      installClassExFilters(request);
  }

  /**
   * Install class exclusion filters.
   * Note that the request needs to be disabled for us to be able to
   * do this
   */
  private void installClassExFilters(EventRequest request) {
    if (classExFilters == null) return;
    Iterator iter = classExFilters.iterator();
    while (iter.hasNext()) {
      String f = iter.next().toString();
      if (request instanceof ClassPrepareRequest) {
        ((ClassPrepareRequest)request).addClassExclusionFilter(f);
      } else if (request instanceof ClassUnloadRequest) {
        ((ClassUnloadRequest)request).addClassExclusionFilter(f);
      } else if (request instanceof ExceptionRequest) {
        ((ExceptionRequest)request).addClassExclusionFilter(f);
      } else if (request instanceof WatchpointRequest) {
        ((WatchpointRequest)request).addClassExclusionFilter(f);
      }
    }
  }



  /**
   * Unlike the original javadt (from which most of the spec code comes,
   * we do not maintain three spec states, ie, resolved, unresolved, and
   * error. In our case, on an error, we simply remove the spec from the
   * list of specs being maintained by the application, and inform the
   * jde of this fact (that there was an error resolving the spec)
   * (using app.removeSpecAndInformJDE(this))
   * <p>
   * XXX see if the above needs to be changed
   * <p>
   * Consequently, we only need keep track of if we're resolved yet or
   * not.
   */
  boolean isResolved = false;

  /**
   * Used to cross-reference the EventRequest to its
   * spec.
   */
  static public final Object specPropertyKey = "spec";

  /**
   * The reference type spec for this event request spec: this should 
   * match the ReferenceType for the spec to be
   * "resolved"
   */
  ReferenceTypeSpec refSpec;

  /**
   * The EventRequest corresponding to this spec. This
   * is set when the spec resolves successfully.
   */
  EventRequest request = null;
    
  /** get the id corresponding to this spec */
  public Long getID() { return m_ID; }

  /**
   * sets the request up. This is called when a resolve succedes. 
   */
  void setRequest(EventRequest request) {
    this.request = request;
    // put a link to this spec in the request itself. a sort of
    // cross referencing
    request.putProperty(specPropertyKey, this);
    request.putProperty(threadKey, thread);
    request.putProperty(expressionKey, expr);
    request.setSuspendPolicy(suspendPolicy);
    installClassFilters(request);
    installClassExFilters(request);
    //	System.out.println("YYY:"+((request.suspendPolicy() == EventRequest.SUSPEND_NONE)?"none":"not none"));

    request.enable();
  }

  public EventRequest getEventRequest() { return request; }

  /**
   * This function is called to resolve an {@link EventRequestSpec} when 
   * the ReferenceType is known to match
   * <p>
   * if any errors occur at any time during resolution of the event-
   * requestspec, its entry in the {@link EventRequestSpecList} is
   * removed, and jde informed about it 
   * <p>
   * @return true if the resolution was successful
   */
  abstract boolean resolve(ReferenceType refType) throws JDEException;

  /**
   * This function is called after each new class is loaded. If this
   * spec hasn't been resolved yet, it's attempted to be resolved. the
   * handling is almost exactly the same as that in
   * {@link #attemptImmediateResolve}
   * <p>
   */
  public void attemptResolve(ReferenceType refType, Integer procID) throws JDEException {
    if (!isResolved() && refSpec.matches(refType)) {
      if (resolve(refType)) {
        JDE.debug(EVENTS, "resolve succeeded: " + refType.name());
        setIsResolved(procID);
      }
    }
  }

  /**
   * Attempts to resolve the eventRequestSpec immediately. There are
   * three possibilities:
   * <ul>
   * <li> The corresponding class hasn't been loaded. the method returns
   * normally.
   * <li> The class has been loaded, and the resolution is successful.
   * The method returns normally, having set the isResolved flag in this
   * class
   * <li> The class has been loaded, but there was an error trying to
   * resolve this spec. An exception is raised, and is caught in this
   * method. This spec is then removed from the spec list kept in the
   * Application object, and jde informed that this spec could not be
   * resolved, so that the UI can take appropriate actions (for example
   * removing the highlighting of a breakpoint)
   * </ul>
   */
  void attemptImmediateResolve(VirtualMachine vm, Integer procID) throws JDEException {
    Iterator iter = vm.allClasses().iterator();
	
    // XXX - I added the !isResolved condition, to save some loop iterations.
    //       Since I don't fully understand it, it could be a bug, but I think not. / Petter
    while (iter.hasNext() && !isResolved) {
      ReferenceType refType = (ReferenceType)iter.next();
      if (refSpec.matches(refType)) {
        if (resolve(refType)) {
          setIsResolved(procID);
        } 
      }
    }
  }
    
  /**
   * @return true if this spec has been resolved.
   */
  public boolean isResolved() {
    return isResolved;
  }

  /**
   * set resolved status and notify Emacs.
   */
  public void setIsResolved(Integer procID) {
    isResolved = true;
    JDE.signal(procID, SPEC_RESOLVED, m_ID.toString(), NOQUOTE);
  }

  boolean isJavaIdentifier(String s) {
    if (s.length() == 0) {                              
      return false;
    }

    if (! Character.isJavaIdentifierStart(s.charAt(0))) {
      return false;
    }

    for (int i = 1; i < s.length(); i++) {
      if (! Character.isJavaIdentifierPart(s.charAt(i))) {
        return false;
      }
    }

    return true;
  }

} // EventRequestSpec

/*
 * $Log: EventRequestSpec.java,v $
 * Revision 1.4  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of EventRequestSpec.java
