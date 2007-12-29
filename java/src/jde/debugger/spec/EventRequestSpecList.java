package jde.debugger.spec;


import com.sun.jdi.*;
import com.sun.jdi.event.*;
import com.sun.jdi.request.*;

import java.util.*;
import jde.debugger.JDEException;
import jde.debugger.JDE;
import jde.debugger.Debugger;
import jde.debugger.Protocol;

/**
 * EventRequestSpecList.java
 * <p>
 * Maintains a list of all the "specs", i.e. requests by the user for
 * notification of a particular type of event. Not all commands create
 * specs: watchpoints, breakpoints, and exception catches do.
 * <p>
 * See {@link EventRequestSpec} for more details.
 * <p>
 * Created: Thu Jul 15 11:26:23 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class EventRequestSpecList implements Protocol {

  /**
   * a Hashmap of all the {@link EventRequestSpec}s for the application,
   * specID -> spec
   */
  private Map      m_eventRequestSpecs;
  private Debugger m_debugger;

  public EventRequestSpecList(Debugger debugger) {
    m_eventRequestSpecs = new HashMap();
    m_debugger          = debugger;
  }
    
  /** 
   * Resolve all deferred eventRequests waiting for 'refType'. This is
   * called when a new reference type is prepared. We iterate through
   * all the requestspecs, calling their
   * {@link EventRequestSpec#attemptResolve attemptResolve}
   * methods.
   *
   * @param refType The reference type that was recently prepared
   */
  public void resolve(ReferenceType refType) {
    synchronized(m_eventRequestSpecs) {
      Iterator iter = m_eventRequestSpecs.values().iterator();
      while (iter.hasNext()) {
        EventRequestSpec ers = (EventRequestSpec) iter.next();
		
        try {
          ers.attemptResolve(refType, m_debugger.getProcID());
        }
        catch (JDEException e) {
          JDE.debug(EXCEPTION, e.toString());
          // XXX should I do this: 
          m_eventRequestSpecs.remove(ers.getID());
          // I think so.
        }
      }
    }
  }
    
  /** Install a new event request spec - XXX synchronize throughout!? */
  public void install(EventRequestSpec ers) throws JDEException {
    synchronized (m_eventRequestSpecs) {
      m_eventRequestSpecs.put(ers.getID(), ers);
    }

    try {
      ers.attemptImmediateResolve(m_debugger.getVM(), m_debugger.getProcID());
    }
    catch (JDEException e) {
      synchronized (m_eventRequestSpecs) {
        m_eventRequestSpecs.remove(ers.getID());
      }
      // and propagate the problem
      throw e;
    }
  }

  /** Delete an existing event request spec */
  public void delete(EventRequestSpec ers) {
    EventRequest request = ers.getEventRequest();
    synchronized (m_eventRequestSpecs) {
      m_eventRequestSpecs.remove(ers.getID());
    }
    // XXX - prolly want to change the below stuff
    if (request != null) {
      request.virtualMachine().eventRequestManager()
        .deleteEventRequest(request);
    }
  }

  /** remove a spec based on its specID */
  public void removeSpec(Long specID)
    throws JDEException {
    synchronized (m_eventRequestSpecs) {
      if (!m_eventRequestSpecs.containsKey(specID))
        throw new JDEException("'"+specID+"' doesn't exist");
      delete((EventRequestSpec)m_eventRequestSpecs.get(specID));
    }
  }

  public EventRequestSpec createExceptionIntercept(String classPattern, 
                                                   boolean notifyCaught, 
                                                   boolean notifyUncaught){
    ReferenceTypeSpec refSpec =
      new PatternReferenceTypeSpec(classPattern);
    EventRequestSpec ers =
      new ExceptionSpec(refSpec, notifyCaught, notifyUncaught);
    return ers;
  }

  public WatchpointSpec createAccessWatchpoint
    (String classPattern, String m) {
    ReferenceTypeSpec refSpec = 
      new PatternReferenceTypeSpec(classPattern);
    WatchpointSpec ers =
      new AccessWatchpointSpec(refSpec, m);
    return ers;
  }

  public WatchpointSpec createModificationWatchpoint
    (String classPattern, String m) {
    ReferenceTypeSpec refSpec = 
      new PatternReferenceTypeSpec(classPattern);
    WatchpointSpec ers =
      new ModificationWatchpointSpec(refSpec, m);
    return ers;
  }

  public EventRequestSpec createClassLineBreakpoint
    (String classPattern, int line) {
    ReferenceTypeSpec refSpec = 
      new PatternReferenceTypeSpec(classPattern);
    EventRequestSpec ers =
      new LineBreakpointSpec(refSpec, line);
    return ers;
  }

  public EventRequestSpec createSourceLineBreakpoint
    (String sourceName, int line) {
    ReferenceTypeSpec refSpec = 
      new SourceNameReferenceTypeSpec(sourceName, line);
    EventRequestSpec ers = 
      new LineBreakpointSpec(refSpec, line);
    return ers;
  }
        
  public EventRequestSpec createMethodBreakpoint
    (String classPattern, String methodId, List methodArgs) {
    ReferenceTypeSpec refSpec = 
      new PatternReferenceTypeSpec(classPattern);
    EventRequestSpec e = 
      new MethodBreakpointSpec(refSpec, methodId, methodArgs);
    return e;
  }
        
} // EventRequestSpecList

/*
 * $Log: EventRequestSpecList.java,v $
 * Revision 1.4  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of EventRequestSpecList.java
