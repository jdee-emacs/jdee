package jde.debugger.command;

import java.util.List;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.ThreadDeathRequest;
import com.sun.jdi.request.ThreadStartRequest;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.ObjectStore;


/**
 * 'trace_threads' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * trace_threads <u>type</u> [threadID]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>type</u> can be either "start" or "death"
 * <li> If no threadID is specified, all the corresponding thread
 * events are raised.
 * </ul>
 *
 * <p>
 * @see jde.debugger.EventHandler#threadStartEvent(ThreadStartEvent)
 * @see jde.debugger.EventHandler#threadDeathEvent(ThreadDeathEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 */
public class TraceThreads extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    String type = m_args.remove(0).toString().toLowerCase();
	
    if (!(type.equals("start") || type.equals("death")))
      throw new JDEException("Invalid type");
	
    List classFilters   = Etc.getClassFiltersFromArgs(m_args);
    List classExFilters = Etc.getClassExFiltersFromArgs(m_args);
	
    EventRequestManager em = m_debugger.getVM().eventRequestManager();
	
    Long        requestID = null;
    ObjectStore store     = m_debugger.getStore();
	
    if (type.equals("start")) {
      ThreadStartRequest ter = em.createThreadStartRequest();
	    
      ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (m_args.size() > 0) {
        Long            threadID = Etc.safeGetLong(m_args.remove(0), "thread ID");
        ObjectReference tRef     = store.get(threadID);
        if (tRef == null) {
          throw new JDEException("No such thread exists");
        } else if (!(tRef instanceof ThreadReference)) {
          throw new JDEException("No such thread exists (anymore?)");
        }
        ter.addThreadFilter((ThreadReference)tRef);
      }
	    
      requestID = m_debugger.addIdentifiableRequest(ter);
	    
    } else if (type.equals("death")) {
	    
      ThreadDeathRequest ter = em.createThreadDeathRequest();
	    
      ter.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (m_args.size() > 0) {
        Long            threadID = Etc.safeGetLong(m_args.remove(0), "thread ID");
        ObjectReference tRef     = store.get(threadID);
        if (tRef == null) {
          throw new JDEException("No such thread exists");
        } else if (!(tRef instanceof ThreadReference)) {
          throw new JDEException("No such thread exists (anymore?)");
        }
        ter.addThreadFilter((ThreadReference)tRef);
      }
	    
      requestID = m_debugger.addIdentifiableRequest(ter);
    }
	

    m_debugger.signalCommandResult(m_cmdID, requestID.toString(), CMD_OK);
  }
    
    

  public Object clone() {return new TraceThreads();}
  
} // TraceThreads

/*
 * $Log: TraceThreads.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:26  paulk
 * Initial revision.
 *
 *
 */

// End of TraceThreads.java
