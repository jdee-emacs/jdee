package jde.debugger.command;

import java.util.Iterator;
import java.util.List;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.request.EventRequestManager;
import com.sun.jdi.request.MethodEntryRequest;
import com.sun.jdi.request.MethodExitRequest;
import jde.debugger.Etc;
import jde.debugger.JDEException;


/**
 * 'trace_methods' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * trace_methods <u>type</u>
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
 *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id <u>requestID</u>)
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>type</u> is either "entry" or "exit"
 * <li> Use <u>requestID</u> to cancel the trace request.
 * </ul>
 *
 * <p>
 * @see jde.debugger.EventHandler#methodEntryEvent(MethodEntryEvent)
 * @see jde.debugger.EventHandler#methodExitEvent(MethodExitEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class TraceMethods extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    String type = m_args.remove(0).toString().toLowerCase();
    if (!(type.equals("entry") || type.equals("exit")))
      throw new JDEException("Invalid type");
	
    Object          thread = Etc.getThreadFromArgs(m_args);
    ObjectReference tRef   = null;

    if (thread == null) {
      tRef = null;
    } else if (thread instanceof Long) {
      tRef = (ObjectReference) m_debugger.getStore().get(thread);
      if (tRef == null) {
        throw new JDEException("No such thread exists");
      } else if (!(tRef instanceof ThreadReference)) {
        throw new JDEException("No such thread exists (anymore?)");
      }
    } else if (thread instanceof String) {
      tRef = m_debugger.getThreadReference((String) thread);
    }
	
    List classFilters   = Etc.getClassFiltersFromArgs(m_args);
    List classExFilters = Etc.getClassExFiltersFromArgs(m_args);
	
    Long requestID      = null;
	
    EventRequestManager em = m_debugger.getVM().eventRequestManager();
	
    if (type.equals("entry")) {
	    
      MethodEntryRequest mer = em.createMethodEntryRequest();
	    
      if (tRef != null) 
        mer.addThreadFilter((ThreadReference) tRef);
	    
      mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (classFilters != null) {
        Iterator it = classFilters.iterator();
        while (it.hasNext())
          mer.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
        Iterator it = classExFilters.iterator();
        while (it.hasNext())
          mer.addClassExclusionFilter(it.next().toString());
      }
      requestID = m_debugger.addIdentifiableRequest(mer);
	    
    } else if (type.equals("exit")) {
	    
      MethodExitRequest mer = em.createMethodExitRequest();
	    
      if (tRef != null) 
        mer.addThreadFilter((ThreadReference) tRef);
	    
      mer.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (classFilters != null) {
        Iterator it = classFilters.iterator();
        while (it.hasNext())
          mer.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
        Iterator it = classExFilters.iterator();
        while (it.hasNext())
          mer.addClassExclusionFilter(it.next().toString());
      }
      requestID = m_debugger.addIdentifiableRequest(mer);
    }
	
    m_debugger.signalCommandResult(m_cmdID, requestID.toString(), CMD_OK, NOQUOTE);
  }
    
  public Object clone() {return new TraceMethods();}
    
} // TraceMethods

/*
 * $Log: TraceMethods.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:26  paulk
 * Initial revision.
 *
 *
 */

// End of TraceMethods.java
