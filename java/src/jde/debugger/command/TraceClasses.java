package jde.debugger.command;

import java.util.Iterator;
import java.util.List;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.ClassUnloadRequest;
import com.sun.jdi.request.EventRequestManager;
import jde.debugger.Etc;
import jde.debugger.JDEException;


/**
 * 'trace_classes' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * trace_classes <u>type</u>
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
 * <li> <u>type</u> is either "preparation" or "unloading"
 * <li> use <u>requestID</u> to cancel the trace request.
 * </ul>
 *
 * <p>
 * @see jde.debugger.EventHandler#classPrepareEvent(ClassPrepareEvent)
 * @see jde.debugger.EventHandler#classUnloadEvent(ClassUnloadEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class TraceClasses extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");
	
    String type = m_args.remove(0).toString().toLowerCase();
	
    if (!(type.equals("preparation") || type.equals("unloading")))
      throw new JDEException("Invalid type");
	
    Long requestID = null;
	
    List classFilters   = Etc.getClassFiltersFromArgs(m_args);
    List classExFilters = Etc.getClassExFiltersFromArgs(m_args);
	
    EventRequestManager em = m_debugger.getVM().eventRequestManager();
	
    if (type.equals("preparation")) {
	    
      ClassPrepareRequest cpr = em.createClassPrepareRequest();
	    
      cpr.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (classFilters != null) {
        Iterator it = classFilters.iterator();
        while (it.hasNext())
          cpr.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
        Iterator it = classExFilters.iterator();
        while (it.hasNext())
          cpr.addClassExclusionFilter(it.next().toString());
      }
      requestID = m_debugger.addIdentifiableRequest(cpr);
	    
    } else if (type.equals("unloading")) {
	    
      ClassUnloadRequest cur = em.createClassUnloadRequest();
	    
      cur.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
	    
      if (classFilters != null) {
        Iterator it = classFilters.iterator();
        while (it.hasNext())
          cur.addClassFilter(it.next().toString());
      }
      if (classExFilters != null) {
        Iterator it = classExFilters.iterator();
        while (it.hasNext())
          cur.addClassExclusionFilter(it.next().toString());
      }
      requestID = m_debugger.addIdentifiableRequest(cur);
    }
	

    m_debugger.signalCommandResult(m_cmdID, requestID.toString(), CMD_OK);
  }
    
  public Object clone() {return new TraceClasses();}
    
} // TraceClasses

/*
 * $Log: TraceClasses.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of TraceClasses.java
