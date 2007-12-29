package jde.debugger.command;


import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;
import jde.debugger.JDE;



/**
 * Stops the VM and debug the specified thread.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * debug_thread threadID
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *  
 * @author Raffael Herzog
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */
public class DebugThread extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
	
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");
	
    // find the thread to debug
    Long uniqueID = Etc.safeGetLong(m_args.remove(0), "thread ID");
	
    ThreadReference tRef = (ThreadReference) m_debugger.getStore().get(uniqueID);
        
    // it should exist
    if (tRef == null) {
      throw new JDEException("Invalid thread ID or the thread is dead");
    }
	
    // suspend the whole vm
    m_debugger.getVM().suspend();
	
    // simulate a step event
    try {
      final String locationRep = Rep.getLocationRep(tRef.frame(0).location());
      final String lispForm    = "(list '"
        +EVENT_STEP_COMPLETED
        +" "+locationRep
        +")";
	    
      JDE.signal(m_debugger.getProcID(), EVENTSET, 
                 "\"thread\" " + 
                 Rep.getThreadRep(tRef) + BR + lispForm);
      m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
    }
    catch ( IncompatibleThreadStateException exc ) {
      // this should never happen...
      throw new JDEException(exc.toString());
    }
  }

  public Object clone() {return new DebugThread();}
  
} // DebugThread

/*
 * $Log: DebugThread.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/07/07 04:51:35  paulk
 * Removed DOS line endings.
 *
 * Revision 1.1  2001/07/06 02:04:50  paulk
 * Initial revision.
 *
 *
 *
 */

// End of DebugThread.java
