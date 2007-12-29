package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.SessionManager;
import jde.debugger.JDE;


/**
 * 'finish' command. 
 * <p>
 * The way this is implemented, the finishing of a process is done in two
 * steps: first, the debugger is told to stop running the VM handling
 * the debuggee process. When the VM stops executing, it sends a VMDisconnected
 * event, which means that the debugger shuts itself down properly. The Finish
 * command handles only the first step, telling the debugger to stop executing
 * the debuggee VM.
 *
 * <b>Syntax:</b>
 * <pre>
 * finish
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> if multiple VMs are being debugged, this command will
 * kill the one corresponding to app_id, retaining others.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 */
public class Finish extends DebugProcessCommand {
    
  /**
   * 
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    m_debugger.stopExecution();
    JDE.commandResult(m_cmdID, "Shutting down process", CMD_OK, QUOTE);
  }
    
  public Object clone() {return new Finish();}

} // Finish

/*
 * $Log: Finish.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:12  paulk
 * Initial version.
 *
 *
 */

// End of Finish.java
