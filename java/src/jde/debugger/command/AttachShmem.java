package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Debugger;
import jde.debugger.SessionManager;


/**
 * Attaches to an already running application through shared memory.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * attach_shmem  app_id name
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> The debugee vm has to have been launched with the right parameters.
 * See the <italic>Connection and Invocation</italic> section of the
 * JPDA documentation.
 * </ul>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.4 $
 */
public class AttachShmem extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {
    // XXX - fix the 'true' here, and define a better way to determine
    // whether to use a GUI or not.
    Debugger debugger = new Debugger(m_targetProcessID, true);
	
    SessionManager.registerDebugger(debugger);
	
    try {
      debugger.attachVMShmem(m_args);
      debugger.start();
    }
    catch (JDEException e) {
      SessionManager.deregisterDebugger(debugger);
      throw e;
    }
	
    debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }
    
  public Object clone() {return new AttachShmem();}
    
    
} // AttachShmem


/*
 * $Log: AttachShmem.java,v $
 * Revision 1.4  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.3  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/02/02 05:58:12  paulk
 * Added command succeeded messages.
 *
 * Revision 1.1  2000/01/31 12:44:15  paulk
 * Attach existing application through shared memory.
 *
 */

// End of AttachShmem.java
