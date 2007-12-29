package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.SessionManager;
import jde.debugger.Debugger;


/**
 *  Listens in shared memory for a debuggee vm requesting debug
 *  services.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * listen_shmem  app_id name
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.4 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 */
public class ListenShmem extends DebugSessionCommand {
  
  protected void doCommand() throws JDEException {
    // XXX - fix the 'true' here, and define a better way to determine
    // whether to use a GUI or not.
    Debugger debugger = new Debugger(m_targetProcessID, true);

    if (m_args.size() < 1)
      throw new JDEException("Missing name");
	
    final String address = m_args.remove(0).toString();
	
    SessionManager.registerDebugger(debugger);
    debugger.listenShmem(address);
	
    debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }	

  public Object clone() {return new ListenShmem();}
    
} // ListenShmem


/*
 * $Log: ListenShmem.java,v $
 * Revision 1.4  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.3  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/04/10 05:44:54  paulk
 * Corrected spelling error in message.
 *
 * Revision 1.1  2000/01/30 12:42:19  paulk
 * Defines command to attach debugger to an existing application through
 * shared memory.
 *
 */

// End of ListenShmem.java
