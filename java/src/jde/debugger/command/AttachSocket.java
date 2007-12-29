/*
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * $Revision: 1.5 $
 */

package jde.debugger.command;

import jde.debugger.Debugger;
import jde.debugger.JDEException;
import jde.debugger.SessionManager;


/**
 * Attaches to an already running application through a socket.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * attach_socket app_id -port p_value [-host h_value]
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> The debugee vm has to have been launched with the right parameters.
 * See the <italic>Connection and Invocation</italic> section of the
 * JPDA documentation.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.5 $
 */
public class AttachSocket extends DebugSessionCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  protected void doCommand() throws JDEException {
    // XXX - fix the 'true' here, and define a better way to determine
    // whether to use a GUI or not.
    Debugger debugger = new Debugger(m_targetProcessID, true);
	
    SessionManager.registerDebugger(debugger);
	
    try {
      debugger.attachVMSocket(m_args);
      debugger.start();
    }
    catch (JDEException e) {
      SessionManager.deregisterDebugger(debugger);
      throw e;
    }
	
    debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }

  public Object clone() {return new AttachSocket();}
 
  
} // AttachSocket


/*
 * $Log: AttachSocket.java,v $
 * Revision 1.5  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.4  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.3  2000/08/09 03:43:07  paulk
 * Fixed bug where JDEBug would not attach to a process running on a remote host because it was setting the wrong connector argument (host instead of hostname). Thanks to Matthew Conway <matt_conway@i2.com>.
 *
 * Revision 1.2  2000/02/02 05:58:12  paulk
 * Added command succeeded messages.
 *
 * Revision 1.1  2000/01/31 12:45:08  paulk
 * Attach existing application through socket.
 *
 */

// End of AttachSocket.java
