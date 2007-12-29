package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Debugger;
import jde.debugger.SessionManager;


/**
 * Listens on a socket for a debuggee application 
 * requesting debug services.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * listen_socket app_id port 
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 */
public class ListenSocket extends DebugSessionCommand {
  
  public ListenSocket() {
  }
    
  protected void doCommand() throws JDEException {
    // XXX - fix the 'true' here, and define a better way to determine
    // whether to use a GUI or not.
    Debugger debugger = new Debugger(m_targetProcessID, true);

    if (m_args.size() < 1)
      throw new JDEException("Missing name");
	
    final String address = m_args.remove(0).toString();
	
    SessionManager.registerDebugger(debugger);
    debugger.listenSocket(address);
	

    debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }
    
  public Object clone() {return new ListenSocket();}
    
  
} // ListenSocket


/*
 * $Log: ListenSocket.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/30 12:45:18  paulk
 * Defines command to listen on a socket for a debuggee application
 * requiring debugger services.
 *
 */

// End of ListenSocket.java
