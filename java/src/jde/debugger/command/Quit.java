package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.JDEbug;
import jde.debugger.SessionManager;
import jde.debugger.JDE;


/**
 * Kills the debugger.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * quit
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 */
public class Quit extends DebugSessionCommand {
    
  protected void doCommand() throws JDEException {
    JDE.debug(EVENTS, "shutting down debugger");
    SessionManager.shutdown();
	
    JDE.commandResult(m_cmdID, null, CMD_OK);
    JDEbug.theDebugger.shutdown();

    // XXX Petter:
    // I would very much prefer not doing a System.exit() here, since
    // that has a tendency to hide threading problems, locked threads, etc.
    // But the main thread is normally blocked in CommandStream.nextToken(),
    // and I know that there is/was a bug in the AWT thread management as
    // well, meaning that they can only be terminated by exiting the whole
    // process.
    System.exit(0);
  }
    
  public Object clone() {return new Quit();}
    
} // Quit


/*
 * $Log: Quit.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/31 12:47:01  paulk
 * Quit debugger.
 *
 */

// End of Quit.java
