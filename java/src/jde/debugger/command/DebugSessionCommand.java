package jde.debugger.command;

import java.util.List;

import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.JDEbug;


/**
 * DebugSessionCommand.java
 *
 *
 * Created: Fri Jan 28 21:59:32 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.5 $
 */

abstract public class DebugSessionCommand extends DebugCommand {
  /**
   * The process (debugger process) ID that this command is 
   * targeted at.
   */
  protected Integer m_targetProcessID;
    
  public DebugSessionCommand() { }

  public void init(Integer cmdID, String cmdName, List args) throws JDEException {

    super.init(cmdID, cmdName, args);

    if (cmdName.equals("quit")) return;

    if (args.size() < 1 )
      throw new JDEException("Missing application ID");
	
    // the app id with which it will be known.
    // note that we remove the arguments as we consume them from the
    // list.
    m_targetProcessID = new Integer(Etc.safeGetint(args.remove(0), "target process ID"));
	
    // the app id cannot be same as the debugger ID (-1)
    if (m_targetProcessID.equals(JDEbug.debuggerID)) {
      throw new JDEException("Invalid Application ID");
    }
  }
} // DebugSessionCommand


/*
 * $Log: DebugSessionCommand.java,v $
 * Revision 1.5  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.4  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.3  2000/02/02 06:01:14  paulk
 * Removed the get connector list code from getConnectors method and
 * instead made the connector list a static member that is initialized
 * once per session. Did this because it is suspected that getting the
 * connector list on the command thread was causing the debugger to hang
 * on some Windows/NT systems.
 *
 * Revision 1.2  2000/02/01 06:02:47  paulk
 * Added special handling for quit command.
 *
 * Revision 1.1  2000/01/30 12:37:44  paulk
 * Defines debug session commands (e.g., launch, attach, quit, etc.).
 *
 */

// End of DebugSessionCommand.java
