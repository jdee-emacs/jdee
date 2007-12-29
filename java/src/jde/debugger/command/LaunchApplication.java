package jde.debugger.command;

import jde.debugger.Debugger;
import jde.debugger.JDEException;
import jde.debugger.SessionManager;


/**
 * Launches an application.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * launch app_id [-use_executable javax] classname [args]
 * </pre>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.8 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 */
public class LaunchApplication extends DebugSessionCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    // XXX - fix the 'true' here, and define a better way to determine
    // whether to use a GUI or not.
    Debugger debugger = new Debugger(m_targetProcessID, true);
	
    SessionManager.registerDebugger(debugger);
	
    try {
      debugger.launchVM(m_cmdID, m_args);
      debugger.start();
    }
    catch (JDEException e) {
      SessionManager.deregisterDebugger(debugger);
      throw e;
    }
  }

  public Object clone() {return new LaunchApplication();}
  
} // LaunchApplication


/*
 * $Log: LaunchApplication.java,v $
 * Revision 1.8  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.7  2002/10/11 05:41:06  paulk
 * Fixed bug where the debuggee application launcher was downcasing the first vm command line argument. Thanks toEric W Brown <ewb@us.ibm.com> for this fix.
 *
 * Revision 1.6  2001/05/24 02:50:06  paulk
 * Moved  jde.signalCommandResult(....,
 * port) from LaunchApplication.java  into the same thread that creates a
 * server SIO socket to make sure that Emacs connects to this socket after its
 * creation. Thanks to "Eugene Gavrilov" <eag99@mail.ru>.
 *
 * Revision 1.5  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.4  2000/07/28 06:27:02  paulk
 * Committing all modified files.
 *
 * Revision 1.3  2000/03/03 07:45:05  paulk
 * Replaced backslashes with forward slashes in launch error messages.
 *
 * Revision 1.2  2000/01/31 12:41:45  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.1  2000/01/30 12:39:50  paulk
 * Defines command to launch debuggee application.
 *
 */

// End of LaunchApplication.java
