package jde.debugger;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import jde.debugger.command.CommandHandler;
import jde.debugger.command.DebugCommand;
import jde.debugger.command.DebugCommandFactory;

/**
 * Class of JDEbug debuggers.
 * <p>
 * This class defines methods for communicating with the JDE. It
 * maintains a list of active applications. It passes application
 * commands to the apps specified by the commands.
 * <p>
 * See {@link Protocol Protocol class} for command/response formats and
 * {@link EventHandler EventHandler} for event set formats.
 * <p>
 * JDEbug responds to every command with either a result or error
 * message.
 * <p>
 *
 * @author Amit Kumar
 * @since 0.1
 * @author Paul Kinnucan
 * @since 1.3
 * @version $Revision: 1.13 $
 */
public class JDEbug extends Thread implements Protocol {

  /**
   * The ID of jdebug. This is used by jde when issuing commands that
   * are not specific to any particular vm, for instance, 'quit', or
   * the command used to launch new application/vms.<br>
   * It is the Integer -1.
   */
  public static final Integer debuggerID = new Integer(-1);

  public static JDEbug theDebugger = new JDEbug();

  private boolean       m_shutdown = false;
  private CommandStream m_commandStream;

  /**
   * Protected constructor, since this is a singleton class.
   */
  protected JDEbug() {
  }

  public void init() throws IOException {
    // The debugger uses standard in to read commands from Emacs.
    m_commandStream = new CommandStream(new BufferedReader(new InputStreamReader(System.in)));
  }


  /**
   * Runs the debugger thread. This method reads and executes commands
   * from the JDE.
   */
  public void run() {

    JDE.debug(FRAMEWORK, "Starting JDEbug main loop");

    while (!m_shutdown) {
      JDE.debug(FRAMEWORK, "JDEbug waiting for cmd");
      List command = m_commandStream.nextCommand();
      JDE.debug(FRAMEWORK, "JDEbug got cmd");

      final Integer procID    = Integer.valueOf(command.get(0).toString());
      final Integer cmdID     = Integer.valueOf(command.get(1).toString());
      final String  cmdName   = command.get(2).toString().toLowerCase();
      final List    arguments = command.subList(3, command.size());

      try {
        CommandHandler handler = SessionManager.getCommandHandler(procID);

        if (handler == null) {
          throw new JDEException("no command handler found for debugger process: " + procID);
        }

	JDE.debug(EVENTS, "JDEbug firing command event");
	handler.fireCommandEvent(procID,
				 cmdID,
				 cmdName,
				 arguments);
      }
      catch (JDEException ex) {
        JDE.commandResult(cmdID, "Error occurred while executing " + cmdName +
                          ". Error: " + ex,
                          CMD_NOK, QUOTE);
      }
    }

    JDE.debug(FRAMEWORK, "jdebug main loop terminating");
    SessionManager.shutdown(); // Just in case no Quit command was issued
  }

  /**
   * Sets a flag that terminates the main loop (implemented in the
   * {@link #run} method).
   */
  public void shutdown() {
    m_shutdown = true;
  }

} // JDEbug

/*
 * $Log: JDEbug.java,v $
 * Revision 1.13  2003/04/29 16:51:57  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.12  2003/01/16 05:38:28  paulk
 * Cosmetic change
 *
 * Revision 1.11  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 * Revision 1.10  2001/08/14 05:15:01  paulk
 * Miscellaneous updates.
 *
 * Revision 1.9  2001/03/24 05:36:48  paulk
 * Updated to reflect reorganization of debuggee code.
 *
 * Revision 1.8  2000/10/20 04:18:29  paulk
 * *** empty log message ***
 *
 * Revision 1.7  2000/07/28 06:26:31  paulk
 * Committing all modified files.
 *
 * Revision 1.6  2000/02/14 06:25:34  paulk
 * Implemented workaround for JPDA bug that prevented setting of
 * breakpoints in inner classes.
 *
 * Revision 1.5  2000/01/31 12:41:39  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.4  2000/01/30 12:47:40  paulk
 * Changed to a singleton class. Implemented support for object-oriented
 * commands created by DebugCommandFactory. Reimplemented launch and
 * listen commands as object-oriented commands.
 *
 * Revision 1.3  2000/01/28 04:24:55  paulk
 * Threaded listen commands. Moved launch, attach, and listen commands
 * from Application to Jdebug class. Did general cleanup of Jdebug and
 * Application class, including using more specific names for some
 * variables, moving fields to the end of files, rewriting comments.
 *
 */


// End of JDEbug.java
