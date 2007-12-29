package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.JDEbug;


/**
 * A command handler that handles only instances of
 * DebugSessionCommand. There should really only be one instance of
 * this class, under the control of the {@link
 * jde.debugger.SessionManager session manager}, but there are no
 * checks to ensure this.
 *
 * <p>
 * Created: Tue Jan 08 11:23:20 2002
 *
 * @author Petter Måhlén
 * @version 1.0
 * @see DebugSessionCommand
 */

public class SessionCommandHandler extends CommandHandler {
  public SessionCommandHandler(){
    super();
    setName("Session Command Handler");
  }

  /**
   * Method for other threads/objects to use to ask the command
   * handler to deal with the given DebugCommand. This method checks
   * that the command is a DebugSessionCommand, and then
   * {@link CommandHandler#queue queues} the command for further
   * processing.
   *
   * @param cmd a <code>DebugCommand</code> value
   * @exception JDEException if an error occurs
   * @see DebugCommand
   * @see DebugSessionCommand
   */
  public void handle(DebugCommand cmd) throws JDEException {
    if (cmd instanceof DebugSessionCommand) {
      queue(cmd);
    }
    else {
      throw new JDEException("INTERNAL ERROR: Non-session command " + cmd + " passed to handler " + getProcID());
    }
  }

  public Integer getProcID() {
    // a bit of a hack this..
    return JDEbug.debuggerID;
  }
}// SessionCommandHandler
