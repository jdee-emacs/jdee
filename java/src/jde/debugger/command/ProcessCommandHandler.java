package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.Debugger;
import jde.debugger.SessionManager;


/**
 * A command handler for DebugProcessCommands. There will be one such
 * command for each Debugger instance.
 *
 * <p>
 * Created: Tue Jan 08 11:23:20 2002
 *
 * @author  Petter Måhlén
 * @version $Revision: 1.2 $
 */

public class ProcessCommandHandler extends CommandHandler {
  private Debugger m_debugger;
    
  public ProcessCommandHandler(Debugger debugger) {
    super();
    m_debugger = debugger;
  }
    
  /**
   * Method for other threads/objects to use to ask the command
   * handler to deal with the given DebugCommand. This method checks
   * that the command is a DebugProcessCommand, keeps track of the
   * debugger that a command is being executed within, and then
   * {@link CommandHandler#queue queues} the command for further
   * processing.
   *
   * @param cmd a <code>DebugCommand</code> value
   * @exception JDEException if an error occurs
   * @see DebugCommand
   * @see DebugProcessCommand
   */
  public void handle(DebugCommand cmd) throws JDEException {
    if (cmd instanceof DebugProcessCommand) {
      // make sure that the command has access to the data in the debugger for
      // this process.
      ((DebugProcessCommand) cmd).setDebugger(m_debugger);
      queue(cmd);
    }
    else {
      throw new JDEException("INTERNAL ERROR: Non-process command " + cmd + " passed to handler " + getProcID());
    }
  }
    
  public Integer getProcID() {
    return m_debugger.getProcID();
  }
}// ProcessCommandHandler

/*
 * $Log: ProcessCommandHandler.java,v $
 * Revision 1.2  2003/04/05 04:22:05  paulk
 * Removed carriage returns.
 *
 * Revision 1.1  2003/01/15 06:05:00  paulk
 * Initial revision.
 *
 */

// End of ProcessCommandHandler.java
