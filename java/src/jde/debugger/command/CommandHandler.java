package jde.debugger.command;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import jde.debugger.CommandEvent;
import jde.debugger.CommandListener;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.Protocol;





/**
 * Abstract class that provides most of the implementation of the
 * command handlers. A command handler is run by a thread, which picks
 * commands from a queue and executes them. Commands are placed on the
 * queue by another thread that calls the {@link #handle} method,
 * which must be defined in a subclass to this. To stop the command
 * handler, one should call the {@link #requestStop} method.
 *
 * <p>
 * Created: Tue Jan 08 10:48:01 2002
 *
 * @author  Petter Måhlén
 * @version 1.0
 */

public abstract class CommandHandler extends Thread implements Protocol {
  /**
   * Milliseconds to wait for new commands before logging a message.
   */
  private static final int TIMEOUT = 10000;

  private   LinkedList m_commandQueue;
  private   boolean    m_stopRequested;
  private final Collection m_commandListeners;


  public CommandHandler() {
    m_commandQueue  = new LinkedList();
    m_stopRequested = false;
    m_commandListeners = new LinkedList();

    addCommandListener(new CmdListener());
  }

  /** Fire an event indicating that a command has been received. */
  public void fireCommandEvent(Integer procID,
			       Integer cmdID,
			       String cmdName,
			       List arguments)
    throws JDEException
  {
    synchronized(m_commandQueue) {
      CommandEvent event = new CommandEvent(procID,
					    cmdID,
					    cmdName,
					    arguments);
      if (m_commandQueue.contains(event)) {
        // two CommandEvents are considered equal if they have the same
        // command id's.
        throw new JDEException("Command " + cmdID + " already exists in queue for process " + getProcID() + ": " + m_commandQueue);
      }
      m_commandQueue.addLast(event);
      m_commandQueue.notifyAll();
    }
  }

  /** Add an CommandListener.  If the listener is already in the
   * list, nothing is done
   * @param listener The listener to add
   */
  public void addCommandListener(CommandListener listener) {
    if (m_commandListeners.contains(listener))
      return;
    m_commandListeners.add(listener);
  }

  /** Remove an CommandListener.  If the listener is already in the
   * list, nothing is done
   * @param listener The listener to remove
   */
  public void removeCommandListener(CommandListener listener) {
    m_commandListeners.remove(listener);
  }


  /**
   * Place the command on the internal command queue. This method
   * should be called by the handle() method of the sub-class, after
   * it has performed whatever consistency checks are necessary. The
   * method is synchronized on the private m_commandQueue member. It
   * will normally be executed by another thread than the one that runs
   * the actual command handler.<p>
   *
   * The above comments refer to an old scheme for processing
   * commands.  The current implementation does nothing and should be
   * removed.
   *
   * @param cmd a <code>DebugCommand</code> value
   * @exception JDEException if an error occurs
   */
  protected final void queue(DebugCommand cmd) throws JDEException {
  }

  /**
   * Removes the first command item in the queue, if there is one. If
   * the queue is empty, it will wait for something to be placed there.
   * This method should only be executed by the CommandHandler thread.
   *
   * @return a <code>DebugCommand</code> value
   */
  private final CommandEvent remove() {
    synchronized (m_commandQueue) {
      while(m_commandQueue.size() == 0 && !m_stopRequested) {
        JDE.debug(EVENTS, "proc: " + getProcID() + " waiting for new commands");
        try {
          m_commandQueue.wait(TIMEOUT);
        }
        catch (InterruptedException e) {}
      }

      if (m_stopRequested) {
        return null;
      }

      CommandEvent cmd = (CommandEvent) m_commandQueue.removeFirst();

      JDE.debug(EVENTS, "proc: " + getProcID() + " got command: " + cmd);

      return cmd;
    }
  }


  /**
   * Handle a debug command: normally means performing some kind of
   * consistency check and then adding it to the internal queue, using
   * the {@link CommandHandler#queue(DebugCommand)} method.
   *
   * @param cmd a <code>DebugCommand</code> value
   * @exception JDEException if an error occurs
   */
  public abstract void handle(DebugCommand cmd) throws JDEException;

  /**
   * Used by other threads to request the CommandHandler to stop executing.
   *
   */
  public void requestStop() {
    m_stopRequested = true;

    // just make sure that the thread wakes up so it can die properly and quickly
    synchronized (m_commandQueue) {
      m_commandQueue.notifyAll();
    }
  }

  public abstract Integer getProcID();

  /**
   * The main loop of the thread simply calls the {@link #remove}
   * method, and then the {@link DebugCommand#doCommand} method of
   * the received command.
   */
  public void run() {
    JDE.debug(EVENTS, "command handler: " + getProcID() + " starting up");

    while (!m_stopRequested) {
      CommandEvent event = remove();
      try {

        // event will be null if a request to stop the thread has been made.
        // If so, don't execute the command, and just let the loop die.
	if (null != event) {
	  for (Iterator iter = m_commandListeners.iterator();
	       iter.hasNext();
	       /* */) {
	    CommandListener listener = (CommandListener) iter.next();
	    listener.commandReceived(event);
	  } // for each CommandListener
	} // if received event
      } catch (JDEException e) {
        // signal the problem to JDE.
        JDE.commandResult(event.getCmdId(),
                          "Exception during command execution: " + e,
                          CMD_NOK, QUOTE);
      } catch (RuntimeException exc) {
        // signal the problem to JDE.
	JDE.signalException(exc);
      }
    }
    JDE.debug(EVENTS, "command handler: " + getProcID() + " terminated");
  }

  /** Default CommandListener to implement processing based on Command
   * class. */
  private class CmdListener implements CommandListener {
    public void commandReceived(CommandEvent event) throws JDEException {
      Integer cmdId = event.getCmdId();
      String cmdName = event.getCmdName();
      List arguments = event.getArgs();

      // Get the command object
      DebugCommand   cmd  =
	DebugCommandFactory.theFactory.createCommand(cmdId,
						     cmdName,
						     arguments);



      if (cmd == null) {
	throw new JDEException("command not implemented: " + cmdName);
      }

      // Perform consistency checks.  XXX Handle should be renamed
      // to something like checkCommand. troy
      JDE.debug(FRAMEWORK, "JDEbug command is " + cmd.getClass().getName());
      handle(cmd);

      // Actually do it
      cmd.doCommand();
      JDE.debug(EVENTS, "CommandHandler " + getProcID() + " did cmd");
    } // commandReceived
  } // inner class CmdListener
}// CommandHandler

/*
 * $Log: CommandHandler.java,v $
 * Revision 1.3  2004/12/24 16:05:16  troy
 * Add window to display threads and stacks
 *
 * Revision 1.2  2003/04/29 16:52:09  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.1  2003/01/15 06:01:55  paulk
 * Initial revision.
 *
 */

// End of CommandHandler.java
