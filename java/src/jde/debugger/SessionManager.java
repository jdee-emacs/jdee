package jde.debugger;

import java.util.HashMap;
import java.util.Iterator;

import jde.debugger.command.CommandHandler;
import jde.debugger.command.SessionCommandHandler;


/**
 * The session manager keeps track of which debugging sessions are
 * currently active and handled by which debuggers. There is also a
 * special debug command handler thread which is handled by the
 * SessionManager ({@link #m_handler}). It's a singleton object that
 * exposes only a set of static interface functions.
 *
 * <p>
 * Created: Tue Jan 08 13:19:51 2002
 *
 * @author Petter Måhlén
 * @version $Revision: 1.3 $
 */

public class SessionManager implements Protocol {
  /**
   * The mappings of process ID/debugger that are handled by JDEbug.
   * Not synchronized, because it is only accessed from methods defined
   * in this file, and these methods are (and must remain) synchronized.
   */
  private HashMap               m_debuggers;
  private SessionCommandHandler m_handler;

  private static SessionManager s_theManager   = new SessionManager();
  private static long           s_nextObjectID = 0;

  /**
   * Creates a new <code>SessionManager</code> instance and starts
   * up the {@link #m_handler} session command handler thread.
   *
   */
  private SessionManager() {
    m_debuggers = new HashMap();
    m_handler   = new SessionCommandHandler();
    m_handler.start();
  }

  private synchronized CommandHandler p_getCommandHandler(Integer procID)
    throws JDEException {

    // special hack for the session command handler
    if (procID.equals(JDEbug.debuggerID)) {
      return m_handler;
    }

    Debugger debugger = (Debugger) m_debuggers.get(procID);

    if (debugger == null) {
      throw new JDEException("No process with id " + procID + " found");
    }

    return debugger.getCommandHandler();
  }

  private synchronized Debugger p_getDebugger(Integer procID)
    throws JDEException {

    Debugger debugger = (Debugger) m_debuggers.get(procID);

    if (debugger == null) {
      throw new JDEException("No debugger for process id " + procID + " found");
    }

    return debugger;
  }

  private synchronized void p_registerDebugger(Debugger debugger) throws JDEException {
    Integer procID = debugger.getProcID();

    if (m_debuggers.containsKey(procID)) {
      throw new JDEException("registerDebugger: A process with id " + procID + " already exists!");
    }

    m_debuggers.put(procID, debugger);
    JDE.debug(EVENTS, "registered debugger with procid: " + procID);
  }


  private synchronized void p_deregisterDebugger(Debugger debugger) throws JDEException {
    Integer procID = debugger.getProcID();

    if (null != m_debuggers) {
      if (!m_debuggers.containsKey(procID)) {
	// XXX - sort of doubtful whether I should really throw an exception here,
	// but it's generally best to be strict with incorrect usage.
	throw new JDEException("deregisterDebugger: No process with id " + procID + " exists!");
      }

      if (debugger.isValid()) {
	throw new JDEException("INTERNAL ERROR: an attempt was made at deregistering a valid debugger. The debugger must be shut down first.");
      }

      m_debuggers.remove(procID);
    }
    JDE.debug(EVENTS, "removed debugger with procid: " + procID);
  }


  private synchronized void p_shutdown() {
    if (m_handler != null) {
      m_handler.requestStop();
    }

    if (m_debuggers != null) {
      Iterator iter = m_debuggers.values().iterator();

      while (iter.hasNext()) {
        Debugger dbgr = (Debugger) iter.next();

        try {
          dbgr.shutdown();
        }
        catch (JDEException e) {
          JDE.signal(dbgr.getProcID(), Protocol.ERROR,
                     "SessionManager.p_shutdown() caught exception when shutting down debugger: " + e,
                     QUOTE);
        }
      }
    }

    // Invalidate the object
    if (null != m_debuggers)
      m_debuggers.clear();
    m_debuggers = null;
    m_handler   = null;
  }

  /*
   * PUBLIC, STATIC INTERFACE -----------------------------------------------------
   */


  /**
   * Returns the command handler for a given process ID. Note that
   * it doesn't return the Debugger object, but its command handler.
   *
   * @param procID an <code>Integer</code> value
   * @return a <code>CommandHandler</code> value
   * @exception JDEException if there is no registered debugger for
   * the given process ID
   * @see jde.debugger.command.ProcessCommandHandler
   * @see Debugger
   */
  public static CommandHandler getCommandHandler(Integer procID) throws JDEException {
    return s_theManager.p_getCommandHandler(procID);
  }

  /**
   * Returns the Debugger object for a given process ID.
   *
   * @param procID an <code>Integer</code> value
   * @return a <code>Debugger</code> value
   * @exception JDEException if there is no registered debugger for
   * the given process ID
   * @see Debugger
   */
  public static Debugger getDebugger(Integer procID) throws JDEException {
    return s_theManager.p_getDebugger(procID);
  }

  /**
   * Registers the given Debugger as active. When this is done, it
   * is possible to retrieve the Debugger and its CommandHandler
   * through the {@link #getDebugger} and {@link #getCommandHandler}
   * methods.
   *
   * @param debugger a <code>Debugger</code> value
   * @exception JDEException if there is already a registered
   * debugger with the same process ID.
   */
  public static void registerDebugger(Debugger debugger) throws JDEException {
    s_theManager.p_registerDebugger(debugger);
  }

  /**
   * Deregisters the given debugger.
   *
   * @param debugger a <code>Debugger</code> value
   * @exception JDEException if the debugger hasn't been registered
   * previously, or if the debugger is still valid, as indicated by
   * the {@link Debugger#isValid} method.
   */
  public static void deregisterDebugger(Debugger debugger) throws JDEException {
    s_theManager.p_deregisterDebugger(debugger);
  }

  /**
   * Shuts down the SessionManager, by first shutting down each
   * registered Debugger, and then shutting down the session command
   * handler. After the shutdown, this object is no longer possible
   * to use.
   */
  public static void shutdown() {
    s_theManager.p_shutdown();
  }

  /**
   * Generates a unique number with each call (unique for each time
   * that this class is loaded, not in any wider sense).
   *
   * @return a <code>Long</code> value
   */
  public static synchronized Long generateObjectID() {
    return new Long(s_nextObjectID++);
  }
}// SessionManager

/*
 * $Log: SessionManager.java,v $
 * Revision 1.3  2003/04/29 16:51:57  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.2  2003/01/15 05:50:51  paulk
 * Remove CRs.
 *
 * Revision 1.1  2003/01/08 07:16:45  paulk
 * Initial revision.
 *
 */

