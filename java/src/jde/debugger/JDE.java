package jde.debugger;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.text.DateFormat;
import java.util.Date;

/**
 * JDE.java
 *
 *
 * Created: Thu Feb 15 12:58:59 2001
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.8 $
 */

public class JDE implements Protocol {

  /**
   * The single instance of this class.
   *
   */
  private static JDE s_jde = new JDE(System.out);

  /**
   * Writes command responses, messages, and event notifications to
   * JDE.
   */
  private PrintWriter m_out;

  /** logical OR of the debug flags that should be used in this particular session */
  private int m_debugFlags
    = APP_IO
//     | JDE_PIPE
//     | EVENTS
    | EXCEPTION
//     | FRAMEWORK
//     | COMMANDS
    | GUI
    ;

  private JDE(OutputStream out) {
    m_out = new PrintWriter(out, true);

  }


  /*
   * Private methods ----------------------------------------------------
   */


  private synchronized void p_transmit(String type, String message) {
    m_out.println("(" + JDE_BUG + type + " " + message + ")");
  }

  private void p_setDebugFlags(int flags) {
    m_debugFlags = flags;
  }

  private void p_debug(int flag, String message) {
    if ((m_debugFlags & flag) != 0) {
      p_transmit(DEBUG, " \"" +
                 DateFormat.getTimeInstance(DateFormat.MEDIUM).format(new Date()) + ": " +
                 message + "\"");
    }
  }

  /*
   * Public, static interface  --------------------------------------------------
   */

  /**
   * To indicate which of the debugging flags should be set. If a
   * flag is set, it means that calls to <code>JDE.debug()</code>
   * with that flag will actually be printed.
   *
   * @param flags an <code>int</code> value with a bitwise OR of the
   * types of information that are desired.
   * @see Protocol#NONE
   */
  public static void setDebugFlags(int flags) {
    s_jde.p_setDebugFlags(flags);
  }

  /**
   * Send a debugging message to Emacs, for display in the
   * <code>*JDEbug*</code> buffer. The message will only be printed
   * if the flag that is passed in the first argument is set. The
   * current time is prepended to the message string.
   *
   * @param flag an <code>int</code> value
   * @param message a <code>String</code> value
   * @see Protocol
   * @see #setDebugFlags
   */
  public static void debug(int flag, String message) {
    s_jde.p_debug(flag, message);
  }

  /**
   * Sends the result of executing a debugger command to
   * Emacs. Depending on wether the command was successful or not,
   * different lisp code will be generated. If the
   * <code>quote</code> argument is set ({@link Protocol#QUOTE}),
   * double quotes will be added around the message string. The idea
   * is that if the command result should be a Lisp string, you
   * should set the <code>quote</code> argument to get the double
   * quotes, and otherwise not.
   *
   * @param cmdID   the ID of the command
   * @param message a <code>String</code> value
   * @param success indicates what to generate, {@link
   * Protocol#COMMAND_RESULT} or {@link Protocol#COMMAND_ERROR}
   * @param quote a <code>boolean</code> value
   */
  public static void commandResult(Integer cmdID, String message, boolean success, boolean quote) {
    String type = success ? COMMAND_RESULT : COMMAND_ERROR;

    StringBuffer messageBuffer = new StringBuffer(cmdID.toString());

    if (message != null && message.length() > 0) {
      messageBuffer.append(" ");
      if (quote) {
        messageBuffer.append("\"");
      }
      messageBuffer.append(message);
      if (quote) {
        messageBuffer.append("\"");
      }
    }

    s_jde.p_transmit(type, messageBuffer.toString());
  }


  /**
   * Equivalent to calling {@link #commandResult(Integer,
   * String, boolean, boolean)} with the <code>quote</code> argument
   * set to {@link Protocol#NOQUOTE}.
   *
   * @param cmdID an <code>Integer</code> value
   * @param message a <code>String</code> value
   * @param success a <code>boolean</code> value
   */
  public static void commandResult(Integer cmdID, String message, boolean success) {
    commandResult(cmdID, message, success, NOQUOTE);
  }

  /**
   * A method for sending more arbitrary information to Emacs. If
   * the <code>quote</code> argument is set, double quotes will
   * added around the message string.
   *
   * @param procID an <code>Integer</code> value
   * @param type a <code>String</code> value indicating what Lisp
   * command should be sent
   * @param message a <code>String</code> value
   * @param quote a <code>boolean</code> value
   */
  public static void signal(Integer procID, String type, String message, boolean quote) {
    StringBuffer messageBuffer = new StringBuffer(procID.toString());

    if (message != null && message.length() > 0) {
      messageBuffer.append(" ");
      if (quote) {
        messageBuffer.append("\"");
      }
      messageBuffer.append(message);
      if (quote) {
        messageBuffer.append("\"");
      }
    }

    s_jde.p_transmit(type, messageBuffer.toString());
  }

  /** A method for signaling JDEE that an exception occured.  The
      message displayed in Emacs will include the stack trace. */
  public static void signalException(Throwable exc) {
    s_jde.p_signalException(exc);
  }

  private void p_signalException(Throwable exc) {
    if ((m_debugFlags & EXCEPTION) != 0) {
      Writer writer = new StringWriter();
      exc.printStackTrace(new PrintWriter(writer));
      signal(new Integer(-1), ERROR,
	     "Exception during command execution: " +
	     writer.toString(),
	     QUOTE);
    }
  }


  /**
   * Equivalent to calling {@link #signal(Integer,
   * String, String, boolean)} with the <code>quote</code> argument
   * set to false.
   *
   * @param procID an <code>Integer</code> value
   * @param type a <code>String</code> value
   * @param message a <code>String</code> value
   */
  public static void signal(Integer procID, String type, String message) {
    signal(procID, type, message, NOQUOTE);
  }
}// JDE

/*
 * $Log: JDE.java,v $
 * Revision 1.8  2004/12/24 16:05:13  troy
 * Add window to display threads and stacks
 *
 * Revision 1.7  2003/09/21 05:23:11  paulk
 * Fix bug in setDebugFlags (it was calling itself).
 *
 * Revision 1.6  2003/04/29 16:51:56  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.5  2003/01/15 05:50:51  paulk
 * Remove CRs.
 *
 * Revision 1.4  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of JDE.java
