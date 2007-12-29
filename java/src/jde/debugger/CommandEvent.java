package jde.debugger;

import java.util.ArrayList;
import java.util.EventObject;
import java.util.List;



/** This class is the event sent to interested clients when JDEE
 * sends a command.
 * Created: 3/18/03 6:02pm.
 *
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @see CommandListener
 * @version $Revision: 1.1 $
*/
public class CommandEvent extends EventObject
{
  /**
   * Constructor.
   * @param procId The process ID
   * @param cmdId The command ID
   * @param cmdName The command name
   * @param args The arguments.  This list is copied, so changes to
   * argument do not affect this object.
   */
  public CommandEvent(Integer procId,
		      Integer cmdId,
		      String cmdName,
		      List args) {
    super(cmdName);
    m_procId = procId;
    m_cmdId = cmdId;
    m_cmdName = cmdName;
    m_args = new ArrayList(args);
  }


  /**
   * Gets the value of the process ID
   *
   * @return the value of procId
   */
  public Integer getProcId()  {
    return this.m_procId;
  }

  /**
   * Gets the value of command ID
   *
   * @return the value of cmdId
   */
  public Integer getCmdId()  {
    return this.m_cmdId;
  }

  /**
   * Gets the value of command name
   *
   * @return the value of cmdName
   */
  public String getCmdName()  {
    return this.m_cmdName;
  }

  /**
   * Gets the command arguments
   *
   * @return the value of args
   */
  public List getArgs()  {
    return this.m_args;
  }

  /** Tests two command events for equality.  Two CommandEvents are
   * considered equal if they have the same command id's.
   * @see #hashCode */
  public boolean equals(Object other) {
    if (!(other instanceof CommandEvent))
      return false;

    CommandEvent event = (CommandEvent) other;
    return getProcId().equals(event.getProcId());
  }

  /** Hash code that is consistent with equals.
   * @see #equals
   */
  public int hashCode() {
    return getProcId().hashCode();
  }

  /** String representation for debugging. */
  public String toString() {
    return getClass().getName() + "[" +
      "m_procId=" + m_procId + "," +
      "m_cmdId=" + m_cmdId + "," +
      "m_cmdName=" + m_cmdName + "," +
      "m_args=" + m_args + "]";
  }
  private final Integer m_procId;
  private final Integer m_cmdId;
  private final String m_cmdName;
  private final List m_args;
}
