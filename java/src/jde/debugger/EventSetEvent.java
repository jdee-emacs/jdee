package jde.debugger;

import java.util.EventObject;

import com.sun.jdi.ThreadReference;
import com.sun.jdi.event.EventSet;



/** This class is the event sent to interested clients when the
 * debugger returns an EventSet.
 * Created: 3/18/03 6:02pm.
 *
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @see EventSetListener
 * @version $Revision: 1.1 $
*/
public class EventSetEvent extends EventObject
{
  /** Constructor */

  public EventSetEvent(EventSet eventSet,
		       ThreadReference thread,
		       boolean suspend) {
    super(eventSet);
    m_threadRef = thread;
    m_suspended = suspend;
  }

  /** Get the event set associated with this event.<p>
   *
   * For now, this returns the same thing as getSource.  In the
   * future, there may be something else we want to put there, and
   * this does avoid excess casting.

   * @return The event set*/
  public EventSet getEventSet() {
    return (EventSet) getSource();
  }

  /** Tests if the debugger has been suspended */
  public boolean isSuspended() {
    return m_suspended;
  }

  /** Indicates whether we suspended the debugger, and want it to stay so. */
  public void setSuspended(boolean suspend) {
    m_suspended = suspend;
  }

  /** Get the thread associated with this event, or null if there is none */
  public ThreadReference getThreadReference() {
    return m_threadRef;
  }

  public String toString() {
    return getClass().getName() + "[" +
      "eventSet=" + getEventSet() + "," +
      "suspended=" + m_suspended + "," +
      "threadRef=" + m_threadRef + "]";
  }

  private boolean m_suspended;
  private final ThreadReference m_threadRef;
}
