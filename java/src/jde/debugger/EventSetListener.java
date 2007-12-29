package jde.debugger;

/** This interface is implemented by objects interested in EventSets
 * from the debugger
 * Created:  3/18/03 6:02pm.
 *
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @see EventSetEvent
 * @version $Revision: 1.1 $
*/
public interface EventSetListener
{
    /** Called whenever an JDE EventSet is received.  Intended for
     * things like conditional breakpoints, where we may want to
     * suspend the debugger.
     * @param evt An event containing the EventSet and other
     * information
     */
    public void eventSetReceived(EventSetEvent evt);

    /** Called whenever an JDE EventSet is received and we suspend the
     * debugger.  Intended for things like displays, where we want to
     * update whenever the debugger is suspended.
     * @param evt An event containing the EventSet and other
     * information
     */
    public void debuggerSuspended(EventSetEvent evt);

    /** Called whenever an JDE EventSet is received and we are about
     * to resume the debugger.
     * @param evt An event containing the EventSet and other
     * information
     */
    public void debuggerResumed(EventSetEvent evt);

}
