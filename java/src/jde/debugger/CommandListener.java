package jde.debugger;




/** This interface is implemented by objects interested in commands
 * from JDEE.
 * Created:  3/18/03 6:02pm.
 *
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @see CommandEvent
 * @version $Revision: 1.1 $
*/
public interface CommandListener
{
    /** Called whenever an JDE command is received.  Intended for
     * things like stack up commands, that might affect several modules debugger.
     * @param evt An event containing the command and other
     * information
     * @throws JDEException if an error occurs.
     */
    public void commandReceived(CommandEvent evt) throws JDEException;


}
