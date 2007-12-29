package jde.debugger.command;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;


/**
 * 'get_thread' command. List a thread in more detail.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_thread threadID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id 
 *  {@link Rep#getThreadRep(ThreadReference, boolean)  detailed-thread-info})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> The thread can be waiting for a monitor through entry
 * into a synchronized method, the synchronized
 * statement, or Object.wait(). The status() method can be used to
 * differentiate between the first two cases and the third. 
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class GetThread extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");
	
    Long            uniqueID = Etc.safeGetLong(m_args.remove(0), "thread ID");
    ObjectReference tRef     = m_debugger.getStore().get(uniqueID);
	
    if (tRef == null) {
      throw new JDEException("No such thread exists");
    } else if (!(tRef instanceof ThreadReference)) {
      throw new JDEException("No such thread exists (anymore?)");
    }
	
    m_debugger.signalCommandResult(m_cmdID, 
                                   Rep.getThreadRep((ThreadReference)tRef, true),
                                   CMD_OK, NOQUOTE);
  }
    
    
    
  public Object clone() {return new GetThread();}
    
} // GetThread

/*
 * $Log: GetThread.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetThread.java
