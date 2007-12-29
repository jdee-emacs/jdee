/*
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import java.util.Iterator;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadGroupReference;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;


/**
 * 'resume' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * resume [threadID]*
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadIDs can be retrieved using the get_threads command
 * <li> if the list is omitted, the entire VM is resumed
 * <li> threadIDs can refer to either threads or thereadgroups.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class Resume extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    // see if there are arguments (should be the thread id). if so, we
    // resume the thread ids passed. else, resume the whole vm.
    if (m_args.size() > 0) {
      Iterator it = m_args.iterator();
      while (it.hasNext()) {
        Long uniqueID = Etc.safeGetLong(it.next(), "thread(group)");
		
        ObjectReference oRef = (ObjectReference) m_debugger.getStore().get(uniqueID);
        if (oRef == null) {
          throw new JDEException("Invalid ThreadID, or the thread/threadgroup is dead");
        } else if (oRef instanceof ThreadReference) {
          ((ThreadReference)oRef).resume();
        } else if (oRef instanceof ThreadGroupReference) {
          ((ThreadGroupReference)oRef).resume();
        } else {
          throw new JDEException("The object is not a thread or a threadgroup");
        }
      }
      m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
    } else {
      try {
        m_debugger.getVM().resume();
        m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
      } catch (Exception ex) {
        throw new JDEException("Unable to resume the application");
      }
    }
  }
    
  public Object clone() {return new Resume();}
    
} // Resume

/*
 * $Log: Resume.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of Resume.java
