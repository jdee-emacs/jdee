package jde.debugger.command;
import java.util.Iterator;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadGroupReference;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;


/**
 * 'suspend' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * suspend [threadID]*
 *
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadIDs can be retrieved using the get_threads command
 * <li> if the list is omitted, the entire VM is suspended
 * <li> threadIDs can refer to either threads or threadgroups.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class Suspend extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    // see if there are arguments (should be the thread id). if so, we
    // suspend the thread ids passed. else, suspend the whole vm.
    if (m_args.size() > 0) {
      Iterator it = m_args.iterator();
      while (it.hasNext()) {
		
        Long uniqueID = Etc.safeGetLong(it.next(), "thread(group)");
		
        ObjectReference oRef = (ObjectReference) m_debugger.getStore().get(uniqueID);
        if (oRef == null) {
          throw new JDEException("Invalid ThreadID, or the thread/threadgroup is dead");
        } else if (oRef instanceof ThreadReference) {
          ((ThreadReference) oRef).suspend();
        } else if (oRef instanceof ThreadGroupReference) {
          ((ThreadGroupReference) oRef).suspend();
        } else {
          throw new JDEException("The object is not a thread or a threadgroup");
        }
      }
      m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
    } else {
      try {
        m_debugger.getVM().suspend();
        m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
      } catch (Exception ex) {
        throw new JDEException("Unable to suspend the application");
      }
    }
  }
    
  public Object clone() {return new Suspend();}
    
} // Suspend

/*
 * $Log: Suspend.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of Suspend.java
