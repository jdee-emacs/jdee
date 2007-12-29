package jde.debugger.command;

import java.util.Iterator;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.ThreadGroupReference;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;


/**
 * 'interrupt' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * interrupt [threadID]+
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> threadID can be retrieved using the get_threads command
 * <li> at least one threadId should be specified
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class Interrupt extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 1) 
      throw new JDEException("Insufficient arguments");
	
    Iterator it = m_args.iterator();
    while (it.hasNext()) {
      Long uniqueID = Etc.safeGetLong(it.next(), "thread ID");
	    
      ObjectReference oRef = (ObjectReference) m_debugger.getStore().get(uniqueID);
      if (oRef == null) {
        throw new JDEException("Invalid ThreadID, or the thread is dead");
      } else if (oRef instanceof ThreadReference) {
        ((ThreadReference) oRef).interrupt();
      } else {
        throw new JDEException("The object is not a thread");
      }
    }
    m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }

  public Object clone() {return new Interrupt();}
  
} // Interrupt

/*
 * $Log: Interrupt.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of Interrupt.java
