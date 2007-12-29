package jde.debugger.command;

import jde.debugger.JDEException;
import com.sun.jdi.ThreadGroupReference;
import jde.debugger.Rep;
import java.util.Iterator;
import java.util.List;


/**
 * List all threads. 'get_threads' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_threads
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link #doCommand thread-info})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> There are a couple of quirks regarding the reporting
 * of thread state:
 * <ul>
 * <li> Quirk 1: Due to a bug in ThreadReference.isAtBreakpoint(),
 *        a thread will report a breakpoint at a location
 *	      even if a threadFilter has been applied for the
 *	      thread. ie, if test.Test.java:41 is your
 *	      breakpoint, and you've applied a threadfilter
 *	      saying you DON'T want an event if the thread ==
 *	      Thread-A, and you somehow suspend Thread-A at
 *	      exactly that line, and do a 'get_threads';
 *	      Thread-A will report to be suspended on a
 *	      breakpoint, although ordinarily it would have
 *	      skipped it.
 *
 * <li> Quirk 2: There seems to be no way of reporting the
 *        status if the user does a
 *	      Thread.suspend(). Well, it's deprecated
 *	      anyways... *shrug*.
 * </ul>
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class GetThreads extends DebugProcessCommand {
  
  /**
   * Returns a representation of all the threads and threadgroups
   * in the VM. For example:
   * <pre>
   *              ThreadGroup-1
   *                  +- ThreadGroup-2
   *                  |        +- ThreadGroup-3
   *                  |        |        \- Thread-1
   *                  |        +- ThreadGroup-4
   *                  |        |        +- Thread-2
   *                  |        |        \- Thread-3
   *                  |        \- Thread-4
   *                  \- Thread-5
   *              ThreadGroup-5
   *                  +- Thread-6
   *
   *
   *          (list
   *            (list "ThreadGroup" <tgID> "ThreadGroup-1"
   *              (list 
   *                (list "Thread" <tID> "Thread-5" ...))
   *              (list 
   *                (list "ThreadGroup" <tgID> "ThreadGroup-2"
   *                  (list 
   *                    (list "Thread" <tID> "Thread-4"))
   *                  (list 
   *                    (list "ThreadGroup" <tgID> "ThreadGroup-3"
   *                      (list)
   *                      (list 
   *                        (list "Thread" <tID> "Thread-1" ...)))
   *                    (list "ThreadGroup" <tgID> "ThreadGroup-4"
   *                      (list)
   *                        (list
   *                          (list "Thread" <tID> "Thread-2" ...)
   *                          (list "Thread" <tID> "Thread-3" ...)))))))
   *          (list "ThreadGroup" <tgID> "ThreadGroup-5"
   *            (list)
   *              (list
   *                (list "Thread" <tID> "Thread-6" ...))))
   * </pre>
   * <b>Syntax:</b>
   * <pre>
   * (list [{@link Rep#getThreadGroupRep top-level thread group}]*)
   * </pre>
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
	
    List         l      = m_debugger.getVM().topLevelThreadGroups();
    Iterator     it     = l.iterator();
    StringBuffer result = new StringBuffer("(list ");
	
    while (it.hasNext()) {
      // XXX may have to populate the ObjectStore here. 
	    
      result.append(BR);
      result.append(Rep.getThreadGroupRep((ThreadGroupReference)it.next()));
    }
	
    result.append(")");
	
    m_debugger.signalCommandResult(m_cmdID, result.toString(), CMD_OK, NOQUOTE);
  }



  public Object clone() {return new GetThreads();}
  
} // GetThreads

/*
 * $Log: GetThreads.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of GetThreads.java
