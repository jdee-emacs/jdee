package jde.debugger.command;
import jde.debugger.JDEException;
import jde.debugger.Etc;


/**
 * 'cancel_trace_threads' command.
 * <p>
 *
 * <b>Syntax: </b>
 * <pre>
 * cancel_trace_threads <u>requestID</u>
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * <b>Comments:</b>
 * <ul>
 * <li> <u>requestID</u> is returned in the trace threads reply
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class CancelTraceThreads extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");

    m_debugger.deleteIdentifiableRequest(Etc.safeGetLong(m_args.remove(0), "request ID"));

    m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }



  public Object clone() {return new CancelTraceThreads();}
  
} // CancelTraceThreads

/*
 * $Log: CancelTraceThreads.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:48:40  paulk
 * Initial version.
 *
 *
 */

// End of CancelTraceThreads.java
