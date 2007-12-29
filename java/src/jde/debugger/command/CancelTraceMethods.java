package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.Etc;


/**
 * 'cancel_trace_methods' command.
 * <p>
 *
 * <b>Syntax: </b>
 * <pre>
 * cancel_trace_methods <u>requestID</u>
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>requestID</u> is returned in the trace methods reply
 * </ul>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class CancelTraceMethods extends DebugProcessCommand {
  
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
    
  public Object clone() {return new CancelTraceMethods();}
    
} // CancelTraceMethods

/*
 * $Log: CancelTraceMethods.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:48:39  paulk
 * Initial version.
 *
 *
 */

// End of CancelTraceMethods.java
