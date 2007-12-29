package jde.debugger.command;

import jde.debugger.JDEException;
import jde.debugger.Etc;


/**
 * 'clear' command. Clears a breakpoint, watchpoint or an exception
 * intercept
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * clear specID
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * <b>Comments:</b>
 * <ul>
 * <li> specIDs are returned in the 'break'/'watch'/'trace_exceptions'
 * commands.
 * </ul>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class Clear extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");
	
    Long specID = Etc.safeGetLong(m_args.remove(0), "spec ID");
    m_debugger.getEventRequestSpecList().removeSpec(specID);
    m_debugger.signalCommandResult(m_cmdID, null, CMD_OK);
  }
    
  public Object clone() {return new Clear();}
  
} // Clear

/*
 * $Log: Clear.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:48:40  paulk
 * Initial version.
 *
 *
 */

// End of Clear.java
