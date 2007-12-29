package jde.debugger.command;
import java.util.List;

import jde.debugger.Debugger;
import jde.debugger.JDEException;


/**
 * DebugProcessCommand.java
 *
 *
 * Created: Fri Jan 28 21:58:06 2000
 *
 * 
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */

abstract public class DebugProcessCommand extends DebugCommand {
  protected Debugger m_debugger = null;

  public void init(Integer cmdID, 
                   String cmdName, List args) throws JDEException {
    super.init(cmdID, cmdName, args);
  }

    
  /**
   * Gets the value of debugger
   *
   * @return the value of debugger
   */
  public Debugger getDebugger() {
    return this.m_debugger;
  }

  /**
   * Sets the value of debugger
   *
   * @param debugger Value to assign to this.debugger
   */
  public void setDebugger(Debugger debugger){
    this.m_debugger = debugger;
  }
    
} // DebugProcessCommand


/*
 * $Log: DebugProcessCommand.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/07/06 02:05:51  paulk
 * Makefile
 *
 * Revision 1.1  2001/03/24 05:48:40  paulk
 * Initial version.
 *
 * Revision 1.2  2000/03/03 07:40:32  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.1  2000/01/31 12:46:10  paulk
 * Defines general behavior of application debug commands.
 *
 */

// End of DebugProcessCommand.java
