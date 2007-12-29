package jde.debugger.command;
import java.util.List;
import jde.debugger.JDEException;
import jde.debugger.Protocol;


/**
 * Class of debugger commands.
 *
 * Command-line syntax:
 *
 * app_id cmd_id cmd_name [arg]*
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */

abstract public class DebugCommand implements Protocol, Cloneable {

  Integer m_cmdID;
  String  m_cmdName;
  List    m_args;

  public DebugCommand() { }
    
  public void init(Integer cmdID, String cmdName, List args) throws JDEException {
    m_cmdID   = cmdID;
    m_cmdName = cmdName;
    m_args    = args;
  }

  public Integer getID() {
    return m_cmdID;
  }
    
  public String toString() {
    return m_cmdID.toString() + " " + m_cmdName;
  }

  public boolean equals(Object o) {
    if (!(o instanceof DebugCommand)) {
      return false;
    }
	
    return m_cmdID.equals(((DebugCommand) o).getID());
  }
    
  abstract protected void doCommand() throws JDEException;
  abstract public Object clone();  
} // DebugCommand


/*
 * $Log: DebugCommand.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/01/30 12:31:51  paulk
 * Initial revision.
 *
 */

// End of DebugCommand.java
