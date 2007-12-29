package jde.debugger.command;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.StringReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;



/**
 * 'get_string' command. Returns the value of a string
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_string objectID 
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getStringRep(StringReference) string-representation})
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */
public class GetString extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
 
    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");

    Long            uniqueID = Etc.safeGetLong(m_args.remove(0), "object ID");
    ObjectReference oRef     = m_debugger.getStore().get(uniqueID);
	    
    if (oRef == null) {
      throw new JDEException("No such object exists");
    } else if (!(oRef instanceof StringReference)) {
      throw new JDEException("Object is not a string");
    }
	
    m_debugger.signalCommandResult(m_cmdID, Rep.getStringRep((StringReference)oRef), CMD_OK, NOQUOTE);
  }
    
  public Object clone() {return new GetString();}
  
} // Run

/*
 * $Log: GetString.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/03/03 07:10:29  paulk
 * Initial revision.
 *
 */

// End of GetString.java
