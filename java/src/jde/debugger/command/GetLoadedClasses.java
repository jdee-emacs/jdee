package jde.debugger.command;

import java.util.Iterator;
import com.sun.jdi.ReferenceType;
import jde.debugger.JDEException;


/**
 * 'get_loaded_classes' command. Returns a list of all loaded classes
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_loaded_classes
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id (list ["type-name"]*))
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class GetLoadedClasses extends DebugProcessCommand {
  
    /**
     *
     * @exception jde.debugger.JDEException <description>
     */
    public void doCommand() throws JDEException {
	StringBuffer typeNames = new StringBuffer("(list");
	
	Iterator it = m_debugger.getVM().allClasses().iterator();
	while (it.hasNext()) {
	    typeNames.append(" \"");
	    typeNames.append(((ReferenceType)it.next()).name());
	    typeNames.append("\"");
	}
	typeNames.append(")");

	m_debugger.signalCommandResult(m_cmdID, typeNames.toString(), CMD_OK, NOQUOTE);
    }
    
    public Object clone() {return new GetLoadedClasses();}
  
} // GetLoadedClasses

/*
 * $Log: GetLoadedClasses.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:13  paulk
 * Initial version.
 *
 *
 */

// End of GetLoadedClasses.java
