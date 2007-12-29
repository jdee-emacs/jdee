package jde.debugger.command;

import java.util.Iterator;

import com.sun.jdi.PathSearchingVirtualMachine;
import jde.debugger.JDEException;




/**
 * 'get_path_information' command. Returns all the vm knows about
 * paths.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_path_information
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id "base-directory" (list [boot-class-path component]*) (list [class-path component]*))
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class GetPathInfo extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    if (!(m_debugger.getVM() instanceof PathSearchingVirtualMachine))
      throw new JDEException("VM doesn't search paths");
	
    PathSearchingVirtualMachine vm =
      (PathSearchingVirtualMachine) m_debugger.getVM();
	
    StringBuffer bootClassPathString = new StringBuffer("(list");
    Iterator it = vm.bootClassPath().iterator();
    while (it.hasNext()) {
      bootClassPathString.append(" \"");
      bootClassPathString.append(it.next());
      bootClassPathString.append("\"");
    }
    bootClassPathString.append(")");
	
    String bcpRes = bootClassPathString.toString().replace('\\', '/');
	
    StringBuffer classPathString = new StringBuffer("(list");
    it = vm.classPath().iterator();
    while (it.hasNext()) {
      classPathString.append(" \"");
      classPathString.append(it.next());
      classPathString.append("\"");
    }
    classPathString.append(")");
	
    String cpRes = classPathString.toString().replace('\\', '/');
	
    m_debugger.signalCommandResult(m_cmdID,
                                   "\""+vm.baseDirectory().replace('\\', '/')+"\""
                                   + BR +bootClassPathString
                                   + BR +classPathString, CMD_OK, NOQUOTE);
  }
    
  public Object clone() {return new GetPathInfo();}
} // GetPathInfo

/*
 * $Log: GetPathInfo.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetPathInfo.java
