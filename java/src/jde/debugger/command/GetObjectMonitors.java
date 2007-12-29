package jde.debugger.command;
import com.sun.jdi.ObjectReference;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;


/**
 * 'get_object_monitors' command. Information about the monitors
 * corresponding to a particular object.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_object_monitors objectID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id 
 *       {@link Rep#getObjectMonitorsRep(ObjectReference) object-monitors-info})
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class GetObjectMonitors extends DebugProcessCommand {
    
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() != 1)
      throw new JDEException("Insufficient arguments");
	
    Long            uniqueID = Etc.safeGetLong(m_args.remove(0), "object ID");
    ObjectReference oRef     = m_debugger.getStore().get(uniqueID);
    if (oRef == null) 
      throw new JDEException("No such object exists");
	
    m_debugger.signalCommandResult(m_cmdID, Rep.getObjectMonitorsRep(oRef), CMD_OK, NOQUOTE);
  }
    
    
    
  public Object clone() {return new GetObjectMonitors();}
  
} // GetObjectMonitors

/*
 * $Log: GetObjectMonitors.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:14  paulk
 * Initial version.
 *
 *
 */

// End of GetObjectMonitors.java
