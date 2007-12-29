package jde.debugger.command;

import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.spec.EventRequestSpecList;
import jde.debugger.spec.WatchpointSpec;


/**
 * 'watch' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * watch classPattern fieldName <u>type</u>
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
 *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
 *      [{@link Etc#getObjectIDFromArgs(List) object-id-restriction}]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 *      [{@link Etc#getClassFiltersFromArgs(List) class-filters}]
 *      [{@link Etc#getClassExFiltersFromArgs(List) class-exclusion-filters}]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id specID)
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>type</u> can be "for_access" or "for_modification"
 * <li> 'classPattern' can be a string pattern of the type *.Test
 * <li> objectID is used when, for example, when you already know the
 *  object id of the object, the access/modification of which's field
 *  you're interested in. 
 * <li> specID is a 'long', that can be used in 'clear' commands.
 * </ul>
 *
 * <p>
 * @see jde.debugger.EventHandler#watchpointEvent(WatchpointEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 */
public class Watch extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
	
    if (m_args.size() < 3)
      throw new JDEException("Insufficient arguments");
	
    String classPattern = m_args.remove(0).toString();
    String methodName   = m_args.remove(0).toString();
    String typeString   = m_args.remove(0).toString().toLowerCase();

    EventRequestSpecList eventRequests = m_debugger.getEventRequestSpecList();
	
    WatchpointSpec er = null;
    if (typeString.equals("for_access")) {
      if (!m_debugger.getVM().canWatchFieldAccess()) 
        throw new JDEException("This VM implementation cannot watch field accesses");
      er = eventRequests.createAccessWatchpoint(classPattern, methodName);
    } else if (typeString.equals("for_modification")) {
      if (!m_debugger.getVM().canWatchFieldModification()) 
        throw new JDEException("This VM implementation cannot watch field modifications");
      er = eventRequests.createModificationWatchpoint(classPattern, methodName);
    } else {
      throw new JDEException("'"+typeString+"' not understood: use either 'for_access' or 'for_modification'");
    }
    er.setThread(Etc.getThreadFromArgs(m_args));
    er.setExpression(Etc.getExprFromArgs(m_args));
    er.setObjectID(Etc.getObjectIDFromArgs(m_args));
    er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
    er.setClassFilters(Etc.getClassFiltersFromArgs(m_args));
    er.setClassExFilters(Etc.getClassExFiltersFromArgs(m_args));
    eventRequests.install(er);
	
    m_debugger.signalCommandResult(m_cmdID, er.getID().toString(), CMD_OK, NOQUOTE);
  }
    
  public Object clone() {return new Watch();}
    
} // Watch

/*
 * $Log: Watch.java,v $
 * Revision 1.2  2003/01/15 05:56:25  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:26  paulk
 * Initial revision.
 *
 *
 */

// End of Watch.java
