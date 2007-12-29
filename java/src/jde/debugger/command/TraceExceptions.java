package jde.debugger.command;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.spec.EventRequestSpec;
import jde.debugger.spec.EventRequestSpecList;



/**
 * 'trace_exceptions' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * trace_exceptions classPattern <u>type</u>
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
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
 * <li> <u>type</u> can be "caught", "uncaught", or "both"
 * <li> specID is a 'long', and can be used in the 'clear'
 * command
 * </ul>
 *
 * <p>
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 */
public class TraceExceptions extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    if (m_args.size() < 2) 
      throw new JDEException("Insufficient arguments");
	
    String classPattern = m_args.remove(0).toString();
    String type         = m_args.remove(0).toString().toLowerCase();
	
    boolean caught   = false;
    boolean uncaught = false;

    if (type.equals("both")) {
      caught   = true;
      uncaught = true;
    } else if (type.equals("caught")) {
      caught   = true;
    } else if (type.equals("uncaught")) {
      uncaught = true;
    } else {
      throw new JDEException("'"+type+"' not understood");
    }
	
    EventRequestSpecList eventRequests = m_debugger.getEventRequestSpecList();
    EventRequestSpec     er            = eventRequests.createExceptionIntercept(classPattern, caught, uncaught);

    er.setThread(Etc.getThreadFromArgs(m_args));
    er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(m_args));
    er.setClassFilters(Etc.getClassFiltersFromArgs(m_args));
    er.setClassExFilters(Etc.getClassExFiltersFromArgs(m_args));
    eventRequests.install(er);
	
    m_debugger.signalCommandResult(m_cmdID, er.getID().toString(), CMD_OK, NOQUOTE);
  }
    
  public Object clone() {return new TraceExceptions();}
    
} // TraceExceptions

/*
 * $Log: TraceExceptions.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 13:35:25  paulk
 * Initial revision.
 *
 *
 */

// End of TraceExceptions.java
