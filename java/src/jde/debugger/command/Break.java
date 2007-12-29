package jde.debugger.command;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import jde.debugger.Etc;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.spec.EventRequestSpec;
import jde.debugger.spec.EventRequestSpecList;


/**
 * 'break' command.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * break {@link #doBreakInMethod in_method} class method [(args)] 
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
 *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 *     
 * break {@link #doBreakOnLine on_line}   class line
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
 *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 *
 * break {@link #doBreakAbsolute absolute}  file line
 *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
 *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmdID specID)
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> There are exactly three kinds of 'break' commands. One 
 *	of in_method, on_line, or absolute need to be used.
 * <li> 'class' can be a string pattern of the type *.Test
 * <li> specID is a 'long', that can be used in 'clear' commands.
 * </ul>
 *
 * <p>
 * 
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @see jde.debugger.EventHandler#breakpointEvent(BreakpointEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 *
 */
public class Break extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {
    try {
      // whatever function is called, should do a signalCommandResult
      // during the execution.
      String type = m_args.remove(0).toString().toLowerCase();
      if (type.equals("in_method")) {
        doBreakInMethod(m_args);
      } else if (type.equals("on_line")) {
        doBreakOnLine(m_args);
      } else if (type.equals("absolute")) {
        doBreakAbsolute(m_args);
      } else
        throw new JDEException("Syntax error: expecting one of 'in_method', 'on_line', or 'absolute'; '"+type+"' is not supported");
    } catch (UnsupportedOperationException ex) {
      throw new JDEException("Unspecified Error occured");
    } catch (IndexOutOfBoundsException ex) {
      throw new JDEException("Syntax error: argument missing");
    }
  }
    
    
  /**
   * A break in a particular method.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * break in_method class method [(arg1,arg2,...)] 
   *      [{@link Etc#getThreadFromArgs(List) thread-restriction}]
   *      [{@link Etc#getExprFromArgs(List) expression-restriction}]
   *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> There should be <b>no spaces</b> before or after the ','; when 
   *      the arguments are supplied.
   * <li> A void method should be indicated by <code>()</code>
   * <li> A unique method doesn't need to supply the arguments. The
   * <b>entire</b> argument list should be absent in this case.
   * </ul>
   */
  public void doBreakInMethod(List args)
    throws JDEException {

    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    String classPattern = args.remove(0).toString();
    String method = args.remove(0).toString();

    // the argument list
    List argumentList = null;

    // see if more arguments are present
    if (args.size() > 0) {

      String arg = args.remove(0).toString();

      // see if any arglist was provided at all
      if (arg.startsWith("(")) {
        // apparently it was. double check.
        if (!arg.endsWith(")")) {
          throw new JDEException("The argument list seems to be corrupt");
        }
        // trim the parens
        arg = arg.substring(1, arg.length() - 1);
        argumentList = new ArrayList();
        StringTokenizer t = new StringTokenizer(arg, ",");
        while (t.hasMoreTokens()) {
          argumentList.add(t.nextToken());
        }
      }
    }
    EventRequestSpecList eventRequests = m_debugger.getEventRequestSpecList();
    EventRequestSpec er = eventRequests.createMethodBreakpoint(classPattern, method, argumentList);
    er.setThread(Etc.getThreadFromArgs(args));
    er.setExpression(Etc.getExprFromArgs(args));
    er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
    eventRequests.install(er);
	
    m_debugger.signalCommandResult(m_cmdID, er.getID().toString(), CMD_OK, NOQUOTE);
  }

  /** A break on a particular line of a class */
  public void doBreakOnLine(List args)
    throws JDEException {

    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    String classPattern = args.remove(0).toString();
    int line = Etc.safeGetint(args.remove(0), "line number");

    EventRequestSpecList eventRequests = m_debugger.getEventRequestSpecList();
    EventRequestSpec er =
      eventRequests.createClassLineBreakpoint(classPattern, line);
    er.setThread(Etc.getThreadFromArgs(args));
    er.setExpression(Etc.getExprFromArgs(args));
    er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
    eventRequests.install(er);
	
    m_debugger.signalCommandResult(m_cmdID, er.getID().toString(), CMD_OK, NOQUOTE);
  }

  /** A break on a line of a given source file */
  public void doBreakAbsolute(List args)
    throws JDEException {

    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    String file = args.remove(0).toString();
    int line = Etc.safeGetint(args.remove(0), "line number");
	
    JDE.debug(EVENTS, "Doing an absolute break on file <" + file + ">, line: " + line);
	
    EventRequestSpecList eventRequests = m_debugger.getEventRequestSpecList();
    EventRequestSpec er =
      eventRequests.createSourceLineBreakpoint(file, line);
    er.setThread(Etc.getThreadFromArgs(args));
    er.setExpression(Etc.getExprFromArgs(args));
    er.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));
    eventRequests.install(er);
	    
    m_debugger.signalCommandResult(m_cmdID, er.getID().toString(), CMD_OK, NOQUOTE);
  }

  public Object clone() {return new Break();}
  
} // Break


/*
 * $Log: Break.java,v $
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:48:39  paulk
 * Initial version.
 *
 *
 */

// End of Break.java
