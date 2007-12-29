package jde.debugger.command;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.NativeMethodException;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;



/**
 * A command that does nothing.  This is for commands that will affect
 * the GUI, but do not directly return anything to JDEE.  <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * null_command threadID [ optional_list ]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id)
 * </pre>
 *
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Troy Daniels
 * @version $Revision: 1.1 $
 */
public class NullCommand extends DebugProcessCommand {

  /**
   * Execute the command.
   * @exception jde.debugger.JDEException Thrown if an error occurs.
   */
  public void doCommand() throws JDEException {
    m_debugger.signalCommandResult(m_cmdID, "", CMD_OK, QUOTE);
  }

  public Object clone() {return new NullCommand();}

} // NullCommand

/*
 * $Log: NullCommand.java,v $
 * Revision 1.1  2003/04/29 16:52:10  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 *
 */

// End of NullCommand.java
