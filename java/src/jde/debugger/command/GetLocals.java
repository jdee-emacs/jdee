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
 * Gets the local variables in a specified stack frame.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_locals threadID stackFrameIndex
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getLocalVariableValueMapRep(Map) local-variables-values})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> Note that stackFrameIndex = 0 corresponds to the
 * current stackframe.
 * <li> The threadID and stackFrameIndex can be got from the
 *	'get_threads' command. Note that many evaluations
 *  might not be possible depending on the state of the thread
 * </ul>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Amit Kumar
 * @author Paul Kinnucan
 * @version $Revision: 1.5 $
 */
public class GetLocals extends DebugProcessCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    boolean weSuspendedThread = false;
    ThreadReference tRef = null;

    if (m_args.size() != 2)
      throw new JDEException("Insufficient arguments");

    try {

      Long uniqueID   = Etc.safeGetLong(m_args.remove(0), "thread ID");
      int  frameIndex = Etc.safeGetint(m_args.remove(0), "frame index");

      Object oRef = m_debugger.getStore().get(uniqueID);
      if (oRef == null) {
        throw new JDEException("No such thread exists");
      } else if (!(oRef instanceof ThreadReference)) {
        throw new JDEException("Object is not a thread");
      }

      tRef = (ThreadReference)oRef;

      if (!tRef.isSuspended()) {
        tRef.suspend();
        weSuspendedThread = true;
      }

      StackFrame frame = null;
      try {
        frame = tRef.frame(frameIndex);
      } catch (IncompatibleThreadStateException ex) {
        throw new JDEException("Thread is not suspended");
      } catch (IndexOutOfBoundsException ex) {
        throw new JDEException("Invalid frame");
      } catch (ObjectCollectedException ex) {
        throw new JDEException("The frame has already been garbage collected");
      }

      if (frame == null) {
        throw new JDEException("Error ascertaining frame");
      }

      String resultString = null;

      try {
        List visibleVariables    = frame.visibleVariables();
        Map  localVariableValues = frame.getValues(visibleVariables);

        // make sure that we keep track of objects, since the user may query
        // for more information about those.
        Iterator iter = localVariableValues.values().iterator();
        while (iter.hasNext()) {
          Value value = (Value) iter.next();

          if (value instanceof ObjectReference) {
            m_debugger.getStore().put((ObjectReference) value);
          }
        }

        // finally, produce the string to report back to emacs
        resultString = Rep.getLocalVariableValueMapRep(localVariableValues);
      } catch (AbsentInformationException ex) {
        throw new JDEException("Local variable information not available: compile with -g");
      } catch (NativeMethodException ex) {
        throw new JDEException("Can't access local variables in native methods");
      }

      // send the command result without adding quotes
      m_debugger.signalCommandResult(m_cmdID, resultString, CMD_OK, NOQUOTE);

    } finally {
      if (weSuspendedThread && (tRef != null)) tRef.resume();
    }

  }

  public Object clone() {return new GetLocals();}

} // GetLocals

/*
 * $Log: GetLocals.java,v $
 * Revision 1.5  2003/04/29 16:52:10  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.4  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.3  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/08/14 02:42:16  paulk
 * DebugCommandFactory.java
 *
 * Revision 1.1  2000/04/10 05:33:28  paulk
 * Initial revision.
 *
 *
 */

// End of GetLocals.java
