package jde.debugger.command;
import java.util.Iterator;
import java.util.Map;

import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.InvalidStackFrameException;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;



/**
 * Gets the "this" object of a specified stack frame.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_this threadID stackFrameIndex
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 *  (jde-dbo-command-result cmd_id {@link Rep#getObjectRep(ObjectReference) detailed-object-info})
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> Note that stackFrameIndex = 0 corresponds to the
 * current stackframe.
 * <li> The threadID and stackFrameIndex can be got from the
 *	'get_threads' command.
 * </ul>
 *
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.4 $
 * @copyright Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 */
public class GetThis extends DebugProcessCommand {
  
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

      Long   uniqueID   = Etc.safeGetLong(m_args.remove(0), "thread ID");
      int    frameIndex = Etc.safeGetint(m_args.remove(0), "frame index");
      Object oRef       = m_debugger.getStore().get(uniqueID);
	    
      if (oRef == null) {
        throw new JDEException("No such thread exists");
      } else if (!(oRef instanceof ThreadReference)) {
        throw new JDEException("Object is not a thread");
      }
	    
      tRef = (ThreadReference) oRef;
	    
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

      ObjectReference thisObj = null;
      try {
        thisObj = frame.thisObject();
      } catch (InvalidStackFrameException ex) {
        throw new JDEException("Invalid stack frame.");
      } 
	    
      if (thisObj != null) {
        // Keep track of this object, for future reference from the UI
        m_debugger.getStore().put(thisObj);
		
        // store the fields in this object that themselves are objects in the 
        // ObjectStore, since the user may query for more information about those.
        Map  fieldValues = thisObj.getValues(thisObj.referenceType().visibleFields());
		
        Iterator iter = fieldValues.values().iterator(); 
        while (iter.hasNext()) {
          Value value = (Value) iter.next();
		    
          if (value instanceof ObjectReference) {
            ObjectReference obj = (ObjectReference) value;
			
            //			Debug.printIf(Debug.EVENTS, "storing field with ID: " + obj.uniqueID() + 
            //				      " of type: " + obj.referenceType().name());
            m_debugger.getStore().put((ObjectReference) value);
          }
        }

        // XXX - report 'this' to the GUI
      }
		
      m_debugger.signalCommandResult(m_cmdID, 
                                     Rep.getObjectRep(thisObj, true),
                                     CMD_OK);
	    
    } finally {
      if (weSuspendedThread && (tRef != null)) tRef.resume();
    }
 
  }

  public Object clone() {return new GetThis();}
  
} // GetThis

/*
 * $Log: GetThis.java,v $
 * Revision 1.4  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.3  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.2  2000/08/14 02:42:16  paulk
 * DebugCommandFactory.java
 *
 * Revision 1.1  2000/04/10 05:35:23  paulk
 * Initial revision.
 *
 *
 */

// End of GetTbis.java
