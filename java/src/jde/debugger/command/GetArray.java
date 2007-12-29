package jde.debugger.command;

import java.util.Iterator;
import java.util.List;

import com.sun.jdi.ArrayReference;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.Value;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.JDE;
import jde.debugger.Rep;

/**
 * 'get_array' command. Information about a given array, and,
 * optionally, values of a range of indices
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_array objectID [index, length]
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getArrayRep(ArrayReference, String) array})
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 * 
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 */
public class GetArray extends DebugProcessCommand {
  private static final int MAX_DISPLAY_ELEMENTS = 30;
  
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
    } else if (!(oRef instanceof ArrayReference)) {
      throw new JDEException("Object is not an array");
    }

    // Keep track of this array for later reference.
    m_debugger.getStore().put(oRef);
	
    if (m_args.size() == 0) {
      m_debugger.signalCommandResult(m_cmdID, 
                                     Rep.getArrayRep((ArrayReference)oRef, ""),
                                     CMD_OK, NOQUOTE);
    } else if (m_args.size() == 2) {
      StringBuffer elements = new StringBuffer();
	    
      int index  = Etc.safeGetint(m_args.remove(0), "index");
      int length = Etc.safeGetint(m_args.remove(0), "length");
	    
      List     elementList = ((ArrayReference) oRef).getValues(index, length);
      Iterator it          = elementList.iterator();
      int      numElements = 0;
	    
      while (it.hasNext() && numElements < MAX_DISPLAY_ELEMENTS) {
        numElements++;
        Value value = (Value) it.next();
		
        // store the fields in this object that themselves are objects in the 
        // ObjectStore, since the user may query for more information about those.
        if (value instanceof ObjectReference) {
          m_debugger.getStore().put((ObjectReference) value);
        }
		
        elements.append(" ");
        elements.append(Rep.getValueRep(value));
      }
	    
      if (it.hasNext()) {
        JDE.debug(EVENTS, "did not list all elements");
        // XXX - should be a way to indicate to the debugger that there are more elts.
      }
	    
      m_debugger.signalCommandResult(m_cmdID, 
                                     Rep.getArrayRep((ArrayReference)oRef, elements.toString()),
                                     CMD_OK, NOQUOTE);
    } else {
      throw new JDEException("Syntax error: Wrong number of arguments");
    }
 
  }

  public Object clone() {return new GetArray();}
  
} // GetArray

/*
 * $Log: GetArray.java,v $
 * Revision 1.3  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.2  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/03/03 07:08:39  paulk
 * Initial revision.
 *
 */

// End of GetArray.java
