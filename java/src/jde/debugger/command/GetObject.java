package jde.debugger.command;

import java.util.Iterator;
import java.util.Map;

import com.sun.jdi.ObjectReference;
import com.sun.jdi.Value;
import jde.debugger.Etc;
import jde.debugger.JDEException;
import jde.debugger.Rep;


/**
 * 'get_object' command. Information about a particular object.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * get_object objectID
 * </pre>
 *
 * <b>Returns:</b>
 * <pre>
 * (jde-dbo-command-result cmd_id {@link Rep#getObjectRep(ObjectReference) detailed-object-info})
 * </pre>
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.3 $
 *
 */
public class GetObject extends DebugProcessCommand {

  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    if (m_args.size() < 1)
      throw new JDEException("Insufficient arguments");

    Long            uniqueID = Etc.safeGetLong(m_args.remove(0), "object ID");
    ObjectReference oRef     = m_debugger.getStore().get(uniqueID);

    if (oRef == null)
      throw new JDEException("No such object exists");

    // store the fields in this object that themselves are objects in the
    // ObjectStore, since the user may query for more information about those.
    Map  fieldValues = oRef.getValues(oRef.referenceType().visibleFields());

    Iterator iter = fieldValues.values().iterator();
    while (iter.hasNext()) {
      Value value = (Value) iter.next();

      if (value instanceof ObjectReference) {
        m_debugger.getStore().put((ObjectReference) value);
      }
    }

    m_debugger.signalCommandResult(m_cmdID,
				   Rep.getObjectRep(oRef, true),
				   CMD_OK);
  }

  public Object clone() {return new GetObject();}

} // GetObject

/*
 * $Log: GetObject.java,v $
 * Revision 1.3  2003/04/29 16:52:10  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.2  2003/01/15 05:56:26  paulk
 * Add Petter Mahlen's changes.
 *
 * Revision 1.1  2001/03/24 05:52:13  paulk
 * Initial version.
 *
 *
 */

// End of Finish.java
