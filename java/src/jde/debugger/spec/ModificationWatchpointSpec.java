package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.request.*;
import jde.debugger.JDEException;

/**
 * ModificationWatchpointSpec.java
 * <p>
 * 
 * <p>
 * Created: Tue Aug  3 15:42:41 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class ModificationWatchpointSpec extends WatchpointSpec {

  public ModificationWatchpointSpec(ReferenceTypeSpec refSpec, String fieldName) {
    super(refSpec, fieldName);
  }

  boolean resolve(ReferenceType refType) throws JDEException {
    Field field = refType.fieldByName(fieldName);
    if (field == null) {
      throw new JDEException("'"+fieldName+"' does not exist in the class");
    }
    EventRequest er = refType.virtualMachine().eventRequestManager().createModificationWatchpointRequest(field);
    setRequest(er);
    return true;
  }

} // ModificationWatchpointSpec

/*
 * $Log: ModificationWatchpointSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of ModificationWatchpointSpec.java
