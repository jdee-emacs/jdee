package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * ExceptionSpec.java
 * <p>
 * 
 * <p>
 * Created: Mon Aug  2 17:01:35 1999
 * 
 * @author Amit Kumar
 * @author Paul Kinnucan
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class ExceptionSpec extends EventRequestSpec {

  boolean notifyCaught;

  boolean notifyUncaught;

  public ExceptionSpec(ReferenceTypeSpec spec, 
                       boolean notifyCaught, boolean notifyUncaught) {
    super(spec);
    this.notifyCaught = notifyCaught;
    this.notifyUncaught = notifyUncaught;
  }

  public boolean resolve(ReferenceType refType) {
    ExceptionRequest er =
      refType.virtualMachine().eventRequestManager().createExceptionRequest(refType, notifyCaught, notifyUncaught);
    super.setRequest(er);
    return true;
  }

} // ExceptionSpec

/*
 * $Log: ExceptionSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of ExceptionSpec.java

