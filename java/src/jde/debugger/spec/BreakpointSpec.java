package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * BreakpointSpec.java
 * <p>
 *
 * <p>
 * Created: Thu Jul 15 12:59:42 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

abstract public class BreakpointSpec extends EventRequestSpec {

  public BreakpointSpec(ReferenceTypeSpec spec) {
    super(spec);
  }

  void setRequest(BreakpointRequest request) {
    super.setRequest(request);
  }
    
} // BreakpointSpec

/*
 * $Log: BreakpointSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of BreakpointSpec.java
