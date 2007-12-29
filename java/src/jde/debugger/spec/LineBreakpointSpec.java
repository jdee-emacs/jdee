package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

import java.util.*;
import jde.debugger.JDEException;

/**
 * LineBreakpointSpec.java
 * <p>
 *
 * <p>
 * Created: Thu Jul 15 13:00:34 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.4 $
 */

public class LineBreakpointSpec extends BreakpointSpec {

  private int lineNumber;

  public LineBreakpointSpec(ReferenceTypeSpec refSpec,
                            int line) {
    super(refSpec);
    this.lineNumber = line;
  }

  boolean resolve(ReferenceType refType) throws JDEException {
    if (!(refType instanceof ClassType)) {
      // remove spec from list of current specs
      throw new JDEException("'"+refType+"' is not a Class");
    }
    Location location = getLocation((ClassType)refType);
    BreakpointRequest br = refType.virtualMachine().eventRequestManager().createBreakpointRequest(location);
	
    setRequest(br);
    return true;
  }

  private Location getLocation(ClassType clazz) throws JDEException {
    Location location = null;
    try {
      List locs = null;
      try {
        locs = clazz.locationsOfLine(lineNumber);
      } catch (InvalidLineNumberException ex) {
        throw new JDEException("Line #"+lineNumber+" does not exist in "+clazz+".");
      }		
      if (locs.size() == 0) {
        // remove spec from list of current specs
        throw new JDEException("Line #"+lineNumber+" does not exist int "+clazz+".");
      }
      // XXX handle multiple locations
      location = (Location)locs.get(0);
      if (location.method() == null) {
        // remove spec from list of current specs
        throw new JDEException("Line #"+lineNumber+" does not correspond to a method in "+
                               clazz + ".");
      } 
    } catch (AbsentInformationException e) {
      // remove spec from list of current specs
      throw new JDEException("Line Information missing for Class '"+clazz+"'");
    }
    return location;
  }

  public int getLineNumber() {
    return lineNumber;
  }

} // LineBreakpointSpec

/*
 * $Log: LineBreakpointSpec.java,v $
 * Revision 1.4  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of LineBreakpointSpec.java
