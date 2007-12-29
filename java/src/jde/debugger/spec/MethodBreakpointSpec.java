package jde.debugger.spec;

import com.sun.jdi.*;
import com.sun.jdi.request.*;

import java.util.*;
import jde.debugger.JDEException;

/**
 * MethodBreakpointSpec.java
 * <p>
 * Funnily, it seems this class actually supports setting breakpoints in
 * a particular method of an arbitrary filename! since it's very weird, it's
 * not supported. Let us know if you require such a feature...
 * <p>
 * Created: Thu Jul 15 15:52:45 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class MethodBreakpointSpec extends BreakpointSpec {

  String methodName;
  List methodArgs;
    
  public MethodBreakpointSpec(ReferenceTypeSpec refSpec,
                              String methodName, List methodArgs) {
    super(refSpec);
    this.methodName = methodName;
    this.methodArgs = methodArgs;
  }

  boolean resolve(ReferenceType refType) throws JDEException {
    if (!isValidMethodName(methodName)) {
      throw new JDEException("'"+methodName+"' is not a valid method name.");
    }
    if (!(refType instanceof ClassType)) {
      throw new JDEException("'"+refType+"' is not a Class"); 
    }
    Location location = getLocation((ClassType)refType);
    if (location == null) {
      throw new JDEException("Can't set breakpoint on an abstract/native method");
    }
    BreakpointRequest br = refType.virtualMachine().eventRequestManager().createBreakpointRequest(location);

    setRequest(br);
    return true;
  }

  private Location getLocation(ClassType clazz) throws JDEException {
    Method method = findMatchingMethod(clazz);
    Location location = method.location();
    return location;
  }
    
  public String getMethodName() {
    return methodName;
  }

  public List getMethodArgs() {
    return methodArgs;
  }

  public String toString() {
    StringBuffer buffer = new StringBuffer("break in_method ");
    if (refSpec instanceof SourceNameReferenceTypeSpec) {
      buffer.append(((SourceNameReferenceTypeSpec)refSpec).getSourceName());
    } else if (refSpec instanceof PatternReferenceTypeSpec) {
      buffer.append(((PatternReferenceTypeSpec)refSpec).getClassPattern());
    }
    buffer.append(" "+methodName+" ");
    if (methodArgs != null) {
      Iterator iter = methodArgs.iterator();
      boolean first = true;
      buffer.append('(');
      while (iter.hasNext()) {
        if (!first) {
          buffer.append(',');
        }
        buffer.append((String)iter.next());
        first = false;
      }
      buffer.append(")");
    }
    return buffer.toString();
  }

  private boolean isValidMethodName(String s) {
    return isJavaIdentifier(s) || 
      s.equals("<init>") ||
      s.equals("<clinit>");
  }

  /* 
   * Compare a method's argument types with a Vector of type names.
   * Return true if each argument type has a name identical to the 
   * corresponding string in the vector and if the number of 
   * arguments in the method matches the number of names passed
   */
  private boolean compareArgTypes(Method method, List nameList) {
    List argTypeNames = method.argumentTypeNames();


    // If argument counts differ, we can stop here
    if (argTypeNames.size() != nameList.size()) {
      return false;
    }

    // Compare each argument type's name
    for (int i=0; i<argTypeNames.size(); ++i) {
      String comp1 = (String)argTypeNames.get(i);
      String comp2 = (String)nameList.get(i);
      if (! comp1.equals(comp2)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Remove unneeded spaces and expand class names to fully 
   * qualified names, if necessary and possible.
   */
  private String normalizeArgTypeName(String name)
    throws JDEException {
    /* 
     * Separate the type name from any array modifiers, 
     * stripping whitespace after the name ends
     */
    int i = 0;
    StringBuffer typePart = new StringBuffer();
    StringBuffer arrayPart = new StringBuffer();
    name = name.trim();
    while (i < name.length()) {
      char c = name.charAt(i);
      if (Character.isWhitespace(c) || c == '[') {
        break;      // name is complete
      }
      typePart.append(c);
      i++;
    }
    while (i < name.length()) {
      char c = name.charAt(i);
      if ( (c == '[') || (c == ']') ) {
        arrayPart.append(c);
      } else if (!Character.isWhitespace(c)) {
        throw new JDEException("At least one of the arguments of method '"+methodName+"' is invalid.");
      }
      i++;
    }
    name = typePart.toString();

    /*
     * When there's no sign of a package name already, 
     * try to expand the 
     * the name to a fully qualified class name
     */
    if ((name.indexOf('.') == -1) || name.startsWith("*.")) {
      try {
        List refs = new LinkedList(); // XXX to get it to compile
        //                List refs = proc.findClassesMatchingPattern(name);
        // if more than one class match, take the first, but
        // inform anyways.
        if (refs.size() > 0) {
          name = ((ReferenceType)(refs.get(0))).name();
          // warn
          if (refs.size() > 1) {
            //			jde.signal(procID, WARNING, "(Method Breakpoint Warning) More than one classes matched resolving an argument for method '"+methodName+"'. Defaulting to the first match.");
          }
        }
      } catch (IllegalArgumentException e) {
        // We'll try the name as is 
      }
    }
    name += arrayPart.toString();
    return name;
  }

  /* 
   * Attempt an unambiguous match of the method name and 
   * argument specification to a method. If no arguments 
   * are specified, the method must not be overloaded.
   * Otherwise, the argument types much match exactly 
   */
  private Method findMatchingMethod(ClassType clazz) 
    throws JDEException {

    // Normalize the argument string once before looping below.
    List argTypeNames = null;
    if (methodArgs != null) {
      argTypeNames = new ArrayList(methodArgs.size());
      Iterator iter = methodArgs.iterator();
      while (iter.hasNext()) {
        String name = (String)iter.next();
        name = normalizeArgTypeName(name);
        argTypeNames.add(name);
      }
    }

    // Check each method in the class for matches
    Iterator iter = clazz.methods().iterator();
    Method firstMatch = null;  // first method with matching name
    Method exactMatch = null;  // (only) method with same name & sig
    int matchCount = 0;        // > 1 implies overload
    while (iter.hasNext()) {
      Method candidate = (Method)iter.next();

      if (candidate.name().equals(getMethodName())) {
        matchCount++;

        // Remember the first match in case it is the only one
        if (matchCount == 1) {
          firstMatch = candidate;
        }

        // If argument types were specified, check against candidate
        if ((argTypeNames != null) 
            && compareArgTypes(candidate, argTypeNames) == true) {
          exactMatch = candidate;
          break;
        }
      }
    }

    // Determine method for breakpoint
    Method method = null;
    if (exactMatch != null) {
      // Name and signature match
      method = exactMatch;
    } else if ((argTypeNames == null) && (matchCount > 0)) {
      // At least one name matched and no arg types were specified
      if (matchCount == 1) {
        method = firstMatch;       // Only one match; safe to use it
      } else {
        throw new JDEException("Ambiguous method '"+methodName+"'. Specify arguments.");
      }
    } else {
      throw new JDEException("No method named '"+methodName+"' in class.");
    }
    return method;
  }
    
} // MethodBreakpointSpec

/*
 * $Log: MethodBreakpointSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of MethodBreakpointSpec.java
