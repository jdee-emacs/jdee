/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.11 $ 
 * $Date: 2002/12/04 07:06:46 $ 
 *
 * InterfaceFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * InterfaceFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

/**
 * Defines a method signature.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.11 $
 */

public class Signature {
  
  /** 
   * The method represented by this signature 
  */
  private Method method;

  /** 
   * the parameters in this signature 
   */
  private Class[] parameters;

  /** 
   * the exceptions thrown by this signature 
   */
  private Class[] exceptions;

  /** 
   * the return value 
   */
  private Class returnValue;

  /** 
   * The Class that declared this signature
   */
  private Class declaring;

  /**
   * the InterfaceFactory processing this signature 
   */
  private ClassRegistry registry;

  /**
   * Toggles truncation of package information in signatures 
   */
  private boolean truncate;

  /**
   * Toggles generation of see
   */
  private boolean see;

  /** 
   * Creates a signature for the specified method and register
   * its types with the specified registry (which may be null).
   * Removes package info from generated signatures.
   *
   * @param meth Method represented by this signature
   * @param aregistry Registry containing this signature
   */
  public Signature (Method meth, ClassRegistry aregistry) {
    this(meth, aregistry, true);
  }

  /** 
   * Creates a signature for the specified method and register its
   * types with the specified registry (which may be null).
   *
   * @param meth Method represented by thi signature
   * @param aregistry Registry containing this signature
   * @param truncate toggles removal of package info from generated signatures 
   */
  public Signature (Method meth, ClassRegistry aregistry, boolean truncate) {
    this(meth, aregistry, truncate, false);
  }

  /** 
   * Creates a signature for the specified method and register its
   * types with the specified registry (which may be null).
   *
   * @param meth Method represented by thi signature
   * @param aregistry Registry containing this signature
   * @param truncate toggles removal of package info from generated signatures 
   * @param see Class to link the see section to or null, if no see
   * section should be created
   */
  public Signature (Method meth, ClassRegistry aregistry, boolean truncate,
                    boolean see) {
    this.method   = meth;
    this.registry = aregistry;
    this.truncate = truncate;
    this.see = see;
    
    parameters   = method.getParameterTypes();
    exceptions   = method.getExceptionTypes();
    returnValue = method.getReturnType();
    declaring    = method.getDeclaringClass();

    register();
  }

  /** 
   * Toggles truncation of package info .
   * 
   * @param b Truncation toggle
   */
  public void setTruncating(boolean b) {
    truncate = b;
  }

  /** 
   * Sets the registry used to register this signature's types.
   * 
   * @param registry Registry used to register this signature's type.
   */
  public void setRegistry(ClassRegistry registry) {
    this.registry = registry;
  }

  /** 
   * Gets the class that declared the method represented by this signature.
   *
   * @return The class this method was declared in 
   */
  public Class getDeclaringClass () {
    return declaring;
  }

  /** 
   * Generates a javadoc string for this signature.
   * 
   * @return A javadoc for this method
   */
  public String toJavaDoc() {
    NameFactory factory = registry.getNameFactory();
    StringBuffer buf = new StringBuffer("/**\n *\n *");
    
    for (int i = 0; i < parameters.length; i++) {
      buf.append(" @param "
                 + factory.getParameterName(this, i)
                 + " <description>\n *");
    }

    if (!"void".equals(returnValue.getName())) {
      buf.append(" @return <description>" + "\n *");
    }
    
    for (int i = 0; i < exceptions.length; i++) {
      buf.append(" @exception " + exceptions[i].getName()
                 + " <description>\n *");
    }
    
    if (see) {
      buf.append(" @see ");
      buf.append(method.getDeclaringClass().getName());
      buf.append('#');
      buf.append(method.getName());
      buf.append('(');
      buf.append(getParameters(method.getParameterTypes(), false));
      buf.append(')');
      buf.append("\n *");
    }
    
    buf.append("/");
    
    return buf.toString();
  }

  /** 
   * Gets the signature as a string.
   *
   * @return Signature of this method.
   */
  public String toString() {
    String m    = getModifiers();
    String r    = baseName(returnValue);
    String meth = method.getName();

    String p    = getParameters(parameters);
    String e    = getExceptions(exceptions);

    return m + " " + r + " " + meth + "(" + p + ")" + e;
  }

  /**
   * Describe <code>paramsEqual</code> method here.
   *
   * @param p a <code>Class[]</code> value
   * @return a <code>boolean</code> value
   */
  public boolean paramsEqual(Class[] p) {
    int n = parameters.length;
    boolean res = (p.length == n);
    if (res) {
      for (int i = 0; i < n; ++i) {
        if (!p[i].equals(parameters[i])) {
          res = false;
          break;
       }
      }
    }
    return res;
  }

  /**
   * Tests whether a given object equals this signature.
   * The object is considered equal if it is a signature
   * and it has the same method name and parameter list.
   *
   * @param compare Test object
   * @return <code>true</code> if the test object equals this signature.
   */
  public boolean equals(Object compare) {
    if (compare instanceof Signature) {
      Signature sig = (Signature) compare;
      return method.getName().equals(sig.getMethod().getName())
        && paramsEqual(sig.getMethod().getParameterTypes());
    }
    return false;
  }

  /**
   * Gets the method of which this is a signature.
   *
   * @return Method of which this is a signature.
   */
  public Method getMethod() {
    return method;
  }
    
  /** 
   *  Computes the basename of the specified class.  This returns
   *  "Object" from "java.lang.Object."  It returns the "single"
   *  form of an Array object.
   *
   *  In the case of inner classes, the base name includes
   *  the outer class name, e.g.,
   *
   *    mypackage.OuterClass$InnerClass
   *
   *  is returned as
   *
   *    OuterClass.InnerClass
   *
   * @param type Class whose basename is required
   * 
   * @return basename
   */
  public final String baseName(Class type) {
    String name = null;
    if (type.isArray()) {
      try {
        
        Class cl       = type;
        int dimensions = 0;
        
        while (cl.isArray()) {
          cl = cl.getComponentType();
          dimensions++;
        }
        
        StringBuffer sb = new StringBuffer();
        sb.append(cl.getName());
        
        for (int i = 0; i < dimensions; i++) {
          sb.append("[]");
        }
        
        name = sb.toString();
      } catch (Throwable e) {
        name = type.getName();
      }
    } else {
         name = type.getName();    
    }
    

    // If truncate flag is true, remove
    // the package name from the class
    // name, e.g.,
    //
    //  java.lang.Object
    //
    // becomes simply Object.
    if (truncate) {
      int idx = name.lastIndexOf('.');
      
      if (idx > -1) {
        name = name.substring(idx + 1);
      }
    }  

    // If type is an inner class, its base
    // name includes the outer class name,
    // with a $ separating the inner from
    // the outer name, e.g.,
    //
    //   OuterClass$InnerClass
    //   
    // Replace the $ with  a . (period).
    name =  name.replace('$', '.');

    return name;
  }

  /** 
   * Make a list of this method's exceptions separated by commata.
   * 
   * @return Exceptions thrown by this method.
   */
  public final String getExceptionList() {
    return getExceptionList(exceptions);
  }


  
  /**
   * Make a list of all given exceptions separated by commas.
   *
   * @param except Array of exception types
   * @return Comma-separated list of exceptions
   */
  public final String getExceptionList(Class[] except) {
    
    if ((null == except) || (except.length == 0)) {
      return "";
    }

    StringBuffer buf = new StringBuffer();
    
    for (int i = 0; i < except.length; i++) {
      String type = baseName(except[i]);

      buf.append(type);
      if (i < except.length - 1) {
        buf.append(", ");
      }
      
    }
    
    return buf.toString();
  }

  /** 
   * Gets a throw clause listing the exceptions thrown by this method.
   * 
   * @param except Vector of exceptions
   *
   * @return Exceptions thrown by this method.
   */
  private final String getExceptions(Class[] except) {
    String res = getExceptionList (except);
    
    if (res.length() == 0) {
      return res;
    }
    
    return " throws " + res;
  }

  /** 
   * Gets a parameter list for this method; parameters are named
   * by the NameFactory whose default implementation uses param1
   * .. paramn 
   *
   * @return Parameter list in string form
   */
  public final String getParameters () {
    return getParameters(parameters, true);
  }

  /** 
   * Gets a parameter list for this method; parameters are named
   * by the NameFactory whose default implementation uses param1
   * .. paramn 
   *
   * @param params Parameters of this method
   *
   * @return Parameter list in string form
   */
  public final String getParameters (Class[] params) {
    return getParameters(params, true);
  }

  /** 
   * Gets a parameter list for this method; parameters are named
   * by the NameFactory whose default implementation uses param1
   * .. paramn 
   *
   * @param params Parameters of this method
   * @param withName toggles parameter names
   *
   * @return Parameter list in string form
   */
  public final String getParameters (Class[] params, boolean withName) {
    
    if ((null == params) || (params.length == 0)) {
      return "";
    }
    
    StringBuffer buf = new StringBuffer();
    NameFactory factory = registry.getNameFactory();

    for (int i = 0; i < params.length; i++) {
      String type = baseName(params[i]);
      String name = factory.getParameterName(this, i);

      buf.append(type);
      if (withName) {
        buf.append(' ');
        buf.append(name);
      }

      if (i < params.length - 1) {
        buf.append(", ");
      }
      
    }

    return buf.toString();
  }

  /**
   * Get the names of all parameters of this signature,
   * without typenames.
   *
   * @return te parameter names seperated by comma.
   */
  public final String getParameterNames () {
    return getParameterNames(method.getParameterTypes());
  }    

  /** 
   * Gets a list of parameter names for this method; parameters are
   * named by the NameFactory whose default implementation uses param1
   * .. paramn.  Class type names are not included in the result
   * Contributed by Charles Hart <cfhart@Z-TEL.com>
   *
   * @param params Parameters of this method
   *
   * @return Parameter list in string form */
  public final String getParameterNames (Class[] params) {
    
    if ((null == params) || (params.length == 0)) {
      return "";
    }
    
    StringBuffer buf = new StringBuffer();
    NameFactory factory = registry.getNameFactory();

    for (int i = 0; i < params.length; i++) {
      String type = baseName(params[i]);
      String name = factory.getParameterName(this, i);

      buf.append(name);

      if (i < params.length - 1) {
        buf.append(", ");
      }
      
    }

    return buf.toString();
  }

  /**
   * Gets list of modifiers for this method.
   * Interface methods are always public and may be synchronized 
   *
   * @return a <code>String</code> containing a list of modifiers
   */
  public final String getModifiers () {
    StringBuffer buf = new StringBuffer("public");
    int mod = method.getModifiers();

    if (Modifier.isSynchronized(mod)) {
      buf.append(" synchronized");
    }
    
    return buf.toString();
  }

  /** 
   * Gets the base type of the return value.
   *
   * @return Base type of return value.
   */
  public String getReturnBaseType () {
    return baseName(returnValue);
  }

  /** 
   * Register this Signature's types with the SignatureRegistry
   */
  private final void register() {
    if (null != registry) {
      registry.registerImport(declaring);
      registry.registerImport(returnValue);
    
      for (int i = 0; i < parameters.length; i++) {
        registry.registerImport(parameters[i]);
      }

      for (int i = 0; i < exceptions.length; i++) {
        registry.registerImport(exceptions[i]);
      }
    }
  }

} // end of Signature class

/*
 * $Log: Signature.java,v $
 * Revision 1.11  2002/12/04 07:06:46  paulk
 * Updated to handle implementation of interfaces that reference inner classes.
 *
 * Revision 1.10  2002/06/06 05:12:44  paulk
 * DefaultNameFactory now generates meaningful method parameter names based
 * on the parameter type or the method name. Thanks to Ole Arndt.
 *
 * Revision 1.9  2002/05/14 06:38:43  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp" <lipp@danet.de>
 * for proposing and implementing this improvement.
 *
 * Revision 1.8  2001/09/16 16:24:13  paulk
 * Corrected bug in the constructor Signature (Method meth, ClassRegistry aregistry,
 * boolean truncate).
 * Thanks to petter.mahlen@chello.se.
 *
 * Revision 1.7  2000/08/03 04:28:41  paulk
 * Add support for generating a see secton in the Javadoc comment for a method.
 * Thanks to raffael.herzog@comartis.com
 *
 * Revision 1.6  2000/07/14 05:26:56  paulk
 * Adds support for delegation wizard.
 *
 */

// End of Signature.java
