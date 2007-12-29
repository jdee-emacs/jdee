/*
 * Copyright (c) Paul Kinnucan 1998, 1999, 2000, 2001. All Rights Reserved.
 *
 * $Revision: 1.14 $ 
 * $Date: 2004/03/18 05:28:36 $ 
 *
 * MethodOverrideFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * MethodOverrideFactory is distributed in the hope that it will be useful,
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
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import jde.util.DynamicClassLoader;

/**
 * Defines a factory for creating an override of a method defined in a 
 * superclass.
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.14 $
 */

public class MethodOverrideFactory extends MethodFactory
{

  /** The factory. */
  static MethodOverrideFactory overrideFactory;

  Vector candidates = new Vector();

  String baseClassName;

  String methodName;
  
  public MethodOverrideFactory() {}

  /** 
   * Creates a MethodOverrideFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public MethodOverrideFactory(NameFactory factory)
  {
    super(factory);
  }

  /**
   * Return a <code>Vector</code> containing all <code>Method</code>
   * objects for the argument class and methodName which can be
   * overridden.
   *
   * @param cl the <code>Class</code> for which all methods that can
   * be overridden will be returned.
   * @param methodName the name of the Method to be returned.
   * @return a <code>Vector</code> of <code>Method</code> objects.
   */
  private Vector getOverrideableMethods(Class cl, String methodName) {
    
    Vector m = new Vector();
    Class baseClass = cl;

    while (baseClass != null) {
      Method[] methods = baseClass.getDeclaredMethods();
      for (int i = 0; i < methods.length; i++) {
        if (methods[i].getName().equals(methodName)) {
          int methodModifiers = methods[i].getModifiers();
          if ((Modifier.isPublic(methodModifiers) ||
               Modifier.isProtected(methodModifiers)) &&
              !Modifier.isFinal(methodModifiers))
            m.addElement(methods[i]);
        }
      }
      baseClass = baseClass.getSuperclass();
    }

    return m;
  }



  /**
   * Get a list of signatures for all methods of a specified name defined
   * or inherited by a specified class. The signatures returned by this method
   * including only the types of the method parameters as the generated names
   * are unnecessary for signature matching. This method prints the signature
   * list to standard out as a Lisp list form.
   *
   * @param baseClassName a <code>String</code> value that specifies the name of
   * the class that defines or inherits the methods whose signatures are to be
   * returned.
   * @param methodName a <code>String</code> value that specifies the name of the
   * methods whose signatures are to be returned.
   */
  public static void getCandidateSignatures(String baseClassName, String methodName) {

    if (overrideFactory == null) 
      overrideFactory = new MethodOverrideFactory();
    else
      overrideFactory.flush();

    overrideFactory.baseClassName = baseClassName;
    overrideFactory.methodName = methodName;

    try {

      DynamicClassLoader dcl = new DynamicClassLoader();
      Vector methods = overrideFactory.getOverrideableMethods
          (dcl.loadClass(baseClassName), methodName);

      for (int i = 0; i < methods.size(); ++i) {
        Method m = (Method) methods.elementAt(i);
        Signature s = new Signature(m, overrideFactory);
        boolean containsSignature = false;
        for (int j = 0; j < overrideFactory.candidates.size(); ++j) 
          if (s.equals(overrideFactory.candidates.elementAt(j))) {
            containsSignature = true;
            break;
          }
        if (! containsSignature) 
          overrideFactory.candidates.addElement(s);
      }

      int n = overrideFactory.candidates.size();

      if (n > 0) {
	String res = "(list ";
	for (int i = 0; i < n; ++i) {
	  Signature s = (Signature) overrideFactory.candidates.elementAt(i);
          s.setTruncating(false);
	  String p = s.getParameters(s.getMethod().getParameterTypes(), false);
          s.setTruncating(true);
	  res = res + "\"" + methodName + "(" + p +  ")\" ";
	}
	res = res + ")";
	println(res);
      }
      else
	println("(error \"Could not find any method named " +
		methodName + " in " + baseClassName + 
		" or any of its superclasses.\")");

    }
    catch (ClassNotFoundException ee) {
      println("(error \"Could not find class " + 
	      baseClassName + "\")");
    }

  }

  public static void getMethodSkeletonExpression(int variant) {
    Signature s = (Signature) overrideFactory.candidates.elementAt(variant);
    String skel = overrideFactory.getMethodSkeletonExpression(s);
    println(skel);

    // Register imported classes.
    overrideFactory.imports.clear();
    Method m = s.getMethod();

    Class[] types = m.getParameterTypes();
    for (int i = 0; i < types.length; ++i)
      overrideFactory.registerImport(types[i]);

    types = m.getExceptionTypes();
    for (int i = 0; i < types.length; ++i)
      overrideFactory.registerImport(types[i]);

    overrideFactory.registerImport(m.getReturnType());

  }

  public static void getImportedClasses() {
    println(overrideFactory.getImportsAsList());
  }

  /** 
   * Clears the import and candidate hashtables for this factory so they
   * can be re-used to process a new set of interfaces.
   */
  public void flush()
  {
    super.flush();
    candidates.removeAllElements();
  }

  /**
   * Gets the candidate signatures for the class and method
   * passed as arguments on the command line.
   *
   * @param args a <code>String[]</code> value
   */
  public static void main (String[] args) {

    String className = "javax.swing.AbstractAction";
    String methodName = "actionPerformed";
    
    if (args.length == 2) {

      className = args[0];
      methodName = args[1];

    }

    System.out.println("Class: " + className);
    System.out.println("Method: " + methodName);

    getCandidateSignatures(className, methodName);


    
     
  } // end of main ()
  


} // MethodOverrideFactory


/*
 * $Log: MethodOverrideFactory.java,v $
 * Revision 1.14  2004/03/18 05:28:36  paulk
 * Now overrides protected as well as public methods and does not override final methods.
 * Thanks to David Esterkin <desterkin@sciquest.com>.
 *
 * Revision 1.13  2004/02/19 06:27:44  paulk
 * Fixes bug that caused the override wizard to fail to match the method designated by the user to be overriden. Thanks to David Esterkin <desterkin@sciquest.com>.
 *
 * Revision 1.12  2003/10/20 05:39:24  paulk
 * Update the getMethods() method to get all the methods of a class
 * whether declared by the class or inherited by the class, including
 * abstract methods.
 *
 * Revision 1.11  2003/07/23 04:40:07  paulk
 * main method now accepts class and method name for testing purpose. Added comment
 * noting the change in behavior of getDeclaredMethods from JDK 1.3 to JDK 1.4.
 *
 * Revision 1.10  2003/05/04 05:50:52  paulk
 * Document the getCandidateSignatures method.
 *
 * Revision 1.9  2003/05/04 05:42:22  paulk
 * Change getCandidateSignatures to return signatures with parameter
 * types only (i.e., without parameter nams). This fixes a
 * signature-matching bug on the Lisp side where the method override
 * wizard was inadvertently trying to matching a signature without
 * parameter names to a list of signatures with parameter names.
 *
 * Revision 1.8  2002/06/03 18:12:27  mnl
 * Use DynamicClassLoader instead of simple Class.forName to avoid class
 * resolving (linking).
 *
 * Revision 1.7  2002/05/14 06:38:44  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp" <lipp@danet.de>
 * for proposing and implementing this improvement.
 *
 * Revision 1.6  2001/08/14 05:15:03  paulk
 * Miscellaneous updates.
 *
 * Revision 1.5  2001/08/11 06:52:31  paulk
 * Adds javadoc parameter to getMethodSkeleton. Thanks to Javier Lopez.
 *
 *
 */

// End of MethodOverrideFactory.java
