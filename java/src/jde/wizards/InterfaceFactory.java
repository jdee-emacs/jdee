/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.10 $ 
 * $Date: 2003/09/07 05:29:12 $ 
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
import java.io.PrintWriter;

/**
 * Defines a factory for creating skeleton implementations of 
 * Java interfaces. The factory can be invoked from the command line
 * or from another program. The factory can generate implementations for
 * multiple interfaces when invoked from the command line.
 *
 * @author Eric D. Friedman and Paul Kinnucan
 * @version $Revision: 1.10 $
 */

public class InterfaceFactory extends MethodFactory {

  /** A container for all methods to be generated */
  private SignatureContainer signatures = new SignatureContainer();

  /** The interface factory. */
  private static InterfaceFactory interfaceFactory;
  
  /**
   * Creates a new <code>InterfaceFactory</code> instance.
   *
   */
  protected InterfaceFactory() { }

  /** 
   * Creates an InterfaceFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public InterfaceFactory(NameFactory factory) {
    super(factory);
  }

  
  /**
   * Gets the value of signatures
   *
   * @return the value of signatures
   */
  public SignatureContainer getSignatures()  {
    return this.signatures;
  }

  /**
   * Sets the value of signatures
   *
   * @param argSignatures Value to assign to this.signatures
   */
  public void setSignatures(SignatureContainer argSignatures) {
    this.signatures = argSignatures;
  }

  /**
   * Gets the value of interfaceFactory
   *
   * @return the value of interfaceFactory
   */
  public static InterfaceFactory getTheFactory()  {
    return InterfaceFactory.interfaceFactory;
  }

  /**
   * Sets the value of interfaceFactory
   *
   * @param argInterfaceFactory Value to assign to this.interfaceFactory
   */
  public static void setTheFactory(InterfaceFactory argInterfaceFactory) {
    InterfaceFactory.interfaceFactory = argInterfaceFactory;
  }

 

  /** 
   * Adds a signature to signature container.
   *
   * @param sig Signature to be stored in the signature table.
   */
  protected final void sortByDeclaringClass(Signature sig) {
    signatures.add(sig);
  }

  /** 
   * Clears import hashtable and interface container for this factory so they
   * can be re-used to process a new set of interfaces.
   */
  public void flush() {
    super.flush();
    signatures.clear();
  }

  
  /**
   * Generates signatures based on introspection of the specified interface. 
   * Strips package specifiers from generated signatures.
   *
   * @param interfaceName the interface to process for signatures.
   * @exception ClassNotFoundException the requested interface cannot be found
   * @exception NotAnInterfaceException the requested class is not an interface
  */
  public void process(String interfaceName)
    throws ClassNotFoundException, NotAnInterfaceException {
    process(interfaceName, true);
  }  
  
  /**
   * Generates signatures based on introspection of the specified class. 
   *
   * @param name the interface to process for signatures.
   * @param truncate toggles truncation of package specifiers in signatures..
   *
   * @exception NotAnInterfaceException the requested class isn't an interface
   * @exception ClassNotFoundException Cannot find requested class
   */
  public void process(String name, boolean truncate)
    throws ClassNotFoundException, NotAnInterfaceException {
    
    if (null == namefactory) {
      namefactory = new DefaultNameFactory();
    }
    
    Class aclass = Class.forName(name);
    if (false == aclass.isInterface()) {
      throw new NotAnInterfaceException(name);
    }
    
    Method[] methods = aclass.getMethods();
    
    for (int i = 0; i < methods.length; i++) {
      sortByDeclaringClass(new Signature(methods[i], this, truncate));
    }
  }


  /**
   * Makes an implementation of an interface.
   *
   * @param name Name of interface to be implemented.
   * @param truncate If <code>true</code>, truncate package specifier
   * when generating code.
   */
  protected void implementInterface(String name, boolean truncate) {
    
    try {
      process(name, truncate);
    } catch (ClassNotFoundException e) {
      println("(error \"Error: could not find interface named: " + name + ". "
              + "Note: name must be qualified.\")");
      return;
    } catch (NotAnInterfaceException e) {
      println("(error \"Error: " + name + " is not an interface.\")");
      return;
    } catch (Exception e) {
      println("(error \"Error: unknown type.\")");
      return;
    }

    dumpExpression(new PrintWriter(System.out, true), truncate);
  }

  /**
   * Makes an expression for generating the implementation of an
   * interface. This method delegates the creation of the
   * implementation to makeInterfaceExpressionInternal.
   *
   * @param name Name of interface to be implemented.
   * @param truncate If <code>true</code>, truncate package specifier
   * when generating code.
   */
  public static void makeInterfaceExpression(String name, boolean truncate) {

    InterfaceFactory theFactory = getTheFactory();

    if (theFactory == null) {
      theFactory = new InterfaceFactory();
      setTheFactory(theFactory);
    }

    theFactory.flush();
    theFactory.implementInterface(name, truncate);

  }


  /**
   * Prints imported classes to standard out.
   *
   */
  public static void getImportedClasses() {
    println(interfaceFactory.getImportsAsList());
  }


  /**
   * Describe <code>dumpExpression</code> method here.
   *
   * @param out a <code>PrintWriter</code> value
   * @param truncate a <code>boolean</code> value
   */
  public void dumpExpression(PrintWriter out, boolean truncate) {
    
    final StringBuffer buf =
      new StringBuffer("(jde-wiz-gen-implementation-methods (list ");
    
    signatures.visit(new SignatureVisitor() {
        public void visit(Signature sig , boolean firstOfClass) {
          if (firstOfClass) {
            buf.append ("(quote ");
            buf.append("\"Implementation of ");
            buf.append(sig.getDeclaringClass().getName());
            buf.append("\")");
          }
          buf.append ("(quote ");
          buf.append(getMethodSkeletonExpression(sig));
          buf.append (")");
        }
      }
                     );

    buf.append("))");
    println(buf.toString());
  }

} // SignatureFactory

/**
 * Thrown when the factory is asked to implement an object that is not an interface.
 *
 * @author <a href="mailto:paulk@mathworks.com">Paul Kinnucan</a>
 * @version 1.0
 */
class NotAnInterfaceException extends Exception {
  
  /**
   * Creates a new <code>NotAnInterfaceException</code> instance.
   *
   * @param name a <code>String</code> value
   */
  NotAnInterfaceException (String name) {
    super(name);
  }
}

/*
 * $Log: InterfaceFactory.java,v $
 * Revision 1.10  2003/09/07 05:29:12  paulk
 * Check for duplicate methods defined by different classes or interfaces.
 * Thanks to Martin Schwamberg.
 *
 * Revision 1.9  2002/05/14 06:38:44  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp" <lipp@danet.de>
 * for proposing and implementing this improvement.
 *
 * Revision 1.8  2001/10/21 14:06:41  paulk
 * Changed some methods from private to protected to permit overriding by descendent classes.
 *
 * Revision 1.7  2001/08/04 03:24:10  paulk
 * DefaultNameFactory.java
 *
 * Revision 1.6  2000/08/01 08:23:22  paulk
 * Fixes bug in dump method. Thanks to Eric Friedman, eric@hfriedman.rdsl.lmi.net.
 *
 *
 */

// End of InterfaceFactory.java
