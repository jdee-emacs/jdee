package jde.wizards;

import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Vector;

/**
 * Defines a factory for creating skeleton implementations of the abstract
 * methods of Abstract Java classes. The factory can be invoked from the
 * command line or from another program.
 *
 * 
 * This class extends the interface factory to handle Abstract classes
 * Copyright (c) Javier Lopez 2001. All Rights Reserved.
 *
 * AbstractClassFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * AbstractClassFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 *
 * @author Javier Lopez
 * @version $Revision: 1.7 $
 */
public class AbstractClassFactory extends InterfaceFactory {

    /** The interface factory. */
    static AbstractClassFactory abstractClassFactory;
  
    public AbstractClassFactory() {}

    /** 
     * Creates an AbstractClassFactory that uses the specified NameFactory for
     * generating parameter names
     *
     * @param factory Factory for generating parameter names
     */
    public AbstractClassFactory(NameFactory factory) {
        super(factory);
    }

    /**
     * Generates signatures based on introspection of the specified interface.
     * Strips package specifiers from generated signatures.
     *
     * @param argAbstracClassName the abstract class to process for signatures.
     * @exception NotAnAbstractClassException the requested class isn't an
     * abstract class
     * @exception java.lang.ClassNotFoundException the requested class cannot
     * be loaded
     */
    public void process(String argAbstracClassName)
        throws ClassNotFoundException, NotAnAbstractClassException {
        process(argAbstracClassName, true);
    }  
  
    /**
     * Generates signatures based on introspection of the specified class.
     *
     * @param name the abstract class to process for signatures.
     * @param truncate toggles truncation of package specifiers in signatures.
     *
     * @exception NotAnAbstractClassException the requested class isn't an
     * abstract class
     * @exception java.lang.ClassNotFoundException the requested class cannot
     * be loaded
     */
    public void process(String name, boolean truncate)
        throws ClassNotFoundException, NotAnAbstractClassException {
        if (null == namefactory)
            namefactory = new DefaultNameFactory();
    
        Class aclass = Class.forName(name);
        int iModifiers = aclass.getModifiers();
        if (!Modifier.isAbstract(iModifiers))
            throw new NotAnAbstractClassException(name);
    
        Vector methods = new Vector();
        getAbstractMethods(aclass, methods);
        int size = methods.size();
        for (int i = 0; i < size; i++)
            sortByDeclaringClass(new Signature((Method)methods.get(i),
                                               this,
                                               truncate));
    }

    /**
     * Creates a list of the abstract methods in argClass
     *
     * @param argClass Class to obtained the abstract methods from
     * @param abstractMethods Contains a list of the abstract methods.
     * @return a <code>Methods[]</code> containing abstract methods
     */
    private void getAbstractMethods(Class argClass, Vector abstractMethods) {
        Method[] methods = argClass.getMethods();
        Method[] declaredMethods = argClass.getDeclaredMethods();
        addMethods(methods, abstractMethods);
        addMethods(declaredMethods, abstractMethods);
    }

    /**
     * Adds the abstract methods in <code>methods</code> into abstractMethods.
     *
     * @param methods a <code>Method[]</code> to be added
     * @param abstractMethods a <code>Vector</code> to add abstract methods.
     */
    private void addMethods(Method[] methods, Vector abstractMethods) {
        Method method;
        int modifiers;
        for (int i = 0; i < methods.length; i++) {
            method = methods[i];
            modifiers = method.getModifiers();
            
            if (Modifier.isAbstract(modifiers)
                && !abstractMethods.contains(method)) {
                abstractMethods.add(method);
            } // end of if (method)
        } // end of for (int i = 0; i < methods.length; i++)
    }

    public static void getImportedClasses() {
        println(abstractClassFactory.getImportsAsList());
    }
  
    /**
     * Makes an implementation of the abstract methods of an abstract class.
     * This method delegates the creation of the implementation to
     * makeAbstractClassInternal.
     *
     * @param name Name of abstract class to be implemented.
     * @param truncate If <code>true</code>, truncate package specifier
     * when generating code.
     */
    public static void makeAbstractClassExpression 
        (String name, boolean truncate) {
    
        if (abstractClassFactory == null)
            abstractClassFactory = new AbstractClassFactory();
    
        abstractClassFactory.flush();
        abstractClassFactory.makeAbstractClassExpressionInternal(name,
                                                                 truncate);
    }

    /**
     * Makes an implementation of the abstract methods of an abstract class.
     *
     * @param name Name of abstract class to be implemented.
     * @param truncate If <code>true</code>, truncate package specifier
     * when generating code.
     */
    private void makeAbstractClassExpressionInternal 
        (String name, boolean truncate) {
        try {
            process(name, truncate);
        } catch (ClassNotFoundException e) {
            println("(error \"Error: could not find abstract class named: " 
                    + name + ". " + "Note: name must be qualified.\")");
            return;
        } catch (NotAnAbstractClassException e) {
            println("(error \"Error: " + name
                    + " is not an abstract class.\")");
            return;
        } catch (Exception e) {
            e.printStackTrace();
            println("(error \"Error: unknown type.\")");
            return;
        }

        dumpExpression(new PrintWriter(System.out, true), truncate);
    }

    public static void main(String[] args) {
        AbstractClassFactory.makeAbstractClassExpression
            ("javax.swing.AbstractAction", false);
    }
}

class NotAnAbstractClassException extends NotAnInterfaceException {
    NotAnAbstractClassException (String name) {
        super(name);
    }
}// End of AbstractClassFactory

/*
 * $Log: AbstractClassFactory.java,v $
 * Revision 1.7  2003/10/19 14:45:30  jslopez
 * Fixes getAbstractMethods to get the methods included by getDeclaredMethods and
 * getMethods.
 *
 * Revision 1.6  2003/10/10 12:51:12  jslopez
 * Fixes generating skeleton for all methods declared by superclass including
 * those that the class or superclass provides an implementation.
 *
 * Revision 1.5  2002/12/04 07:16:38  paulk
 * Cosmetic changes.
 *
 * Revision 1.4  2002/08/30 12:50:10  jslopez
 * Fixes regression bug.
 * Now methods defined in super classes and interfaces are as well
 * added.
 *
 * Revision 1.3  2002/08/30 12:29:58  jslopez
 * Fixes bug generating abstract class skeletons.
 *
 * Revision 1.2  2002/05/14 06:38:44  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp"
 * <lipp@danet.de> for proposing and implementing this improvement.
 * Revision 1.1  2001/08/04 03:14:06  paulk
 * Initial revision.
 *
 */


