package jde.wizards;

import java.beans.Introspector;
import java.lang.reflect.Method;

/**
 * Defines a default parameter name factory for the InterfaceFactory.
 * Tries hard to find a meaningfull name.
 *
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Date: 2002/12/12 05:15:48 $ 
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
 *
 * @author Eric D. Friedman
 * @author Paul Kinnucan
 * @author Ole Arndt
 * @version $Revision: 1.6 $
 */

public class DefaultNameFactory implements NameFactory {

  /**
   * Make up a name for the nth parameter of a method.
   * First this method tries to recognize bean methods, if the method is named
   * "setName" and has one parameter, the parameter returned is the
   * lowercased method name with "set" stripped: "name".
   *
   * Index Bean Properties are methods starting with "set" and having two
   * parameters, the first one of which is an int, or methods starting with
   * "get" and having only one int parameter. In both cases the first
   * parameter is given the name "index". 
   *     
   * Examples:
   *
   *   void setName(String name);
   *   void setNames(String[] names);
   *   void setName(int index, String name);
   *   String getName(int index);
   *
   * For other method names the parameters are named according to their
   * type. 
   *
   * Primitive types are named after the following mapping (now
   * hardcoded, can perhaps be made configurable):
   *
   *   boolean: flag
   *   byte: b
   *   char: c
   *   short: s
   *   int: n
   *   long: l
   *   float: f
   *   double: d
   * 
   * Array types are named like their base type with the String "Array"
   * appended, so a "byte[]" parameter will be named "byteArray":
   *
   *    void fill(char[] charArray);
   *
   * All other parameters will be named according to their type's unqualified
   * lowercased classname:
   *
   *    void actionPerformed(ActionEvent actionEvent);
   *
   * To avoid any name clashes, the parameters will have a number appended
   * if there is another parameter of the same type in the methods
   * parameterlist:
   *
   *   void write(byte[] byteArray, int n, int n1);
   *   void put(String string, String string1);
   *
   * @param sig  - signature of the declaring method
   * @param num     - the parameter number whose name we want.
   * @return a name for the n'th parameter of the method.
   */
  public String getParameterName(Signature sig, int num) {
    Method method = sig.getMethod();
    String  methodName = method.getName();
    Class[] parameters = method.getParameterTypes();
    Class   type = parameters[num];

    // To handle inner-class .class files
    String  className = type.getName().replace('$', '.'); 

    // special handling for bean methods
    if (methodName.length() > 3) {
      if (methodName.startsWith("set")) {
        if (parameters.length == 2) {
          // an indexed propery setter
          if (num == 1 && className.equals("int")) {
            return "index";
          }

          if (num == 2) {
            return methodToParamName(methodName, 3);
          }
        }
      
        // a normal propery setter
        if (parameters.length == 1 && num == 1) {
          return methodToParamName(methodName, 3);
        }
      }

      if (methodName.startsWith("get")) {
        // indexed getter
        if (parameters.length == 1 && num == 1 && className.equals("int"))
          return "index";
      }
    }

    return unique(parameters, type, num, getNameFromClass(className));
  }


  /**
   * Generate a name for a parameter from the type of the parameter.
   *
   * @param className  the parameters type
   * @return a more or less fitting name.
   */
  private final String getNameFromClass(String className) {

        
    
    // if this is an classname starting with an upper case letter
    // downcase it and use class name as parameter name
    int i = className.lastIndexOf('.')+1;      
    if (Character.isUpperCase(className.charAt(i))) {
      className = Introspector.decapitalize(className.substring(i));

      // Handle user-defined type array names of the form
      //   [Lname;
      if (className.endsWith(";")) {
        className = className.substring(0, className.length()-1) + "Array";
      }
      
      return className;
    }

    // handle primitive arrays
    if (className.equals("[Z")) {
      return "boolArray";
    }
        
    if (className.equals("[B")) {
      return "byteArray";
    }
        
    if (className.equals("[C"))
      return "charArray";
    if (className.equals("[S"))
      return "shortArray";
    if (className.equals("[I"))
      return "intArray";
    if (className.equals("[J"))
      return "longArray";
    if (className.equals("[F"))
      return "floatArray";
    if (className.equals("[D"))
      return "doubleArray";

    // handle primitives
    if (className.equals("boolean"))
      return "flag";
    if (className.equals("byte"))
      return "b";
    if (className.equals("char"))
      return "c";
    if (className.equals("short"))
      return "s";
    if (className.equals("int"))
      return "n";
    if (className.equals("long"))
      return "l";
    if (className.equals("float"))
      return "f";
    if (className.equals("double"))
      return "d";

    // give up
    return "arg";
  }
    
  /**
   * Make name unique, look if there a more parameters of this type
   * before us. In this case append a number.
   *
   * @param parameters all parameter types of the method
   * @param type the type of the current parameter
   * @param num the position of the current parameter
   * @param name parameter basename
   * @return a unique parameter name
   */
  private final String unique(Class[] parameters, Class type, int num, String name) {
    if (parameters.length > 1) {
      int i = 0;
      for (int j = 0; j < num; ++j)
        if (parameters[j] == type)
          ++i;

      if (i != 0)
        name += i;
    }

    return name;    
  }
    
  /**
   * Strip the first characters from the string and decapitalize
   * the rest.
   *
   * @param name the original name.
   * @param strip number of characters to strip from the front of the name.
   * @return the decapitalized and shortened name.
   */
  private final String methodToParamName(String name, int strip) {
    return Introspector.decapitalize(name.substring(strip));
  }
}

/*
 * $Log: DefaultNameFactory.java,v $
 * Revision 1.6  2002/12/12 05:15:48  paulk
 * Now correctly generates names for method parameters that are arrays of user-defined types.
 *
 * Revision 1.5  2002/12/04 07:06:47  paulk
 * Updated to handle implementation of interfaces that reference inner classes.
 *
 * Revision 1.4  2002/06/06 05:12:44  paulk
 * DefaultNameFactory now generates meaningful method parameter names based
 * on the parameter type or the method name. Thanks to Ole Arndt.
 *
 */

// End of DefaultNameFactory.java
