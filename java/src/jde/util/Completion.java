/*
 *    
 *    Completion.java
 *    Copyright (C) 1999, 2001, 2002, 2003 Rodrigo Reyes (reyes@chez.com)
 *
 *    $Revision: 1.23 $
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package jde.util;

import java.lang.reflect.Modifier;
import java.lang.reflect.Field;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.io.Writer;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.IOException;

/**
 * This class provides completion facilities.
 *
 * @author Rodrigo Reyes (reyes@chez.com) 
 */

public class Completion {
  /*************************************************************************
   * Constants
   *************************************************************************/
  public static final String NIL = "nil";
  public static final String NL = "\n";
  public static final String T = "t";
  public static final String LIST = "list";
  public static final String START_PAREN = "(";
  public static final String END_PAREN = ")";
  public static final String DOUBLE_QUOTE = "\"";
  public static final String SPACE = " ";
  public static final String START_LIST;
  static {
    StringBuffer sb = new StringBuffer (10);
    sb.append(START_PAREN);
    sb.append(LIST);
    sb.append(SPACE);
    START_LIST = sb.toString();
  }
    
  /**
   * Access level for a class member
   */
  public static final int PUBLIC    = 0;
    
  /**
   * Access level for a class member
   */
  public static final int PROTECTED = 1;
    
  /**
   * Access level for a class member
   */
  public static final int DEFAULT   = 2;
    
  /**
   * Access level for a class member
   */
  public static final int PRIVATE   = 3;

  /**
   * Tests whether a class is an ancestor of another class.
   * This method prints "t" to standard out if the class is an ancestor
   * of the other class. If the class is not an ancestor or either class
   * cannot be found, this method prints nil to standard out.
   * @param ancestor Name of the ancestor class
   * @param child  Name of the supposed child class
   */
  public static void isAncestorOf(String ancestor, String child) {
    try {
      Class classAncestor = Class.forName(ancestor);
      Class classChild = Class.forName(child);
      if (classAncestor.isAssignableFrom(classChild)) {
	System.out.println(T);
      } else {
	System.out.println(NIL);
      }	  
    } catch (Exception ex) {
      System.out.println(NIL);
    }
  }
    
  /**
   * Returns true if the entity is accessible to the specified level of
   * protection.
   * @param modifiers the modifiers as returned by Member.getModifiers()
   * or Class.getModifiers() 
   * @param level the level of protection
   * @return if the Member is accessible to the specified level of
   * protection.
   * @see Member.getModifiers()
   * @see Class.getModifiers()
   */
  private static boolean isAccessible(int modifiers, int level) {
    switch(level) {
    case PUBLIC:    // member is accessible if it has public access
      return  Modifier.isPublic    (modifiers);
    case PROTECTED: // accessible if member is protected
      return  Modifier.isProtected (modifiers);
    case DEFAULT: // accessible if member is not public, protected
      // or private
      return (!Modifier.isPublic (modifiers)
	      && !Modifier.isProtected(modifiers)
	      && !Modifier.isPrivate  (modifiers));
    case PRIVATE:   // accessible if member is private
      return  Modifier.isPrivate   (modifiers);
    default:
      // cannot get here any more, since the access level is
      // only used internally
      throw new Error("Completion.isAccessible(int, int) "
		      + "called with incorrect access level parameter:"
                      + level);
    }//switch
  }
    
  /**
   * Prints (list "name" "type") to the system output.
   *
   * @param name field name 
   * @param type field type
   */
  private static String printField(String name, String type) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(name));
    sb.append(SPACE);
    sb.append(printWithinQuotes(type));
    sb.append(END_PAREN);
        
    return sb.toString();
  }
    
  /**
   * Prints (list "name" "params") to the system output.
   *
   * @param name constructor name
   * @param params parameter type
   */
  private static String printConstructor(String name, Class[] params) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(name));
    sb.append(SPACE);
    sb.append(listClassArray(params));
    sb.append(SPACE);
    
    return sb.toString();
  }

  /**
   * Prints (list "name" "returnType" "args") to the system output.
   *
   * @param name method name
   * @param returnType method return type
   * @param args method arguments
   */
  private static String printMethod(String name,
                                    String returnType,
                                    Class[] args) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(name));
    sb.append(SPACE);
    sb.append(printWithinQuotes(returnType));
    sb.append(SPACE);
    sb.append(listClassArray(args));
    sb.append(SPACE);
        
    return sb.toString(); 
  } 

  /**
   * Prints (list "className") to the system output.
   *
   * @param name className
   */
  private static String printClass(String name) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(name));
    sb.append(END_PAREN);
    return sb.toString();
  }
    
  /**
   * <code>printExceptions</code>
   *
   * @param exceptions a <code>Class[]</code>
   * @return a <code>String</code>
   */
  private static String printExceptions(Class[] exceptions) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(listClassArray(exceptions));
    sb.append(END_PAREN);
    return sb.toString();
  }
    
  /**
   * Prints item within quotes i.e "item"
   *
   * @param item string to be quoted.
   */
  private static String printWithinQuotes(String item) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(DOUBLE_QUOTE);
    sb.append(item);
    sb.append(DOUBLE_QUOTE);
        
    return sb.toString();
  }

  /**
   * Recursively finds fields of the specified access level in the
   * supplied class and superclasses.
   *
   * @param c the class to start the search in - nothing is done if
   * this is NULL
   * @param level  the access level to look for
   * @param sb the StringBuffer where the results should be added
   */
  private static void recursiveListFields(Class c,
                                          int level,
                                          StringBuffer sb)  {
    //This is only used while initializing
    if (c == null) {
      return;
    }

    Field[] fields;
    Field   field;
    String  f;

    // ----- Added by Petter for interfaces
    if (level == PUBLIC) {
      // For public access, we can use getFields() and skip
      // recursion. This ensures that the method works for
      // interface types, and saves some function calls.
      fields = c.getFields();
    } else {
      fields = c.getDeclaredFields();
    }
    // ----- End addition by Petter

    for (int index = 0; index < fields.length ; index++) {
      field = fields[index];
      if (isAccessible(field.getModifiers(), level)) {
        f = printField(field.getName(),
                       className(field.getType()));
        if (sb.toString().lastIndexOf(f) == -1) {
          sb.append(f);
        }
      }
    }

    // ----- Addition by Petter to reduce the number of function
    //       calls and not list non-accessible private fields.
    if (!c.isInterface()
	&& level != PRIVATE
	&& level != PUBLIC) {
      // For interfaces, the getSuperClass() method will return
      // nil, and in any case, the only type of access relevant
      // for interfaces is PUBLIC. For PUBLIC access, the
      // getFields() call has listed all the relevant fields.
      // For PRIVATE access, that is only applicable in the
      // calling class anyway, so we shouldn't do recursion.
      recursiveListFields(c.getSuperclass(), level, sb);
    }
    // ----- End addition by Petter
  }

  /**
   * Finds constructors of the specified access level in the supplied class.
   *
   * @param c the class to search in
   * @param level  the access level to look for
   * @param sb the StringBuffer where the results should be added
   */
  private static void listConstructors(Class c,
                                       int level,
                                       StringBuffer sb)  {
    Constructor[] constrs = c.getDeclaredConstructors();
    Constructor constructor;
    Class[] exceptions;
    StringBuffer cons;
        
    for (int index = 0; index < constrs.length; index++) {
      constructor = constrs[index];
      if (isAccessible(constructor.getModifiers(), level)) {
        cons = new StringBuffer(100);
        cons.append(printConstructor(constructor.getName(),
                                     constructor.getParameterTypes()));
        // Add exceptions
        exceptions = constructor.getExceptionTypes();
                
        if (exceptions.length > 0) {
          cons.append(printExceptions(exceptions));
        } else {
          cons.append(NIL);
        }
        cons.append(END_PAREN);
        if (sb.toString().lastIndexOf(cons.toString()) == -1) {
          sb.append(cons);
        }
      }
    }
  }
  
  /**
   * Recursively finds methods of the specified access level in the
   * supplied class and superclasses.
   *
   * @param c the class to start the search in - nothing is done if this is
   * NULL
   * @param level  the access level to look for
   * @param sb the StringBuffer where the results should be added
   */
  private static void recursiveListMethods(Class c,
                                           int level,
                                           StringBuffer sb)  {
    //This is only used while initializing
    if (c == null) {
      return;
    }
	
    Method[] methods;
    Method method;
    Class[] exceptions;
    StringBuffer temp;

    // ----- Added by Petter for interfaces
    if (level == PUBLIC) {
      // For public access, we can use getMethods() and skip
      // recursion. This ensures that the method works for
      // interface types, and saves some function calls.
      methods = c.getMethods();
    } else {
      methods = c.getDeclaredMethods();
    }
    // ----- End addition by Petter
        
    for (int index = 0; index < methods.length ; index++) {
      method = methods[index];
      if (isAccessible(method.getModifiers(), level)) {
        temp = new StringBuffer(100);
        temp.append(printMethod(method.getName(),
                                className(method.getReturnType()),
                                method.getParameterTypes()));
        // Add exceptions
        exceptions = method.getExceptionTypes();
                
        if (exceptions.length > 0) {
          temp.append(printExceptions(exceptions));
        } else {
          temp.append(NIL);
        }
        temp.append(END_PAREN);
                
        if (sb.toString().lastIndexOf(temp.toString()) == -1) {
          sb.append(temp);
        }
      }
    }


    // ----- Addition by Petter to reduce the number of function
    //       calls and not list non-accessible private fields.
    if (!c.isInterface()
	&& level != PRIVATE
	&& level != PUBLIC) {
      // For interfaces, the getSuperClass() method will return
      // nil, and in any case, the only type of access relevant
      // for interfaces is PUBLIC. For PUBLIC access, the
      // getMethods() call has listed all the relevant members.
      // For PRIVATE access, that is only applicable in the
      // calling class anyway, so we shouldn't do recursion.
      recursiveListMethods(c.getSuperclass(), level, sb);
    }
    // ----- End addition by Petter
  }
        
  /**
   * Recursively finds inner classes of the specified access level
   * in the supplied class and superclasses.
   *
   * @param c the class to start the search in - nothing is done if this is
   * NULL
   * @param level  the access level to look for
   * @param sb the StringBuffer where the results should be added
   */
  private static void recursiveListInnerClasses(Class c,
                                                int level,
                                                StringBuffer sb) {
    if (c == null) {
      return;
    }

	
    Class[] innerClasses;
    Class   innerClass;
    String  clas;

    // ----- Added by Petter for interfaces
    if (level == PUBLIC) {
      // For public access, we can use getClasses() and skip
      // recursion. This ensures that the method works for
      // interface types, and saves some function calls.  XXX -
      // actually, this doesn't work properly, since classes
      // defined in interfaces don't show up here.
      innerClasses = c.getClasses();
    }
    else {
      innerClasses = c.getDeclaredClasses();
    }
    // ----- End addition by Petter
        
    for (int index = 0; index < innerClasses.length ; index++) {
      innerClass = innerClasses[index];
      if (isAccessible(innerClass.getModifiers(), level)) {
        clas = printClass(innerClass.getName());
        if (sb.toString().lastIndexOf(clas) == -1) {
          sb.append(clas);
        }
      }
    }

    // ----- Addition by Petter to reduce the number of function
    //       calls and not list non-accessible private fields.
    if (!c.isInterface() &&
        level != PRIVATE &&
        level != PUBLIC) {
      // For interfaces, the getSuperClass() method will return
      // nil, and in any case, the only type of access relevant
      // for interfaces is PUBLIC. For PUBLIC access, the
      // getClasses() call has listed all the relevant members.
      // For PRIVATE access, that is only applicable in the
      // calling class anyway, so we shouldn't do recursion.
      recursiveListInnerClasses(c.getSuperclass(), level, sb);
    }
    // ----- End addition by Petter
  } 
    
  private static void listClassInfo(Class c, int level, StringBuffer sb) {
    sb.append(START_LIST);
        
    //we add the protected/private fields depending on the access level
    sb.append(START_LIST);
    recursiveListFields(c, level, sb);
    sb.append(END_PAREN);

    // constructors
    sb.append(NL);
    sb.append(START_LIST);
    listConstructors(c, level, sb);
    sb.append(END_PAREN);

    // methods, added recursively
    sb.append(NL);
    sb.append(START_LIST);
    recursiveListMethods(c, level, sb);
    sb.append(END_PAREN);

    // inner classes, added recursively
    sb.append(NL);
    sb.append(START_LIST);
    recursiveListInnerClasses(c, level, sb);
    sb.append(END_PAREN);
    sb.append(END_PAREN);
    sb.append(NL);
    
    return;
  }
  /**
   * Gets information on the specified class. Information is returned as a 
   * list of lists that is printed to System.out.
   *
   * @param className a <code>String</code> value
   */
  public static void getClassInfo(String className) {
    getClassInfo(className, PUBLIC);
  }
    
  /**
   * Gets information on the specified class. Information is returned as a 
   * list of lists that is printed to System.out.
   *
   * @param className a <code>String</code> value
   * @param level access level i.e. public, protected, default, and private
   */
  public static void getClassInfo(String className, int level) {
    try {
      DynamicClassLoader dcl = new DynamicClassLoader();
      Class c = dcl.loadClass(className);
      if (c != null) {
        StringBuffer sb = new StringBuffer (3000);
        sb.append(START_LIST);
        sb.append(NL);
        listClassInfo(c, level, sb);
        sb.append(END_PAREN);
        sb.append(NL);
                
        Writer out
          = new BufferedWriter(new OutputStreamWriter(System.out));
        try {
          out.write(sb.toString());
          out.flush();
        } catch (IOException e) {
        }
      }
    } catch (ClassNotFoundException e) {
      System.out.println(NIL);              
    } catch (Exception e) {
      System.out.println("(error \"Trying to load " + className +
			 " caused a Java exception: " + e + "\")");       
    } catch (NoClassDefFoundError e) {
      System.out.println(NIL);
    } catch (UnsatisfiedLinkError e) {
      // This occurs with classes that have native methods whose native
      // implementations cannot be found.
      System.out.println("(error \"Trying to load " + className +
			 " caused a Java UnsatisfiedLinkError: " + e + "\")");       
    } catch (LinkageError e) {
      System.out.println("(error \"Trying to load " + className +
			 " caused a Java LinkageError: " + e + "\")");       
    }    
  }
    
  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.
   *
   * @param className a value of type 'String'
   * @param imports   an array of imported packages
   */
  public static void getClassInfo(String className,
                                  String[]imports) {
    String name; 
    Class c;
    for (int i = 0 ; i < imports.length ; i++) {
      name = imports[i] + className;
      try {
        c = Class.forName(name);
        if (c != null) {
          getClassInfo(name);
        }
      }  catch (ClassNotFoundException e) {
        // try to find className in another package.     
      } catch (Exception e) {
	System.out.println("(error \"Trying to load " + name +
			 " caused a Java exception: " + e + "\")");       
      } catch (NoClassDefFoundError e) {
	System.out.println(NIL);
      } catch (UnsatisfiedLinkError e) {
	// This occurs with classes that have native methods whose native
	// implementations cannot be found.
	System.out.println("(error \"Trying to load " + name +
			 " caused a Java UnsatisfiedLinkError: " + e + "\")");       
      } catch (LinkageError e) {
	System.out.println("(error \"Trying to load " + name +
			 " caused a Java LinkageError: " + e + "\")");       
      }    		
    }
    System.out.println(NIL);
  }
    
  static String className(Class c) {
    if (c.isArray())
      return c.getComponentType().getName() + "[]";
    else
      return c.getName();
  }
    
  static String listClassArray(Class[] classes) {
    StringBuffer sb = new StringBuffer (100);
    for (int i = 0; i < classes.length; i++) {
      sb.append(printWithinQuotes(className(classes[i])));
      if ((i + 1) != classes.length) {
        sb.append(SPACE);
      }
    }
    return sb.toString();
  }
  public static void main (String[] args) {
    getClassInfo("java.lang.Object", 0);
    getClassInfo("java.lang.Object", 1);
    getClassInfo("java.lang.Object", 2);
    getClassInfo("java.lang.Object", 3);
  }

} // Completion

/*
 * $Log: Completion.java,v $
 * Revision 1.23  2003/07/25 04:40:54  paulk
 * Return NIL for NoClassDefFoundError.
 *
 * Revision 1.22  2003/07/25 04:26:09  paulk
 * Revert to outputting nil for class not found exception.
 *
 * Revision 1.21  2003/07/25 04:10:18  paulk
 * Trigger a Lisp error if trying to load a class causes a Java error or
 * exception.
 *
 * Revision 1.20  2002/02/21 12:23:57  jslopez
 * Rollback to  create a new instance of the
 * DynamicClassLoader all the time.
 *
 * Revision 1.19  2002/02/21 02:59:09  jslopez
 * Updates the classInfoClass to reuse a single instance of the
 * DynamicClassLoader.
 *
 * Revision 1.18  2002/02/20 12:34:39  jslopez
 * Updates getClassInfo to catch Exception.
 *
 * Revision 1.17  2002/01/31 02:52:17  jslopez
 * changes:
 * - use getFields(), etc., for all public access: saves recursion, and ensures
 * that we get the members from the interfaces, without needing to go through
 * each of them.
 * - do not do recursion for PUBLIC/PRIVATE access, or for interfaces. Saves
 * function calls, and fixes a bug that meant listing non-accessible private
 * members.
 * Thanks to Petter Måhlén for contributing these changes.
 *
 * Revision 1.16  2001/11/18 02:40:01  jslopez
 * Cleaned up imports.
 *
 * Revision 1.15  2001/10/03 05:53:42  paulk
 * Replaces calls to sb.lastIndexof(...) where sb is a StringBuffer that does
 * not define the method lastIndexOf with calls to sb.toString().lastIndexOf(...).
 *
 * Revision 1.14  2001/09/28 13:35:05  jslopez
 * Now getClassInfo retreives protected, and default package info
 * for super classes. Thanks to Petter Måhlén [petter.mahlen@chello.se]
 *
 * Revision 1.13  2001/09/09 14:30:07  jslopez
 * Removed inecessary isPublic called in several places inside listClassInfo.
 * Added more Inner class info to the listClassInfo method.
 *
 * Revision 1.12  2001/09/06 14:52:19  jslopez
 * Removing debugging comment from the completion.
 *
 * Revision 1.11  2001/09/02 03:37:04  jslopez
 * Moved all hardcoded strings to static final variables.
 * Joined the two listClassInfo methods.
 * Substituted all the System.out.print for the use of a StringBuffer. The result
 * is print out at the end of the listClassInfo method.
 *
 * Revision 1.10  2001/08/14 05:15:02  paulk
 * Miscellaneous updates.
 *
 * Revision 1.9  2001/07/21 03:56:23  paulk
 * Addional inner class support. Thanks to Javier Lopez.
 *
 * Revision 1.8  2001/07/21 03:42:24  paulk
 * Now includes inner classes in the class info list. Thanks to Javier Lopez.
 *
 * Revision 1.7  2001/07/06 02:00:09  paulk
 * Makefile
 *
 * Revision 1.6  2001/06/27 03:09:04  paulk
 * The isAccessible method now returns true for both private and protected methods
 * when the access modifier is private. Thanks to "Javier Lopez" <jlopez@cellexchange.com>.
 *
 * Revision 1.5  2000/08/10 08:46:25  paulk
 * Now outputs nil if class info not found.
 *
 * Revision 1.4  2000/08/01 07:44:49  paulk
 * Adds an isAncestorOf method. Now gets private and protected methods and fields. Thanks to Stephan Nicolas.
 *
 * Revision 1.3  2000/07/27 04:49:52  paulk
 * Now returns the type as well as name of each public field of a class. Thanks to Stephane Nicolas <s.nicolas@videotron.ca>.
 *
 * Revision 1.2  2000/02/09 04:48:41  paulk
 * Now uses Modifier.isPublic() to test whether a class's fields,
 * methods, and constructors are public and hence candidates for
 * completion. Now gets all fields and methods, not just those declared
 * by the class.
 *
 */

// End of Completion.java
