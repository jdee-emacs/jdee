/*
 *    ClassInfo.java
 *    Copyright (C) 2002, 2003 Rodrigo Reyes (reyes@chez.com),
 *                       Javier Lopez (jslopez@forumsys.com), 
 *                        Mahlen (petter.mahlen@chello.se)
 *                       Paul Kinnucan (pkinnucan@attbi.com)
 *
 *    $Revision: 1.1 $
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

import java.io.*;
import java.lang.reflect.*;
import jde.util.DynamicClassLoader;

/**
 * This class provides information about classes on jde-global-classpath.
 *
 * @author Rodrigo Reyes (reyes@chez.com),
 * @author Javier Lopez (jslopez@forumsys.com)
 * @authoMahlen (petter.mahlen@chello.se)
 * @author Paul Kinnucan (pkinnucan@attbi.com) 
 */

public class ClassInfo {

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
  public static final int PACKAGE   = 2;
    
  /**
   * Access level for a class member
   */
  public static final int PRIVATE   = 3;

  public static final int FIELD_INFO = 0;
  public static final int CTOR_INFO = 1;
  public static final int METHOD_INFO = 2;
  public static final int INNER_CLASS_INFO = 3;



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
      Class classAncestor = Class.forName( ancestor );
      Class classChild = Class.forName( child );
      if( classAncestor.isAssignableFrom( classChild ) )
          System.out.println(T);
      else
        System.out.println(NIL);
    } catch( Exception ex) {
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
    case PACKAGE: // accessible if member is not public, protected
      // or private
      return (!Modifier.isPublic   (modifiers) &&
              !Modifier.isProtected(modifiers) &&
              !Modifier.isPrivate  (modifiers));
    case PRIVATE:   // accessible if member is private
      return  Modifier.isPrivate   (modifiers);
    default:
      // cannot get here any more, since the access level is
      // only used internally
      throw new Error("Completion.isAccessible(int, int) " +
                      "called with incorrect access level parameter:"
                      + level);
    }//switch
  }

  private static String accessLevel(int modifiers) {
	String level;
	if (Modifier.isPublic(modifiers)) {
	  level = String.valueOf(PUBLIC);
	} else if (Modifier.isProtected(modifiers)) {
	  level = String.valueOf(PROTECTED);	  
	} else if (Modifier.isPrivate(modifiers)) {
	  level = String.valueOf(PRIVATE);
	} else {
	  level = String.valueOf(PACKAGE);
	}
	
	return level;
  }
	
  private static StringBuffer listModifiers(int modifiers) {
	StringBuffer sb = new StringBuffer (30);

	if (Modifier.isAbstract(modifiers)) {
	  sb.append("\"abstract\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isFinal(modifiers)) {
	  sb.append("\"final\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isInterface(modifiers)) {
	  sb.append("\"interface\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isNative(modifiers)) {
	  sb.append("\"native\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isPrivate(modifiers)) {
	  sb.append("\"private\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isProtected(modifiers)) {
	  sb.append("\"protected\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isPublic(modifiers)) {
	  sb.append("\"public\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isStatic(modifiers)) {
	  sb.append("\"static\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isStrict(modifiers)) {
	  sb.append("\"strict\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isSynchronized(modifiers)) {
	  sb.append("\"synchronized\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isTransient(modifiers)) {
	  sb.append("\"transient\"");
	  sb.append(SPACE);	  
	}

	if (Modifier.isVolatile(modifiers)) {
	  sb.append("\"volotile\"");
	  sb.append(SPACE);	  
	}

	if (sb.length() > 0) {
	  StringBuffer temp = new StringBuffer(sb.length() + 26);
	  temp.append("(cons 'typemodifiers (list ");
	  temp.append(sb);
	  temp.append("))");
	  sb = temp;
	}
	
	return sb;
	
  }

  private static StringBuffer listExceptions(Class[] classes) {
	StringBuffer sb = new StringBuffer (30);
	  	
	sb.append("(cons 'throws  (list ");
	for (int i = 0; i < classes.length; i++) {
	  sb.append(printWithinQuotes(className(classes[i])));
	  if ((i + 1) != classes.length) {
		sb.append(SPACE);
	  }
	}
	sb.append("))");

	return sb;
  }


  /**
   * Creates a field info list. The list has the following form
   *
   *  (0 name access-level type)
   *
   * where 0 indicates that this info list is for a field, name is 
   * the name of the field, access-level is one of
   * PUBLIC, PROTECTED, PACKAGE, or PRIVATE, and type is
   * the type of the field. 
   *  
   * @param field field name 
   */
  private static StringBuffer tokenizeField(Field field) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(field.getName()));
    sb.append(SPACE);
    sb.append("'variable");
    sb.append(SPACE);
    sb.append(printWithinQuotes(className(field.getType())));
    sb.append(SPACE);
	sb.append(NIL); // default value
    sb.append(SPACE);

	StringBuffer modifiers = listModifiers(field.getModifiers());
	if (modifiers.length() > 0) {
	  sb.append(START_LIST);
	  sb.append(modifiers);
	  sb.append(END_PAREN);
	} else {
	  sb.append(NIL);
	}
    sb.append(SPACE);

	sb.append(NIL); // docstring
	sb.append(END_PAREN);
        
    return sb;
  }
    
  /**
   * Prints (list "name" "params") to the system output.
   *
   * @param name constructor name
   * @param params parameter type
   */
  private static StringBuffer tokenizeCtor(Constructor ctor) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(ctor.getName()));
    sb.append(SPACE);
    sb.append("'function ");
    sb.append(listClasses(ctor.getParameterTypes()));
    sb.append(SPACE);

    // extra specifiers
    sb.append(START_LIST);
    sb.append("'(constructor . t)");
    StringBuffer temp = listModifiers(ctor.getModifiers());
    if (temp.length() > 0) {
      sb.append(SPACE);
	  sb.append(temp);
    } 
    Class[] types = ctor.getExceptionTypes();
    if (types.length > 0) {
      sb.append(SPACE);
      sb.append(listExceptions(types));
    }
    sb.append(END_PAREN);
    sb.append(SPACE);

    sb.append(NIL); // no docstring

    sb.append(END_PAREN);

    return sb;
  }

  /**
   * Prints (list "name" "returnType" "args") to the system output.
   *
   * @param name method name
   * @param returnType method return type
   * @param args method arguments
   */
  private static StringBuffer tokenizeMethod(Method method) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(method.getName()));
    sb.append(SPACE);
    sb.append("'function ");
    sb.append(SPACE);
    sb.append(listClasses(method.getParameterTypes()));
    sb.append(SPACE);

    // extra specifiers
    StringBuffer temp = listModifiers(method.getModifiers());
    Class[] types = method.getExceptionTypes();

    if (sb.length() > 0 || types.length > 0) {
      sb.append("(list");
      
      if (temp.length() > 0) {
	sb.append(SPACE);
	sb.append(temp);
      } 

      if (types.length > 0) {
	sb.append(SPACE);
	sb.append(listExceptions(types));
      }
       
    }

    sb.append(END_PAREN);
    sb.append(SPACE);

    sb.append(NIL); // no docstring

    sb.append(END_PAREN);

    return sb; 
  } 

  /**
   * Get (list INNERCLASSINFO "className" access)
   *
   * @param name className
   */
  private static StringBuffer innerClassInfo(Class ic) {
    StringBuffer sb = new StringBuffer (30);
    sb.append(START_LIST);
    sb.append(printWithinQuotes(ic.getName()));
    sb.append(SPACE);
    sb.append(String.valueOf(INNER_CLASS_INFO));
    sb.append(SPACE);
    sb.append(accessLevel(ic.getModifiers()));
    sb.append(END_PAREN);
    return sb;
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


  
 
 private static void getInheritedInnerClasses(Class c, StringBuffer sb)  {
    //This is only used while initializing
    if (c == null) {
      return;
    }
	
	
	Class[] classes = c.getDeclaredClasses();

        
    for (int index = 0; index < classes.length ; index++) {
      Class ic = classes[index];
      if (!Modifier.isPrivate(ic.getModifiers())) {
		sb.append(innerClassInfo(ic));
      }
    }

	getInheritedInnerClasses(c.getSuperclass(), sb);

  }

  private static void getInnerClasses(Class c, StringBuffer sb) {


	// Get methods declared by c.
	Class[] classes = c.getDeclaredClasses();

    for (int index = 0; index < classes.length ; index++) {
	  sb.append(innerClassInfo(classes[index]));
    }

	getInheritedInnerClasses(c.getSuperclass(), sb);

   }
        

  private static void getMemberInfo(Class c, StringBuffer sb) {
    sb.append(START_LIST);
        
	// Get fields declared by c.
	Field[] fields = c.getDeclaredFields();

    for (int index = 0; index < fields.length ; index++) {
	  sb.append(tokenizeField(fields[index]));
	}

    // constructors
    Constructor[] ctors = c.getDeclaredConstructors();
      
    for (int index = 0; index < ctors.length; index++) {
      sb.append(tokenizeCtor(ctors[index]));
	}

    // Get methods declared by c.
    Method[] methods = c.getDeclaredMethods();

    for (int index = 0; index < methods.length ; index++) {
      sb.append(tokenizeMethod(methods[index]));
    }

    // inner classes, including inherited classes
    getInnerClasses(c, sb);

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
    try {
      DynamicClassLoader dcl = new DynamicClassLoader();
      Class c = dcl.loadClass(className);
      if (c != null) {
        StringBuffer sb = new StringBuffer (3000);
        sb.append(START_LIST);
		sb.append(printWithinQuotes(className));
		sb.append(SPACE);
		sb.append("'type");
		sb.append(SPACE);
        getMemberInfo(c, sb);
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
      } catch (ClassNotFoundException cnfe) { }
		
    }
    System.out.println(NIL);
  }
    
  static String className(Class c) {
    if (c.isArray())
      return c.getComponentType().getName() + "[]";
    else
      return c.getName();
  }
    
  static StringBuffer listClasses(Class[] classes) {
    StringBuffer sb = new StringBuffer (100);
	if (classes.length > 0) {
	  	
	  sb.append(START_LIST);
	  sb.append(SPACE);
	  for (int i = 0; i < classes.length; i++) {
		sb.append(printWithinQuotes(className(classes[i])));
		if ((i + 1) != classes.length) {
		  sb.append(SPACE);
		}
	  }
	  sb.append(END_PAREN);
	} else {
	  sb.append(NIL);
	}
	
	return sb;
  }

  /**
   * Tests whether a class has a member of a specified name.
   * If so, prints t to standard out; if not, nil.
   *
   * @param className Name of class
   * @param memberName Name of member
   */
  public static void hasMember(String className, String memberName) {
    try {
      DynamicClassLoader dcl = new DynamicClassLoader();
      Class c = dcl.loadClass(className);
      if (c != null) {

	Field fields[] = c.getFields();
	boolean hasField = false;

	for (int index = 0; index < fields.length ; index++) {
	  if (fields[index].getName().equals(memberName)) {
	    hasField = true;
	    break;
	  }
	}

	Method methods[] = c.getMethods();
	boolean hasMethod = false;

	for (int index = 0; index < methods.length ; index++) {
	  if (methods[index].getName().equals(memberName)) {
	    hasMethod = true;
	    break;
	  }
	}


        StringBuffer sb = new StringBuffer (3000);
        sb.append(START_LIST);
		sb.append(hasField ? "t" : "nil");
		sb.append(hasMethod ? " t" : " nil");
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
    } catch (Exception cnfe) {System.out.println(NIL);}
  }


  public static void main (String[] args) {
    getClassInfo("java.lang.Object");
  }

} // ClassInfo

/*
 * $Log: ClassInfo.java,v $
 * Revision 1.1  2005/12/02 05:37:00  paulk
 * Provides info for files on JDE classpath.
 *
 */

// End of ClassInfo.java
