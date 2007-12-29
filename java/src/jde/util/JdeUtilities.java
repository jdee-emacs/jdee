/*
 *    JdeUtilities.java
 *    $Revision: 1.13.2.1 $
 *
 *    Copyright (C) 1999-2004, 2006 Len Trigg (trigg@cs.waikato.ac.nz)
 *    Copyright (C) 1999-2002 Paul Kinnucan (paulk@mathworks.com)
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
import java.util.*;

/**
 * This class provides various utility methods.
 *
 * @author Len Trigg (trigg@cs.waikato.ac.nz) 
 * @author Paul Kinnucan (paulk@mathworks.com)
 * @author Matt Conway (Matt_Conway@i2.com )
 * @author Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
 * @version $Revision: 1.13.2.1 $
 */
public class JdeUtilities {

  /** A cache of the items that are important across projects,
   * indexed by the project name */
  private static Map projectCache = new HashMap();

  /** The current project so that callers need not pass in the
      project name every time.  This is convenient, but not
      threadsafe.  If we need thread saftey, we can go change all
      the emacs lisp callers to always pass in the project name */
  private static String currentProjectName = "default";
    
  // Have a default one just in case
  static {
    try {
      ProjectClasses defaultProject =
        new ProjectClasses(System.getProperty("java.class.path"));
      projectCache.put(currentProjectName,defaultProject);
    } catch (IOException e) {
      e.printStackTrace(System.err);
    } // end of try-catch
  }

  /*************************************************************************
   * Constants
   *************************************************************************/
  public static final String NIL = "nil";
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
   * Jde should call this everytime the project changes, or if the
   * classpath needs to be updated.
   *
   * @param projectName a <code>String</code> value
   * @param projectClassPath a <code>String</code> value
   */
  public static void setProjectValues(String projectName,
                                      String projectClassPath) {
    try {
      currentProjectName = projectName;
      ProjectClasses pc = new ProjectClasses(projectClassPath);
      projectCache.put(projectName, pc);
    } catch (IOException e) {
      e.printStackTrace(System.err);
    } // end of try-catch
  }

  /* Convenience to get current project's name */
  public static String getCurrentProjectName() {
    return currentProjectName;
  }
    
  public static ProjectClasses getCurrentProjectClass() {
    return (ProjectClasses) projectCache.get(currentProjectName);
  }

  /**
   * Determine whether a class exists on the JDEE classpath.
   *
   * @param fqn Fully qualified name of the class to be found.
   */
  public static void classExists(String fqn) {
    DynamicClassLoader dcl = new DynamicClassLoader();
    while (fqn != null) {
      try {
        dcl.loadClass( fqn );
        System.out.println( "\"" + fqn + "\"");
        return;
      } catch (ClassNotFoundException e) {
      } catch (NoClassDefFoundError e) {
      } catch (Exception e) {
        System.out.println("(error \"Trying to load " + fqn +
                           " caused a Java exception: " + e + "\")");
        return;
      } catch (UnsatisfiedLinkError e) {
        // This occurs with classes that have native methods whose native
        // implementations cannot be found.
        System.out.println("(error \"Trying to load " + fqn +
                           " caused a Java UnsatisfiedLinkError: " + e + "\")");
        return;
      } catch (LinkageError e) {
        System.out.println("(error \"Trying to load " + fqn +
                           " caused a Java LinkageError: " + e + "\")");
        return;
      }
      // Test whether fqn is an inner class.
      int index = fqn.lastIndexOf('.');
      if (index != -1) {
        fqn = fqn.substring (0, index) + '$' + fqn.substring (index + 1);
      } else {
        fqn = null;
      }
    }
    System.out.println(NIL);              
  }//met


  /**
   * Reload classes contained by a class, jar, or zip file or
   * the current project's classpath.
   *
   * @param classPathEntry path to be reloaded. If null, this method
   * reloads the current project's classpath.
   */
  public static void updateClassList(String classPathEntry) {
    ProjectClasses pc =
      (ProjectClasses) projectCache.get(getCurrentProjectName());

    try {
      if (classPathEntry!=null) {
        pc.reloadClasses(classPathEntry);
      } else {
        pc.reloadClasses();
      } // end of if (classPathEntry!=null)
    } catch (IOException e) {
      e.printStackTrace(System.err);
    } // end of try-catch
  }

  /**
   * Reload the current project's classpath.
   */
  public static void updateClassList() {
    updateClassList(null);
  }
 
  /**
   * Looks up an unqualified class name in the class path to find possible
   * fully qualified matches.  Given `List,' this will find
   * `java.util.List' and `java.awt.List'
   *
   * @param className a value of type 'String'
   */
  public static void getQualifiedName(String className) {
    ProjectClasses pc = null;
    StringBuffer result = null;

    try {
      pc = (ProjectClasses)projectCache.get(currentProjectName);
      result = new StringBuffer(START_PAREN);
      result.append(LIST);

      for (Iterator i = pc.getClassNames(className).iterator();
           i.hasNext();) {
        result.append(SPACE);
        result.append(DOUBLE_QUOTE);
        result.append(i.next().toString());
        result.append(DOUBLE_QUOTE);
      }
      result.append(END_PAREN);
      System.out.println(result.toString());
      System.out.flush();
    } catch (IOException e) {
      e.printStackTrace(System.err);
    } // end of try-catch
  }

  public static void getJavaVersion() {
    StringBuffer sb = new StringBuffer(30);
    sb.append(DOUBLE_QUOTE);
    sb.append(System.getProperty("java.version"));
    sb.append(DOUBLE_QUOTE);
    System.out.println(sb);
    System.out.flush();
  }

  public static void exit() {
    System.exit(7);
  }
} // JdeUtilities

/*
 * $Log: JdeUtilities.java,v $
 * Revision 1.13.2.1  2006/03/01 05:02:57  paulk
 * Fix classExists() to handle inner classes. Thanks to Suraj Acharya [sacharya@gmail.com].
 *
 * Revision 1.13  2004/11/16 05:47:14  paulk
 * restore updateClassList() method. Thanks to Martin Schwamberger.
 *
 * Revision 1.12  2004/10/20 06:14:53  paulk
 * Update to support reloading classes from a single classpath entry. Thanks to Martin Schwamberger.
 *
 * Revision 1.11  2003/07/25 04:39:46  paulk
 * More precisely catch exceptions and errors in classExists() method.
 *
 * Revision 1.10  2002/02/21 12:25:40  jslopez
 * Adds method getCurrentClassPath.
 * Update method classExists to use the DynamicClassLoader.
 *
 * Revision 1.9  2001/11/05 02:03:17  jslopez
 * Adds an exit method.
 *
 * Revision 1.8  2001/09/13 02:35:40  eric
 * (comment copied from branch)
 * java 2-only implementation of JdeUtilities functions.  This uses a lazy
 * map based lookup scheme instead of a linear search of class lists to find
 * fully qualified class names.  It also implements a flyweight/singleton
 * approach to sharing classpath entries that appear in multiple projects.
 * Finally, classpath entries that are part of the `system' classes are
 * wrapped in an immutable decorator which balks are reloading them.
 * This eliminates the overhead of rescanning classes which cannot change
 * over the lifetime of a single bsh process.
 *
 * Revision 1.6.2.2  2001/09/13 02:32:06  eric
 * merge HEAD => branch, in anticipation of merging branch => HEAD
 *
 */

// End of JdeUtilities.java
