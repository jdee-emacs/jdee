/*
 *    DynamicClassLoader.java
 *    Copyright (C) 2001-2004 Javier Lopez (jslopez@forumsys.com)
 *
 *    $Revision: 1.7 $
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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.StringTokenizer;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;


/**
 * The class <code>DynamicClassLoader</code> extends the 
 * abstract class <code>ClassLoader</code>.
 * This class loads the class binaries from the file system
 * all the time, it does not catch the class information.
 * There is caveat to this, classes that come with the JDK
 * such as java.lang.*, are loaded using the standard class loader.
 * The rest of the class are always reloaded from the file system.
 *
 * Created: Sun Jul 01 08:11:12 2001
 *
 * @author <a href="mailto:jlopez@cellexchange.com"></a>
 * @version 1.0
 * @since jde2.2.8beta2
 */
public class DynamicClassLoader extends ClassLoader {
  
  /**
   * Class path.
   *
   */
  public static final String CLASS_PATH
      = System.getProperty("java.class.path");
  
  /**
   * Platform dependent file separator.
   *
   */
  public static final String FILE_SEPARATOR
      = System.getProperty("file.separator");
    
  /**
   * Platform dependent path separator. i.e. in Win32 is ';' in Unix is ':'.
   *
   */
  public static final String PATH_SEPARATOR
      = System.getProperty("path.separator"); 
    
  /**
   * Char use to separate packages. i.e. '.'
   *
   */
  public static final char PACKAGE_SEPARATOR = '.'; 
  
  /**
   * Classes file type. i.e. class
   *
   */
  public static final String CLASS_FILE_TYPE = "class";
  
  /**
   * Loads a class information from the file system,
   * if it fails it tries Class.forName(argClassName)
   *
   * @param argClassName name of the class to be loaded.
   * @return Class of the type argClassName
   * @exception ClassNotFoundException if the class cannot be found.
   */
  public Class loadClass(String argClassName) throws ClassNotFoundException {
    File file;
    byte[] classBytes = null;
    Class c;

    //Checking if the class belong to either java.* or javax.*
    if ((argClassName.startsWith("java.")) || 
        (argClassName.startsWith("javax."))) {
      return Class.forName(argClassName);
    } // end of if ()
    
    //First convert the class name from java.lang.String to java/lang/String
    //where '/' is platform dependent.
    String className = argClassName.replace(PACKAGE_SEPARATOR,
                                            FILE_SEPARATOR.charAt(0));

    //Then add the class file termination i.e. from java/lang/String
    //to java/lang/String.class
    className += PACKAGE_SEPARATOR + CLASS_FILE_TYPE;

    //Look for the class file in the current project classfile or in
    //the system class path if there is no current classpath
    ProjectClasses pc = JdeUtilities.getCurrentProjectClass();
    String classpath = null;
    if (pc != null) {
        classpath = pc.getClassPath();
    } // end of if (pc != null)
    
    if (classpath == null || classpath.equals("")) {
        classpath = CLASS_PATH;
    } // end of if (classpath == null )
    
    StringTokenizer st = new StringTokenizer(classpath, PATH_SEPARATOR);
    ZipFile zf;
    while (st.hasMoreTokens()) {
      file = new File(st.nextToken());  

      //Check if the file is a directory if is not 
      //assume it is a jar or zip file
      try {
        if (file.isDirectory()) {
          //if the file is a directory try to locate the class file 
          //and load it.
          file = new File(file, className);
          classBytes = loadFile(file);
          if (classBytes != null) {
            break;
          } // end of if (classBytes != null)
        } else {
          zf = new ZipFile(file);
          classBytes = loadFile(zf, className);
          if (classBytes != null) {
            break;
          } // end of if (classBytes != null)
        }// end of if-else
      } catch (IOException e) {
        //ignore
      } // end of try-catch
    } // end of while (st.hasMoreTokens())
    
    if (classBytes != null) {
      try {
        c = defineClass(argClassName, classBytes, 0, classBytes.length);
      } catch (SecurityException e) {
        //basic packages such as java.lang.* can't be loaded directly
        c = Class.forName(argClassName);
      } catch (ClassFormatError e) { 
        c = Class.forName(argClassName);
      } catch (NoClassDefFoundError e) {
        c = Class.forName(argClassName);
      }
      return c;
    } else {
      try {
        return Class.forName(argClassName);
      } catch (ClassNotFoundException e) {
        throw new ClassNotFoundException(argClassName);
      } // end of try-catch
    } // end of else
  }//end of loadClass

  private byte[] loadFile(File argFile) {
    byte[] b = null;
    InputStream in = null;
    if (argFile.exists()) {
      try {
        in = new FileInputStream(argFile);
        b = read(in, (int)argFile.length());
      } catch (FileNotFoundException e) {
        b = null;
      } catch (IOException e) {
        b = null;
      } finally {
        try {
          in.close();
        } catch (IOException e) {
        } // end of try-catch
      }//end of try-finally
    }// end of if
    
    return b;
  }

  private byte[] loadFile(ZipFile argFile, String argClassName) {
    //zip and jar files seems to always be separated by a '/'
    argClassName = argClassName.replace(FILE_SEPARATOR.charAt(0), '/');
    byte[] b = null;
    ZipEntry ze;
    InputStream in;
    try {
      ze = argFile.getEntry(argClassName);
      if (ze != null) {
        in = argFile.getInputStream(ze);
        b = read(in, (int) ze.getSize());
      }
    } catch (IOException e) {
      b = null;
    } finally {
      try {
        argFile.close();
      } catch (IOException e) {
      } 
    }
    return b;
  }

  private static byte[] read (InputStream is, int size) throws IOException {
    int len = 0;
    byte [] b = new byte[size];
    try {
      while (true) {
        int n = is.read(b, len, size - len);
        if (n == -1 || n == 0) {
          if (len < size) {
                    // ignore
          }
          break;
        } else
          len += n;
      }
    } finally {
      try {
        is.close();
      } catch (IOException e) {} // ignore
    }        
    return b;
  }

}// DynamicClassLoader

/*
 * $Log: DynamicClassLoader.java,v $
 * Revision 1.7  2004/10/15 03:28:09  paulk
 * Fixed bug caused by assuming that only one read operation is needed to
 * read a class from the file system. Thanks to Suraj Acharya.
 *
 * Revision 1.6  2002/02/21 12:26:45  jslopez
 * Updates the DynamicClassLoader to use the current project
 * classpath stored in JdeUtilities.
 *
 * Revision 1.5  2001/07/21 04:01:21  paulk
 * Now loads classes in java and javax packages instead of waiting for an exception to be thrown The purpose is two-fold, one is performance and the other one is to get rid of a nasty LinkageError. Contributed by Javier Lopez.
 *
 * Revision 1.4  2001/07/18 01:51:37  paulk
 * Handles ClassFormatError and NoClassDefFoundErrror exceptions that can occur when trying to load classes. Thanks to Javier Lopez.
 *
 * Revision 1.3  2001/07/17 05:27:53  paulk
 * Fixed to search vm startup classpath if classes not found on user classpath. Thanks to Javier.
 *
 * Revision 1.2  2001/07/07 04:49:43  paulk
 * Removed DOS line endings.
 *
 * Revision 1.1  2001/07/06 01:59:02  paulk
 * Initial revision.
 *
 */

// End of DynamicClassLoader.java
