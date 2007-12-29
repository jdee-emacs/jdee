/*
 * $Id: AntServer.java,v 1.4 2002/02/15 14:05:14 jslopez Exp $
 */
/*
 *    AntServer.java
 *    Copyright (C) 2001 Javier Lopez (jslopez@alum.mit.edu)
 *
 *    $Revision: 1.4 $
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

import java.lang.ClassNotFoundException;
import java.lang.IllegalAccessException;
import java.lang.NoSuchMethodException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.StringTokenizer;

/**
 * AntServer.java provides an interface between 
 * the command line(or from Emacs) and the ant executable.
 * Calling the ant server instead of running the ant script will 
 * avoid the start up time that occur with every incovation
 * of java.exe
 *
 * Created: Sun Aug 12 21:56:50 2001
 *
 * @author <a href="mailto:jslopez@alum.mit.edu"></a>
 * @version 1.0
 * @since jde-2.2.9beta5
 */
public class AntServer {
    
    private static Class ant;
    static {
        try {
            ant = ant.forName("org.apache.tools.ant.Main");
        } catch (ClassNotFoundException e) {
            System.out.println("You need ant.jar in the beanshell classpath.");
            System.out.println("The beanshell uses jde-global-classpath or ");
            System.out.println("the environment variable CLASSPATH.");
        }
    }
    
    private static Method m;
    static {
        try {
            if (ant != null) {
                Class[] params = new Class[] {String[].class};
                m = ant.getMethod("main", params);
            }
        } catch (NoSuchMethodException e) {
        } catch (SecurityException e) {
        }
    
    }
    public static void start(String command) {
        SecurityManager sm = System.getSecurityManager();
        if (sm == null) {
            System.setSecurityManager(new JDESecurityManager());
        }
        
        //Parsing commands
        StringTokenizer st = new StringTokenizer(command);
        String[] args = new String[st.countTokens()];
        
        for (int i = 0; st.hasMoreTokens(); i++) { //Fetching the array 
            args[i] = st.nextToken();
        }
        try {
            if (m != null) {
                Object[] arguments = new Object[] {args};
                m.invoke(ant, arguments);
            }
        } catch (InvocationTargetException e) {
        } catch (IllegalAccessException e) {
        } catch (SecurityException e) {
        }
    }
    
    public static void main (String[] args) {
        AntServer.start("-buildfile c:/cygwin/home/jslopez/code/dev/build.xml");
    }
}// AntServer

/*
 * $Log: AntServer.java,v $
 * Revision 1.4  2002/02/15 14:05:14  jslopez
 * Adds an error when the ant class is not found.
 *
 * Revision 1.3  2001/11/08 12:45:10  paulk
 * Updated to support Ant.
 *
 * Revision 1.2  2001/11/05 14:10:30  jslopez
 * Modified the code to depend only indirectly
 * on ant.jar.
 *
 * Revision 1.1  2001/11/05 01:32:49  jslopez
 * Initial development.
 *
 */
