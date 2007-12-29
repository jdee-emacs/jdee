/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.2 $
 * $Date: 2003/02/17 06:10:30 $
 *
 * Author: Nick Sieger <nsieger@bitstream.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package jde.juci;

import java.lang.reflect.Proxy;
import java.util.*;

import bsh.This;

/**
 * Used to obtain a JUCI connection used to communicate across the
 * Elisp/Java boundary.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class ConnectionFactory {
  /**
   * Not meant to be instantiated.
   */
  private ConnectionFactory() {}

  static final Object connectionLock = new Object();

  static ConnectionImpl currentConnection;

  static boolean unitTesting = false;

  /**
   * Get a connection proxy that implements both {@link Connection} and the
   * given interface class.  To be used by elisp code executing java from
   * emacs.
   *
   * @param proxyInterface a <code>Class</code> value
   * @param global a <code>This</code> value
   * @param connVarName a <code>String</code> value
   * @return a <code>Connection</code> value
   * @exception ConnectionUnavailableException if an error occurs
   */
  public static Connection getConnection(Class proxyInterface, This global, String connVarName) {
    Class[] arr = null;
    if (proxyInterface != null) {
      arr = new Class[] {proxyInterface};
    } else {
      arr = new Class[0];
    }
    return getConnection(arr, global, connVarName);
  }

  /**
   * Get a connection proxy that implements both {@link Connection} and the
   * given interface classes.  To be used by elisp code executing java from
   * emacs.
   *
   * @param proxyInterfaces a <code>Class[]</code> value
   * @param global a <code>This</code> value
   * @param connVarName a <code>String</code> value
   * @return a <code>Connection</code> value
   * @exception ConnectionUnavailableException if an error occurs
   */
  public static Connection getConnection(Class[] proxyInterfaces, This global, String connVarName) {

    for (int i = 0; i < proxyInterfaces.length; i++) {
      assertInterface(proxyInterfaces[i], false);
    }

    Set ifcs  = new HashSet();
    Map impls = new HashMap();

    for (int i = 0; i < proxyInterfaces.length; i++) {
      addInterfaces(ifcs, impls, proxyInterfaces[i], proxyInterfaces[i]);
    }

    addInterfaces(ifcs, impls, Connection.class, null);

    return (Connection) Proxy.newProxyInstance(getClassLoader(),
                                               (Class[]) ifcs.toArray(new Class[ifcs.size()]),
                                               getCurrentImpl(global, impls, connVarName, true));
  }

  /**
   * Get a connection proxy that implements both {@link Connection} and the
   * given interface class.  To be used by java code executing inside the
   * beanshell that needs to invoke emacs.
   *
   * @param proxyInterface a <code>Class</code> value
   * @return a <code>Connection</code> value
   * @exception ConnectionUnavailableException if an error occurs
   */
  public static Connection getConnection(Class proxyInterface) {

    assertInterface(proxyInterface, true);

    Set ifcs  = new HashSet();
    Map impls = new HashMap();

    addInterfaces(ifcs, impls, proxyInterface, proxyInterface);
    addInterfaces(ifcs, impls, Connection.class, null);
    return (Connection) Proxy.newProxyInstance(getClassLoader(),
                                               (Class[]) ifcs.toArray(new Class[ifcs.size()]),
                                               getCurrentImpl(null, impls, null, false));
  }

  /**
   * Get a connection proxy that implements both {@link Connection} and the
   * given interface classes.  To be used by java code executing inside the
   * beanshell that needs to invoke emacs.
   *
   * @param proxyInterfaces a <code>Class[]</code> value
   * @return a <code>Connection</code> value
   * @exception ConnectionUnavailableException if an error occurs
   */
  public static Connection getConnection(Class[] proxyInterfaces) {

    for (int i = 0; i < proxyInterfaces.length; i++) {
      assertInterface(proxyInterfaces[i], true);
    }

    Set ifcs  = new HashSet();
    Map impls = new HashMap();

    for (int i = 0; i < proxyInterfaces.length; i++) {
      addInterfaces(ifcs, impls, proxyInterfaces[i], proxyInterfaces[i]);
    }

    addInterfaces(ifcs, impls, Connection.class, null);
    return (Connection) Proxy.newProxyInstance(getClassLoader(),
                                               (Class[]) ifcs.toArray(new Class[ifcs.size()]),
                                               getCurrentImpl(null, impls, null, false));
  }

  /**
   * TODO: provide a declarative way for beanshell command scripts to call
   * back to Elisp.  Do it through this factory method, which will return a
   * special bsh.This object with an overloaded namespace that has the same
   * methods that are in the incoming object.  But when these methods are
   * invoked, they actually do the printing of the arguments to stdout as
   * lisp forms just like the invokeElisp method of ConnectionImpl does.
   * More research to be done on this approach.
   *
   * @param bshObject a <code>This</code> value
   * @return a <code>This</code> value
   */
  public static This getConnection(This bshObject) {
    return null;
  }

  private static void addInterfaces(Set ifaces, Map impls, Class proxyClass, Class implClass) {
    if (proxyClass.isInterface()) {
      // add the java-to-elisp bridge interface
      ifaces.add(proxyClass);
      if (!impls.containsKey(proxyClass)) {
        impls.put(proxyClass, null);
      }
    } else {
      // add the java implementation bridge -- add all of its interfaces and
      // add it to the implementation map
      Class[] cls = proxyClass.getInterfaces();

      for (int i = 0; i < cls.length; i++) {
        ifaces.add(cls[i]);
        impls.put(cls[i], implClass);
      }

      // superclass interfaces too
      Class superclass = proxyClass.getSuperclass();

      if (superclass != null && superclass != Object.class) {
        addInterfaces(ifaces, impls, superclass, implClass);
      }
    }
  }

  private static ConnectionImpl getCurrentImpl(This global, Map impls, String connVarName, boolean create) {
    ConnectionImpl connimpl = null;
    synchronized (connectionLock) {
      if (currentConnection == null) {
        if (create || unitTesting) {
          currentConnection = new ConnectionImpl(global, impls, connVarName);
        } else {
          throw new ConnectionUnavailableException();
        }
      } else {
        currentConnection.addSupportedInterfaces(impls);
      }
      connimpl = currentConnection;
    }
    return connimpl;
  }

  private static ClassLoader getClassLoader() {
    ClassLoader loader = Thread.currentThread().getContextClassLoader();

    if (loader == null) {
      loader = ConnectionFactory.class.getClassLoader();
    }
    if (loader == null) {
      loader = ClassLoader.getSystemClassLoader();
    }
    return loader;
  }

  private static void assertInterface(Class ifc, boolean shouldBeInterface) {
    if (ifc.isInterface() != shouldBeInterface) {
      throw new ConnectionUnavailableException("Wrong connection type for " + ifc + ": should only call " +
                                               (shouldBeInterface ?
                                                "interfaces from java code" :
                                                "implementations from elisp code"));
    }
  }

  /**
   * Package-local method for unit tests to be able to circumvent some
   * protective checks in order to test the smallest possible units.
   *
   * @param flag whether unit testing is on or off
   */
  static void setUnitTesting(boolean flag) {
    unitTesting = flag;
  }

}

// ConnectionFactory.java ends here
