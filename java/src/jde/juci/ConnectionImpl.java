/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.3 $
 * $Date: 2003/02/18 05:33:08 $
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

import java.io.*;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import bsh.Interpreter;
import bsh.NameSpace;
import bsh.This;

/**
 * Implementation of the JUCI <code>Connection</code> interface.  Implements
 * the two-way communication via a separate thread that invokes java code
 * called from Emacs.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class ConnectionImpl implements InvocationHandler, Connection {

  private This global;
  private Map  impls;

  private Map cachedImplInstances  = new HashMap();

  private PrintWriter output       = new PrintWriter(System.out);
  private LispWriter  lispWriter   = new LispWriter(output);
  private LinkedList  resultStack  = new LinkedList();
  private int         beginCalls   = 0;
  private String      variableName = "juciConn";

  private ThreadedInvoker invoker;
  private boolean threaded = true; // set to false for some unit tests

  private Object monitor = new Object();

  // Debugging
  private String loggerFilename;
  private Logger logger = new Logger();

  private static final Object RESULT_PENDING = new Object() {
      public String toString() { return "RESULT_PENDING"; }
    };

  // instance initializer
  {
    invoker = new ThreadedInvoker();
    invoker.start();
    Thread.yield();             // allow invoker to reach wait()
  }

  /**
   * Creates a new <code>ConnectionImpl</code> instance.
   */
  public ConnectionImpl() {
  }

  /**
   * Creates a new <code>ConnectionImpl</code> instance.
   */
  public ConnectionImpl(This global, Map impls, String variableName) {
    this.global       = global;
    this.impls        = impls;
    this.variableName = variableName;
  }

  void addSupportedInterfaces(Map newImpls) {
    impls.putAll(newImpls);
  }

  /**
   * Set the output writer where lisp forms are written.
   *
   * @param writer a <code>PrintWriter</code> value
   */
  public void setOutput(PrintWriter writer) {
    this.output     = writer;
    this.lispWriter = new LispWriter(output);
  }

  void setThreaded(boolean threaded) {
    this.threaded = threaded;
  }

  /**
   * Implementation of dynamic proxy method invocation handing.  All elisp
   * code calling java and java code calling elisp eventually passes through
   * this method.
   *
   * @param object the proxy object being invoked
   * @param method the method to invoke
   * @param args   method arguments
   * @return       result of method invocation
   * @exception Throwable if an error occurs
   */
  public Object invoke(Object object, Method method, Object[] args) throws Throwable {
    try {
      Object result = null;

      if (Connection.class == method.getDeclaringClass()) {
        if (method.getName().equals("begin")) {
          begin();
        } else if (method.getName().equals("end")) {
          end();
        } else if (method.getName().equals("reset")) {
          reset();
        } else if (method.getName().equals("popResult")) {
          result = popResult();
        } else if (method.getName().equals("pushResult")) {
          pushResult(args[0]);
        } else if (method.getName().equals("setLoggerFilename")) {
          setLoggerFilename((String) args[0]);
        } else if (method.getName().equals("evalBshScript")) {
          evalBshScript((String) args[0]);
        }
      } else if (Object.class == method.getDeclaringClass()) {
        method.invoke(this, args);
      } else {
        result = invokeOther(method, args);
      }

      return result;
    } catch (Throwable t) {
      logger.error("Error during invoke", t);
      throw t;
    }
  }

  private Class getImplClass(Method method) {
    Class declaringClass = method.getDeclaringClass();
    Class implClass      = null;

    if (impls.containsKey(declaringClass)) {
      implClass = (Class) impls.get(declaringClass);
    } else if (impls.values().contains(declaringClass)) {
      implClass = declaringClass;
    }

    return implClass;
  }

  private Object invokeJava(Class implClass, Method method, Object[] args) throws Throwable {

    Object instance = cachedImplInstances.get(implClass);

    if (instance == null) {
      instance = implClass.newInstance();
      cachedImplInstances.put(implClass, instance);
    }

    maybeConvertMaps(args);

    invoker.addInvocation(new ReflectionInvocation(instance, method, args));

    return finishInvoke();
  }

  private Object invokeElisp(Class implClass, Method method, Object[] args)
    throws ElispError {

    logger.debug(method.toString());

    List eval = new ArrayList();
    eval.add(new Symbol("jde-juci-invoke-elisp"));

    List form = new ArrayList();
    form.add(new Symbol(Symbol.java2Elisp(method.getDeclaringClass().getName() + "." + method.getName())));

    if (args != null && args.length > 0) {
      form.addAll(Arrays.asList(args));
    }

    eval.add(form);
    lispWriter.writeForm(eval);
    output.println();
    output.flush();
    pushResult(RESULT_PENDING);

    waitForResult();

    Object result = popResult();
    if (result instanceof ElispError) {
      throw (ElispError) result;
    } else {
      return result;
    }
  }

  private Object invokeOther(Method method, Object[] args) throws Throwable {
    Class implClass = getImplClass(method);

    if (implClass != null) {
      return invokeJava(implClass, method, args);
    } else {
      return invokeElisp(implClass, method, args);
    }
  }

  // wait for a result on an inbound Java call and return it
  private Object finishInvoke() throws Throwable {
    waitForResult();

    Object result = peekResult();
    if (result == RESULT_PENDING) {
      popResult();
      result = null;
    } else if (beginCalls == 0) {
      result = popResult();
    } else {
      // If result isn't pending, it will be written out during end; don't
      // write it out now
      result = null;
    }

    // Always write a result back to Emacs
    writeResult(result != null ? result : LispWriter.NIL);
    return result;
  }

  // Check for lists of Cons objects and automatically convert them to Maps
  private void maybeConvertMaps(Object[] args) {
    for (int i = 0; i < args.length; i++) {

      if (args[i] instanceof List) {

        List l = (List) args[i];
        boolean allCons = true;

        for (int j = 0; j < l.size() && allCons; j++) {
          if (! (l.get(j) instanceof Cons)) {
            allCons = false;
          }
        }

        if (allCons) {

          Map m = new HashMap();

          for (int j = 0; j < l.size(); j++) {
            Cons cons = (Cons) l.get(j);
            m.put(cons.getCar(), cons.getCdr());
          }

          args[i] = m;
        }
      }
    }
  }

  private void waitForResult() {
    if (threaded) {
      try {
        synchronized (monitor) {
          logger.debug("About to wait");
          monitor.wait();
          logger.debug("woke up");
          Thread.yield();
        }
      } catch (InterruptedException ie) {
        logger.warning("caught interrupt");
        Thread.currentThread().interrupt();
      }
    }
  }

  private void notifyResultReady() {
    if (threaded) {
      // Notify other thread waiting for result
      synchronized (monitor) {
        logger.debug("About to notify");
        monitor.notify();
      }
    }
  }

  public void begin() {
    beginCalls++;
    logger.debug("*** begin " + beginCalls);
  }

  public void end() {
    if (--beginCalls <= 0) {

      synchronized (resultStack) {
        if (resultStack.isEmpty()) {
          writeResult(LispWriter.NIL);
        } else {
          while (!resultStack.isEmpty()) {
            writeResult(resultStack.removeLast());
          }
        }
      }

      if (global != null) {
        try {
          global.getNameSpace().setVariable(variableName, null);
        } catch (bsh.EvalError ee) {
          // All we're doing is attempting to unset the variable here, so if
          // we fail at that, the consequences are probably harmless...
        }
      }

      synchronized (ConnectionFactory.connectionLock) {
        ConnectionFactory.currentConnection = null;
      }
    }
    logger.debug("*** end " + beginCalls);
  }

  public void reset() {
    beginCalls = 0;
    resultStack.clear();
    logger.debug("*** reset");
  }

  public void pushResult(Object result) {
    logger.debug("pushing: " + result + (result != null ? " " + result.getClass() : ""));
    synchronized (resultStack) {
      resultStack.addLast(result);
    }
    notifyResultReady();
  }

  public Object popResult() {
    synchronized (resultStack) {
      if (resultStack.isEmpty()) {
        throw new NoMoreResultsException();
      }
      return resultStack.removeLast();
    }
  }

  public Object peekResult() {
    synchronized (resultStack) {
      if (resultStack.isEmpty()) {
        throw new NoMoreResultsException();
      }
      return resultStack.getLast();
    }
  }

  public void setLoggerFilename(String filename) {
    if (loggerFilename == null || !loggerFilename.equals(filename)) {
      try {
        loggerFilename = filename;
        logger         = new Logger(new FileWriter(filename, true));
      } catch (IOException io) {
        // whoops, no logger
      }
    }
  }

  public void evalBshScript(final String statement) throws Throwable {
    // TODO: should do some form of maybeConvertMaps here
    invoker.addInvocation(new BshInvocation(statement));
    finishInvoke();
  }

  private void writeResult(Object result) {
    if (result instanceof ExceptionResult) {
      throw new WrappedRuntimeException(((ExceptionResult) result).exception);
    }

    logger.debug("writeResult", result);
    lispWriter.writeUnknown(result);
    output.println();
    output.flush();
  }

  private static class ExceptionResult {

    Throwable exception;

    ExceptionResult(Throwable t) {
      exception = t;
    }

    public String toString() {
      StringWriter swr = new StringWriter();
      exception.printStackTrace(new PrintWriter(swr));
      return "ExceptionResult: " + swr.toString();
    }
  }

  private interface Invocation {
    Object invoke();
  }

  private class ReflectionInvocation implements Invocation {

    private Object invokeOn;
    private Method method;
    private Object[] args;

    private ReflectionInvocation(Object o, Method m, Object[] arr) {
      invokeOn = o;
      method   = m;
      args     = arr;
    }

    public Object invoke() {
      Object result = null;
      try {
        logger.debug("Invoking " + method);
        result = method.invoke(invokeOn, args);
      } catch (InvocationTargetException ite) {
        result = new ExceptionResult(ite.getTargetException());
      } catch (IllegalAccessException iae) {
        result = new ExceptionResult(iae);
      }

      return result;
    }
  }

  private class BshInvocation implements Invocation {
    private String statement;
    private BshInvocation(String statement) {
      this.statement = statement;
    }
    public Object invoke() {
      PrintStream writer = new PrintStream(new ByteArrayOutputStream());
      Interpreter bsh    = new Interpreter(new StringReader(""), writer, writer, false, global.getNameSpace());
      Object      result = null;
      try {
        logger.debug("Evaluating " + statement);
        result = bsh.eval(statement);
      } catch (bsh.EvalError e) {
        result = new ExceptionResult(e);
      }
      return result;
    }
  }

  private class ThreadedInvoker extends Thread {

    private LinkedList invocations = new LinkedList();

    public ThreadedInvoker() {
      super("ConnectionImpl threaded invoker");
    }

    public void run() {
      for (;;) {
        try {
          Invocation inv = null;

          try {
            synchronized (invocations) {
              inv = (Invocation) invocations.removeFirst();
            }
          } catch (NoSuchElementException nsee) {}

          if (inv == null) {
            synchronized (this) {
              logger.debug("Waiting for invocation");
              wait();
              logger.debug("Woke up");
              Thread.yield();
            }
          } else {
            Object result = inv.invoke();
            pushResult(result);
          }
        } catch (InterruptedException ie) {
          // keep going
        }
      }
    }

    public void addInvocation(Invocation inv) {
      synchronized (invocations) {
        invocations.addLast(inv);
      }

      synchronized (this) {
        logger.debug("Notifying of new invocation");
        notify();
      }
    }

  }
}

// ConnectionImpl.java ends here
