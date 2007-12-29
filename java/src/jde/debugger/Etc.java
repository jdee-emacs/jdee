package jde.debugger;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import com.sun.jdi.*;
import com.sun.jdi.request.*;
import jde.debugger.expr.*;


/**
 * Etc.java
 * <p>
 * random useful utilities
 * <p>
 * Created: Thu Jul  8 13:01:24 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class Etc  {

  /**
   * dump a particular object, in whatever way seems appropriate
   * @param obj The object to dump
   */
  public static void dump(Object obj) {
    if (obj instanceof Collection) {
      Iterator it = ((Collection)obj).iterator();
      while (it.hasNext()) {
        System.out.println(it.next());
      }
    } else if (obj instanceof Object[]) {
      Object[] k = (Object[])obj;
      for (int i=0; i<k.length; i++)
        dump(k[i]);
    } else {
      System.out.println(obj);
    }
  }


  /**
   * Safely convert to a Long, raising an appropriate
   * JDENumberFormatException if required
   *
   * @param obj The object to convert. The .toString() is used.
   * @param type The type of long: used in the exception string if required
   * @return the Long, or raises exception.
   */
  public static Long safeGetLong(Object obj, String type)
    throws JDEException {
    try {
      return new Long(obj.toString());
    } catch (NumberFormatException ex) {
      throw new JDENumberFormatException(type);
    }
  }


  /**
   * Safely convert to an int, raising an appropriate
   * JDENumberFormatException if required
   *
   * @param obj The object to convert. The .toString() is used.
   * @param type The type of int: used in the exception string if required
   * @return the Integer, or raises exception.
   */
  public static int safeGetint(Object obj, String type)
    throws JDEException {
    try {
      return Integer.parseInt(obj.toString());
    } catch (NumberFormatException ex) {
      throw new JDENumberFormatException(type);
    }
  }


  /**
   * Evaluate an expression, given a context
   * <p>
   * @param expr The expression to evaluate
   * @param frame The stackframe that defines the context
   * @return a {@link Rep#getValueRep(Value) value}
   */
  public static Value evaluate(String expr, final StackFrame frame)
    throws JDEException {
    //	System.out.println(expr);
    try {
      ExpressionParser.GetFrame frameGetter = null;
      frameGetter = new ExpressionParser.GetFrame() {
          public StackFrame get()
            throws IncompatibleThreadStateException {
            return frame;
          }
        };
      return ExpressionParser.evaluate(expr, frame.virtualMachine(),
                                       frameGetter);
    } catch (NativeMethodException ex) {
      throw new JDEException("Can't access local variables in native methods");
    } catch (ParseException ex) {
      throw new JDEException(ex.toString());
    } catch (InvocationException ex) {
      throw new JDEException("Exception in expression: "+ex.exception().referenceType().name());
    } catch (InvalidTypeException ite) {
      throw new JDEException("Expression contains invalid type");
    } catch (IncompatibleThreadStateException itse) {
      throw new JDEException("This expression cannot be evaluated at an arbitrary location");
    } catch (ClassNotLoadedException tnle) {
      throw new JDEException("A required class for the evaluation hasn't been loaded");
    }
  }

    
  /**
   * Parses the list of arguments for thread information.
   * <pre>
   * on_thread_id   threadID
   * on_thread_name "threadName"
   * </pre>
   * Note that the exception is <b>not</b> raised if the tags are
   * not present: only if what follows the tag is incorrect.
   * <p>
   * Commands having this argument will raise events only if the event
   * thread matches the specification. More details in EventHandler.
   * <p>
   * Shortcuts: <u>-tid</u> and <u>-tname</u>
   * <p>
   * @return a Long corresponding to the threadID, or the string
   * "threadName"
   * @exception JDEException If the information cannot be resolved
   */
  public static Object getThreadFromArgs(List args)
    throws JDEException {

    String threadArg = null;
    int threadIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("on_thread_id") || arg.equals("on_thread_name")
          || arg.equals("-tid") || arg.equals("-tname")) {
        threadIndex = i;
        threadArg = args.remove(threadIndex).toString();
        break;
      }
    }
    // at this point, either threadArg = null, or on_thread_id/name. in
    // that case, threadIndex should now have the index of the argument
    if (threadArg == null) {
      return null;
    } else {
      if (threadArg.equals("on_thread_id")
          || threadArg.equals("-tid")) {
        if (threadIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'on_thread_id'");
        } else {
          try {
            return new Long(args.remove(threadIndex).toString());
          } catch (NumberFormatException ex) {
            throw new JDENumberFormatException("'on_thread_id' argument");
          }
        }
      } else if (threadArg.equals("on_thread_name")
                 || threadArg.equals("-tname")) {
        if (threadIndex == args.size()) {
          throw new JDEException("Missing argument to 'on_thread_name'");
        } else {
          return args.remove(threadIndex).toString();
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }

    
  /**
   * Parses the list of arguments for expression information.
   * <pre>
   * if "expression"
   * </pre>
   * Note that the exception is <b>not</b> raised if the tag isn't
   * present: only if what follows the tag is incorrect.
   * <p>
   * When used, the expression is evaluated at the time of the event,
   * and the event passed to jde if the expression evaluates to "true".
   * <p>
   * Shortcut: <u>-e</u>
   * <p>
   * @return string corresponding to the expression
   * @exception JDEException If the information cannot be resolved
   */
  public static String getExprFromArgs(List args)
    throws JDEException {
	
    String exprArg = null;
    int exprIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("if") || arg.equals("-e")) { 
        exprIndex = i;
        exprArg = args.remove(exprIndex).toString();
        break;
      }
    }
    // at this point, either exprArg = null, or "if". in
    // that case, exprIndex should now have the index of the argument
    if (exprArg == null) {
      return null;
    } else {
      if (exprArg.equals("if") || exprArg.equals("-e")) {
        if (exprIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'if'");
        } else {
          return args.remove(exprIndex).toString();
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }


  /**
   * Parses the list of arguments for class filter information.
   * <pre>
   * class_filters "classPatternList"
   * </pre>
   * Note that the exception is <b>not</b> raised if the tag isn't
   * present: only if what follows the tag is incorrect.
   * <p>
   * the classPatternList should be a list of class patterns, using
   * space or comma as delimiter.
   * <p>
   * Shortcut: <u>-cf</u>
   * <p>
   * This constraint is used to add class filters to events. To quote
   * JDI documentation:
   * <p>
   * Restricts the events generated by this request to those whose
   * location is in a class whose name matches a restricted regular
   * expression. Regular expressions are limited to exact matches
   * and patterns that begin with '*' or end with '*'; for example,
   * "*.Foo" or "java.*".
   * <p>
   * @return a List of all the class filters. 
   * @exception JDEException If the information cannot be resolved
   */
  public static List getClassFiltersFromArgs(List args)
    throws JDEException {

    String filterArg = null;
    int filterIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("class_filters") || arg.equals("-cf")) { 
        filterIndex = i;
        filterArg = args.remove(filterIndex).toString();
        break;
      }
    }
    // at this point, either filterArg = null, or "if". in
    // that case, filterIndex should now have the index of the argument
    if (filterArg == null) {
      return null;
    } else {
      if (filterArg.equals("class_filters")
          || filterArg.equals("-cf")) {
        if (filterIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'class_filters'");
        } else {
          List filters = new ArrayList();
          StringTokenizer tokens = new StringTokenizer(args.remove(filterIndex).toString(), " \t\n\r\f,");
          while (tokens.hasMoreTokens()) {
            filters.add(tokens.nextToken());
          }
          return filters;
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }

    
  /**
   * Parses the list of arguments for class exclusion filter information.
   * <pre>
   * class_exclusion_filters "classPatternList"
   * </pre>
   * Note that the exception is <b>not</b> raised if the tag isn't
   * present: only if what follows the tag is incorrect.
   * <p>
   * the classPatternList should be a list of class patterns, using
   * space or comma as delimiter.
   * <p>
   * Shortcut: <u>-cef</u>
   * <p>
   * This is used to add class exclusion filters to events. To quote
   * JDI documentation:
   * <p>
   * Restricts the events generated by this request to those whose
   * location is in a class whose name does <b>not</b> match this
   * restricted
   * regular expression. Regular expressions are limited to exact matches
   * and patterns that begin with '*' or end with '*'; for example,
   * "*.Foo" or "java.*".
   * <p>
   * @return a List of all the class exclusion filters. 
   * @exception JDEException If the information cannot be resolved
   */
  public static List getClassExFiltersFromArgs(List args)
    throws JDEException {
	
    String filterArg = null;
    int filterIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("class_exclusion_filters")
          || arg.equals("-cef")) { 
        filterIndex = i;
        filterArg = args.remove(filterIndex).toString();
        break;
      }
    }
    // at this point, either filterArg = null, or "if". in
    // that case, filterIndex should now have the index of the argument
    if (filterArg == null) {
      return null;
    } else {
      if (filterArg.equals("class_exclusion_filters")
          || filterArg.equals("-cef")) {
        if (filterIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'class_exclusion_filters'");
        } else {
          List filters = new ArrayList();
          StringTokenizer tokens = new StringTokenizer(args.remove(filterIndex).toString(), " \t\n\r\f,");
          while (tokens.hasMoreTokens()) {
            filters.add(tokens.nextToken());
          }
          return filters;
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }


  /**
   * Parses the list of arguments for suspend policy information.
   * <pre>
   * using_suspend_policy policy
   * </pre>
   * Note that the exception is <b>not</b> raised if the tags are
   * not present: only if what follows the tag is incorrect.
   * <p>
   * <i>policy</i> is one of "all", "thread", or "none". "all" means the
   * entire
   * VM is suspended when the event occurs, "thread" indicates only the
   * thread on which the event occurs is suspended (only for events
   * associated with threads), while "none" means nothing is suspended
   * when the event occurs.
   * <p>
   * Shortcut: <u>-sp</u>
   * <p>
   * @return a valid int indicating the suspend policy
   * @exception JDEException If the information cannot be resolved
   */
  public static int getSuspendPolicyFromArgs(List args)
    throws JDEException {
	
    String suspendPolicyArg = null;
    int suspendPolicyIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("using_suspend_policy") || arg.equals("-sp")) { 
        suspendPolicyIndex = i;
        suspendPolicyArg =args.remove(suspendPolicyIndex).toString();
        break;
      }
    }
    // at this point, either suspendPolicyArg = null, or "if". in
    // that case, suspendPolicyIndex should now have the index of the
    // argument
    if (suspendPolicyArg == null) {
      return EventRequest.SUSPEND_ALL;
    } else {
      if (suspendPolicyArg.equals("using_suspend_policy")
          || suspendPolicyArg.equals("-sp")) {
        if (suspendPolicyIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'using_suspend_policy'");
        } else {
          String policy = args.remove(suspendPolicyIndex).toString().toLowerCase();
          if (policy.equals("all")) {
            return EventRequest.SUSPEND_ALL;
          } else if (policy.equals("thread")) {
            return EventRequest.SUSPEND_EVENT_THREAD;
          } else if (policy.equals("none")) {
            return EventRequest.SUSPEND_NONE;
          } else {
            throw new JDEException("Invalid suspend policy '"+policy+"'");
          }
		    
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }

    
  /**
   * Parses the list of arguments for object ID information.
   * <pre>
   * if_object_id objectID
   * </pre>
   * Note that the exception is <b>not</b> raised if the tag isn't
   * present: only if what follows the tag is incorrect.
   * <p>
   * Shortcut: <u>-oid</u>
   * <p>
   * @return a Long corresponding to the object ID.
   * @exception JDEException If the information cannot be resolved
   */
  public static Long getObjectIDFromArgs(List args)
    throws JDEException {
	
    String idArg = null;
    int idIndex = -1;
    for (int i = 0; i < args.size(); i++) {
      String arg = args.get(i).toString().toLowerCase();
      if (arg.equals("if_object_id") || arg.equals("-oid")) { 
        idIndex = i;
        idArg = args.remove(idIndex).toString();
        break;
      }
    }
    // at this point, either idArg = null, or "if". in
    // that case, idIndex should now have the index of the argument
    if (idArg == null) {
      return null;
    } else {
      if (idArg.equals("if_object_id")
          || idArg.equals("-oid")) {
        if (idIndex == args.size()) {
          // ie missing argument
          throw new JDEException("Missing argument to 'if_object_id'");
        } else {
          try {
            return new Long(args.remove(idIndex).toString());
          } catch (NumberFormatException ex) {
            throw new JDENumberFormatException("'if_object_id' argument");
          }
        }
      } else {
        throw new JDEException("Should not happen! Contact maintainer");
      }
    }
  }

} // Etc

/*
 * $Log: Etc.java,v $
 * Revision 1.3  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of Etc.java
