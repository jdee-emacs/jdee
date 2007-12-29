package jde.debugger;

import com.sun.jdi.*;

import java.util.*;

/**
 * Rep.java
 * <p>
 * Responsible for providing static methods used in spewing out string
 * representations.
 * <ul>
 * <li> A useful hierarchy:
 *  <ul>
 *  <li> Value
 *   <ul>
 *   <li> ObjectReference
 *    <ul>
 *    <li> StringReference
 *    <li> ArrayReference
 *    <li> ThreadReference
 *    <li> ThreadGroupReference
 *    <li> Other...
 *    </ul>
 *   <li> PrimitiveValue
 *    <ul>
 *    <li> BooleanValue
 *    <li> etc....
 *    </ul>
 *   </ul>
 *  </ul>
 * </ul> 
 * In our design, whenever we encounter an objectReference, we pass a
 * sort of summary to jde, as well as an 'id' to identify it with.
 * Whenever jde needs info about the objectReference, it uses the id to
 * uniquely identify the object.
 * <p>
 * Now, the representation that is sent across for the threads (ie to the
 * jde) depends on the context. When it is sent with reference to thread
 * commands, eg. get_threads, get_thread, get_object_monitors; it has
 * a lot of thread specific information, eg. its state and all.
 * <p>
 * When it's sent treating the thread as an object, eg. get_object, it's
 * represented differently, and a different set of information is sent.
 * <p>
 * Similary, when an array command is used, a different set of information
 * is sent across, as against when it's treated as an object.
 * <p>
 * Created: Tue Aug  3 16:36:54 1999
 *
 *
 * Copyright (c) 2000, 2001, 2003    Paul Kinnucan
 *
 * 
 *
 * @author Amit Kumar
 * @author Paul Kinnucan
 * @since 0.1
 * @version $Revision: 1.18 $
 */

public class Rep implements Protocol {

  /**
   * Returns a representation of a Location
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list "type-name" "sourcefile" lineNumber)
   * (list "type-name" nil lineNumber)
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> lineNumber is -1 if that information is not available
   * </ul>
   */
  public static String getLocationRep(Location loc) {
    StringBuffer locationString = new StringBuffer();
    locationString.append("(list \"");
    locationString.append(loc.declaringType().name());
    locationString.append("\"");
    try {
      locationString.append(" \"");
      locationString.append(loc.sourceName());
      locationString.append("\"");
    } catch (AbsentInformationException ex) {
      locationString.append(" nil"); // XXX - check if this is OK, or if we need to remove the \".
    }
    locationString.append(" ");
    locationString.append(loc.lineNumber());
    locationString.append(")");
    return locationString.toString();
  }
    

  /**
   * Returns a representation of a method
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list "name of method" return-type-name
   *    (list [argument-type-name]*)
   *    ["final"] ["static"] ["native"] ["constructor"] ["abstract"]
   *    ["synchronized"] ["static_initializer"])
   * </pre>
   */
  static String getMethodRep(Method m) {
    List l = m.argumentTypeNames();
    StringBuffer argList = new StringBuffer("(list");
    Iterator it = l.iterator();
    while (it.hasNext()) {
      argList.append(" \"");
      argList.append(it.next().toString());
      argList.append("\"");
    }
    argList.append(")");
	
    // The below code results in an extra StringBuffer being created, but I think
    // that's OK from a performance perspective.
    return "(list \""+m.declaringType().name()+"\""
      +" \""+m.name()+"\""
      +" \""+m.returnTypeName()+"\""
      + BR + argList.toString()
      +(m.isFinal()?" \"final\"":"")
      +(m.isStatic()?" \"static\"":"")
      +(m.isNative()?" \"native\"":"")
      +(m.isConstructor()?" \"constructor\"":"")
      +(m.isAbstract()?" \"abstract\"":"")
      +(m.isSynchronized()?" \"synchronized\"":"")
      +(m.isStaticInitializer()
        ?" \"static_initializer\"":"")
      +")";
  }
    

    
  /**
   * Returns a representation of a local variable on a stack frame
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list "name of variable" "type of variable")
   * </pre>
   */
  static public String getLocalVariableRep(LocalVariable lv) {
    return "(list" + " \""+lv.name()+"\" \""+lv.typeName()+"\")";
  }
    
  /**
   * Returns a representation of a (local variable, value) pair.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * ({@link #getLocalVariableRep local-variable} . {@link #getValueRep value})
   * </pre>
   */
  static public String getLocalVariableValueRep(LocalVariable lv, Value v) {
    return "(cons "+getLocalVariableRep(lv)
      +" "+getValueRep(v)+")";
  }

  /**
   * Returns a list of (local variable, value) pairs.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list [{@link #getLocalVariableValueRep (local variable, value) pair}]*)
   * </pre>
   */
  static public String getLocalVariableValueMapRep(Map map) {
    StringBuffer localVariablesValuesString = new StringBuffer("(list ");
    Set keys = map.keySet();
    Iterator iter = keys.iterator();
    while (iter.hasNext()) {
      LocalVariable localVariable = (LocalVariable)iter.next();
      Value val = (Value)map.get(localVariable);
      localVariablesValuesString.append(BR);
      localVariablesValuesString.append(getLocalVariableValueRep(localVariable, val));
    }
    localVariablesValuesString.append(")");
    return localVariablesValuesString.toString();
  }


  /**
   * Returns a representation of a field.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list "name of field" "type of field" ["transient"] ["volatile"]
   *                                       ["final"] ["static"])
   * </pre>
   */
  static String getFieldRep(Field f) {
    return "(list"
      + " \""+f.name()+"\""
      + " \""+f.typeName()+"\""
      + (f.isTransient() ? " \"transient\"" : "")
      + (f.isVolatile() ? " \"volatile\"" : "")
      + (f.isFinal() ? " \"final\"" : "")
      + (f.isStatic() ? " \"static\"" : "")
      +")";
  }

  /**
   * Returns a representation of a (field, value) pair.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * ({@link #getFieldRep field} . {@link #getValueRep value})
   * </pre>
   */
  static String getFieldValueRep(Field f, Value v) {
    return "(cons "+getFieldRep(f)+" "+getValueRep(v)+")";
  }
    
  /**
   * Returns a list of (field, value) pairs.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list [{@link #getFieldValueRep (field, value) pair}]*)
   * </pre>
   */
  static String getFieldValueMapRep(Map map) {
    StringBuffer fieldsValuesString = new StringBuffer("(list ");
    Set keys = map.keySet();
    Iterator iter = keys.iterator();
    while (iter.hasNext()) {
      Field field = (Field)iter.next();
      Value val = (Value)map.get(field);
      fieldsValuesString.append(BR);
      fieldsValuesString.append(getFieldValueRep(field, val));
    }
    fieldsValuesString.append(")");
    return fieldsValuesString.toString();
  }
    
  private static String filterFPValue(String fpValue) {
    if (fpValue.equals("NaN"))
      return "\"NaN\"";
    else if (fpValue.equals("-Infinity"))
      return "\"-Infinity\"";
    else if (fpValue.equals("Infinity"))
      return "\"Infinity\"";
    else
      return fpValue;    
  }



  /**
   * Returns a representation of a 'value', that can be primitive
   * or an object reference, or void.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list "null")
   * (list "void")
   *
   * {@link #getObjectRep(ObjectReference) object-rep}
   * 
   * (list "boolean" "true")      (list "boolean" "false")
   * (list "byte"    'byte-value')
   * (list "char"    'char-value')
   * (list "double"  double-value)
   * (list "float"   float-value)
   * (list "int"     int-value)
   * (list "long"    long-value)
   * (list "short"   short-value)
   * </pre>
   */
  static public String getValueRep(Value value) {
    if (value == null) {
      return "(list \"null\")";
    } else if (value instanceof VoidValue) {
      return "(list \"void\")";
    } else if (value instanceof ObjectReference) {
      return getObjectRep((ObjectReference)value);
    } else {
      PrimitiveValue v = (PrimitiveValue)value;
      if (v instanceof BooleanValue) {
        return "(list \"boolean\" \""+v.booleanValue()+"\")";
      } else if (v instanceof ByteValue) {
        return "(list \"byte\" \""+v.byteValue()+"\")";
      } else if (v instanceof CharValue) {
        return "(list \"char\" \""+
          escapeString(String.valueOf(v.charValue()))+"\")";
      } else if (v instanceof DoubleValue) {
        return "(list \"double\" " + filterFPValue(String.valueOf(v.doubleValue()))+")";
      } else if (v instanceof FloatValue) {
        return "(list \"float\" "+filterFPValue(String.valueOf(v.floatValue()))+")";
      } else if (v instanceof IntegerValue) {
        return "(list \"int\" \""+v.intValue()+"\")";
      } else if (v instanceof LongValue) {
        return "(list \"long\" \""+v.longValue()+"\")";
      } else if (v instanceof ShortValue) {
        return "(list \"short\" "+v.shortValue()+")";
      }
    }
    return null;
  }

  /**
   * Returns information about an array
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * "Error message"
   * (list "type name" uniqueID ['t|nil] length [element]*)
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The third argument (['t|nil]) indicates if the object has
   * been garbage collected in the debugee vm: it's nil if it hasn't.
   * <li> elements are only present if the index/length make sense. See
   * param list.
   * </ul>
   * <p>
   *
   * @param String a description of the array elements
   */
  static public String getArrayRep(ArrayReference a, String elements) {
    if (a == null) {
      return "\"Error! null array reference in Rep.getArrayRep!\"";
    } else {
	    
      return "(list "
        + "\""+a.referenceType().name()+"\""
        + " " +a.uniqueID()
        + (a.isCollected() ? " 't":" nil")
        + " " + a.length()
        + elements + ")";
    }
  }


  /**
   * Prefix \ escapes to all \ and " characters in a string so that
   * the string can be read byte the Lisp interpreter. For efficiency,
   * if no such characters are found, the argument String itself
   * is returned.
   *
   * @param  str   String to be prefixed.
   * @return A String.
   *
   * @author David Hay
   * @author Mark Gibson
   * @author Steve Haflich
   * @author Charles Hart
   * @author David Dagon
   */
  public static String escapeString (String str) {
	
    if ( str.indexOf('\\') == -1 &&
         str.indexOf('"')  == -1 )
      {
        return str;
      }
    else
      {
        StringBuffer buf = new StringBuffer(str.length() + 16);
        for ( int i = 0; i < str.length(); i++ ) {
          char ch = str.charAt( i );
          switch ( ch ) {
          case '"':  buf.append("\\\"" ); break;
          case '\\': buf.append("\\\\" ); break;
          default:   buf.append( ch );    break;
          }
        }
        return buf.toString();
      }
  }
    
    

  /**
   * Returns the value of a string
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * "Error message"
   * (list "java.lang.String" uniqueID ['t|nil] "string-value")
   * </pre>
   * <b>Comments:</b>
   * <ul>
   * <li> The third argument (['t|nil]) indicates if the object has
   * been garbage collected in the debugee vm: it's nil if it hasn't.
   * </ul>
   * <p>
   */
  static public String getStringRep(StringReference s) {
    if (s == null) {
      return "\"Error!\"";
    } else {
	    
      return "(list "
        + "\""+s.referenceType().name()+"\""
        + " "+s.uniqueID()
        + (s.isCollected() ? " 't":" nil")
        + " \"" + escapeString(s.value()) + "\")";
    }
  }
    

  /**
   * Returns a non-detailed representation of an object.
   *
   * @see #getObjectRep(ObjectReference,boolean)
   */
  static public String getObjectRep(ObjectReference o) {
    return getObjectRep(o, false);
  }

  /**
   * Returns a canonical representation of an object.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * "Error Message"
   * (list "null")
   * <i>Non-detailed</i>
   * (list "type of object" uniqueID ['t|nil])
   * <i>Detailed</i>
   * (list "type of object" uniqueID ['t|nil] {@link #getFieldValueMapRep fields-values})
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The third argument (['t|nil]) indicates if the object has
   * been garbage collected in the debugee vm: it's nil if it hasn't.
   * </ul>
   */
  static public String getObjectRep(ObjectReference o, boolean detailed) {
    if (o == null) {
      return "(list \"null\")";
    } else {
      if (detailed) {
        // fields and values
        String fieldsValuesString;
        try {
          // XXX a more complete list is available using
          // allFields().... fyi
          fieldsValuesString = getFieldValueMapRep(o.getValues(o.referenceType().visibleFields()));
        } catch (ClassNotPreparedException ex) {
          fieldsValuesString = "\"The class isn't prepared\"";
        } catch (ObjectCollectedException ex) {
          fieldsValuesString = "\"The object has already been collected\"";
        } catch (Exception ex) {
          fieldsValuesString = "\"Unable to access fields and values. Optimized class?\"";
        }
		
        return "(list "
          + "\""+o.referenceType().name()+"\""
          + " "+o.uniqueID()
          + (o.isCollected() ? " 't":" nil")+BR
          + fieldsValuesString+")";
      } else {
        return "(list "
          + "\""+o.referenceType().name()+"\""
          + " "+o.uniqueID()
          + (o.isCollected() ? " 't":" nil")
          +")";
      }
    }
  }
    
    
  /*
   * THREAD REPRESENTATIONS
   */

  /**
   * Returns information about monitors of an object.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list uniqueID "type of object" ['t|nil] {@link #getThreadRep owning-thread} (list [{@link #getThreadRep waiting-thread}]*))
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> The third argument (['t|nil]) indicates if the object has
   * been garbage collected in the debugee vm: it's nil if it hasn't.
   * </ul>
   */
  static public String getObjectMonitorsRep(ObjectReference o) {
    if (o == null) {
      return "null";
    } else {

      // owning thread
      String owningThread;
      try {
        ThreadReference t = o.owningThread();
        if (t == null) {
          owningThread = "nil";
        } else {
          owningThread = getThreadRep(t);
        }
      } catch (IncompatibleThreadStateException ex) {
        owningThread = "\"Information Not Available\"";
      } catch (UnsupportedOperationException ex) {
        owningThread = "\"VM has no information\"";
      }
	    
      // waiting threads
      StringBuffer waitingThreadsStringBuffer = new StringBuffer("(list");
      String       waitingThreadsString;
      try {
        List waitingThreads = o.waitingThreads();
        Iterator it = waitingThreads.iterator();
        while (it.hasNext()) {
          waitingThreadsStringBuffer.append(BR);
          waitingThreadsStringBuffer.append(getThreadRep((ThreadReference)it.next()));
        }
        waitingThreadsStringBuffer.append(")");
        waitingThreadsString = waitingThreadsStringBuffer.toString();
      } catch (IncompatibleThreadStateException ex) {
        waitingThreadsString = "\"Information Not Available\"";
      } catch (UnsupportedOperationException ex) {
        waitingThreadsString = "\"VM has no information\"";
      }
	    
	    
      return "(list "+o.uniqueID()+" "
        +"\""+o.referenceType().name()+"\""
        + (o.isCollected() ? " 't":" nil")+BR
        +owningThread+ BR 
        +waitingThreadsString+")";
    }
  }

  /* thread information retrieval routines */

    

  /**
   * Returns a canonical representation of a given ThreadGroupReference.
   * <p>
   * <b>Syntax:</b>
   * <pre>
   * (list "ThreadGroup" uniqueID "name of threadgroup"
   *                     (list [{@link #getThreadRep(ThreadReference) child thread}]*)
   *                     (list [{@link #getThreadGroupRep child threadgroup}]*))
   * </pre>
   */
  public static String getThreadGroupRep(ThreadGroupReference t) {
    StringBuffer rep = new StringBuffer("(list \"ThreadGroup\" ");
    rep.append(t.uniqueID());
    rep.append(" \"");
    rep.append(t.name());
    rep.append("\" ");

    List     list = t.threads();
    Iterator it   = list.iterator();

    rep.append(BR);
    rep.append("(list");

    while (it.hasNext()) {
      rep.append(BR);
      rep.append(getThreadRep((ThreadReference)it.next()));
    }
    rep.append(")");

    list = t.threadGroups();
    it   = list.iterator();
    rep.append(BR);
    rep.append("(list");
    while (it.hasNext()) {
      rep.append(BR);
      rep.append(getThreadGroupRep((ThreadGroupReference)it.next()));
    }
    rep.append("))");
	
    return rep.toString();
  }

  /**
   * Returns a detailed thread representation.
   * @see #getThreadRep(ThreadReference, boolean)
   */
  static public String getThreadRep(ThreadReference t) {
    return getThreadRep(t, true);
  }

  /**
   * Returns a canonical representation of a given ThreadReference.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * <i>Non-detailed</i>
   * (list "Thread" uniqueID "name of thread" <u>status</u> <u>currentState</u>)
   * <i>Detailed</i>
   * (list "Thread" uniqueID "name of thread" status currentState
   *                (list [{@link #getStackFrameRep stack-frame}]*)
   *                <u>owned-monitors-string</u>
   *                <u>current-contended-monitor-string</u>)
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> <u>status</u> is one of: "unknown", "waiting on monitor",
   *      "not started", "runnable", "sleeping", "waiting", and "zombie"
   *
   * <li> <u>currentState</u> is one of "normal", "suspended by debugger",
   *      and "suspended at breakpoint"

   * <li> <u>owned-monitors-string</u>:
   *  <pre>
   *  "Error Message"
   *  (list [{@link #getObjectRep(ObjectReference) owned monitor}]*)
   *  </pre>
   * <li> <u>current-contended-monitor-string</u>:
   *  <pre>
   *  "Error Message"
   *  nil
   *  {@link #getObjectRep(ObjectReference) current contended monitor}
   *  </pre>
   * <li>
   * Examples:
   * <pre>
   *    (list "Thread" 53 "Thread 1, continuous"
   *          "suspended by debugger" "waiting on monitor"
   *          (list 
   *             (list 0 "test.Test" "Test.java" 45))
   *          (list)
   *          (list "java.lang.String" 55))
   *
   *    (list "Thread" 54 "Thread 2"
   *          "suspended by debugger" "waiting on monitor"
   *          (list 
   *             (list 0 "java.lang.Thread" "Thread.java" -1)
   *             (list 1 "test.Test" "Test.java" 47))
   *          (list 
   *             (list "java.lang.String" 55)
   *             (list "java.lang.Integer" 61))
   *          (list))
   * </pre>
   * </ul>
   *
   * <p>
   * @param detailed True if a more detailed representation is desired:
   * includes the stackframe as well as information about the monitors.
   */
  static public String getThreadRep(ThreadReference t,
                                    boolean detailed) {
    int    status       = t.status();
    String statusString = "unknown";
    switch (status) {
    case ThreadReference.THREAD_STATUS_MONITOR:
      statusString = "waiting on monitor";
      break;
    case ThreadReference.THREAD_STATUS_NOT_STARTED: 
      statusString = "not started";
      break;
    case ThreadReference.THREAD_STATUS_RUNNING:
      statusString = "runnable";
      break;
    case ThreadReference.THREAD_STATUS_SLEEPING:
      statusString = "sleeping";
      break;
    case ThreadReference.THREAD_STATUS_WAIT:
      statusString = "waiting";
      break;
    case ThreadReference.THREAD_STATUS_ZOMBIE:
      statusString = "zombie";
      break;
    case ThreadReference.THREAD_STATUS_UNKNOWN:
      statusString = "unknown";
      break;
    default:
      break;
    }

    // note that the above status string refers to the state of the 
    // thread *before* a suspension, if there was a suspension.

    /* Due to a bug in ThreadReference.isSuspended(), we need to
       use suspendCount() */
    String stateString = "normal";
    if (t.isAtBreakpoint()) {
      stateString = "suspended at breakpoint";
    } else if (t.suspendCount() > 0) {
      stateString = "suspended by debugger";
    }

    if (detailed) {

      // info on the stack

      StringBuffer stackStringBuffer = new StringBuffer("(list");
      String       stackString;
      try {
        // a list of the stackframes is also sent...
        List stackFrames = t.frames();
        Iterator it = stackFrames.iterator();
        int index = 0;
        while (it.hasNext()) {
          stackStringBuffer.append(BR);
          stackStringBuffer.append(getStackFrameRep((StackFrame)it.next(), index++));
        }
        stackStringBuffer.append(")");
        stackString = stackStringBuffer.toString();
      } catch (IncompatibleThreadStateException ex) {
        stackString = "\"Information Not Available\"";
      }

      // info on the monitors

      // owned monitors

      StringBuffer ownedMonitorsStringBuffer = new StringBuffer("(list");
      String ownedMonitorsString;
      try {
        List ownedMonitors = t.ownedMonitors();
        Iterator it = ownedMonitors.iterator();
        while (it.hasNext()) {
          ownedMonitorsStringBuffer.append(BR);
          ownedMonitorsStringBuffer.append(getObjectRep((ObjectReference)it.next()));
        }
        ownedMonitorsStringBuffer.append(")");
        ownedMonitorsString = ownedMonitorsStringBuffer.toString();
      } catch (IncompatibleThreadStateException ex) {
        ownedMonitorsString = "\"Information Not Available\"";
      } catch (UnsupportedOperationException ex) {
        ownedMonitorsString = "\"VM has no information\"";
      } catch (ObjectCollectedException ex) {
        ownedMonitorsString = "\"The object has been collected\"";
      }
	    
      // current contended monitor
      // note, however, from the jdi api:
      // The thread can be waiting for a monitor through entry into a
      // synchronized method, the synchronized statement, or
      // Object.wait(). The status() method can be used to
      // differentiate between the first two cases and the third. 
	    
      String currentContendedMonitorString;
      try {
        ObjectReference o = t.currentContendedMonitor();
        if (o == null) {
          currentContendedMonitorString = "nil";
        } else {
          currentContendedMonitorString =
            getObjectRep(o);
        }
      } catch (IncompatibleThreadStateException ex) {
        currentContendedMonitorString =
          "\"Information Not Available\"";
      } catch (UnsupportedOperationException ex) {
        currentContendedMonitorString =
          "\"VM has no information\"";
      } catch (ObjectCollectedException ex) {
        currentContendedMonitorString =
          "\"The object has been collected\"";
      }

      return "(list \"Thread\""
        +" "+t.uniqueID()
        +" \""+t.name()+"\""
        +" \""+statusString+"\""
        +" \""+stateString+"\""
        + BR +stackString
        + BR +ownedMonitorsString
        + BR +currentContendedMonitorString
        +")";
    } else {
      return "(list \"Thread\""
        +" "+t.uniqueID()
        +" \""+t.name()+"\""
        +" \""+statusString+"\""
        +" \""+stateString+"\")";
    }
  }

  /**
   * Returns a canonical representation of a given StackFrame.
   * <p>
   *
   * <b>Syntax:</b>
   * <pre>
   * (list "StackFrame" index "Information not available")
   * (list "StackFrame" index "type name" "source name" lineNumber "method name")
   * </pre>
   *
   * <b>Comments:</b>
   * <ul>
   * <li> lineNumber is -1 for native methods
   * </ul>
   *
   * @param index Gives the index of this particular stack frame for
   * the thread. This basically goes into the string returned as a
   * convenience.
   */
  static String getStackFrameRep(StackFrame s, int index) {
    try {
      Location loc = s.location();
      Method method = loc.method();
      return "(list "+index+" "
        +"\""+loc.declaringType().name()+"\" "
        // Source file name can be a path with a backslash.
        // Need to escape the backslash.
        + "\"" + escapeString(loc.sourceName()) + "\" "
        +loc.lineNumber()+" "
        +"\""+method.name()+"\")";
    } catch (AbsentInformationException ex) {
      return "(list \"StackFrame\" "+index
        +" \"Information not available\")";
    }
  }
    
} // Rep

/*
 * $Log: Rep.java,v $
 * Revision 1.18  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 * Revision 1.17  2002/10/23 05:54:05  paulk
 * Updated the getStackFrameRep method to escape backslashes in the name of the source file
 * corresponding to a stack frame. The name of the source file can be a relative path in
 * some obscure cases and the relative path can include backslashes.
 *
 * Revision 1.16  2001/08/14 05:15:01  paulk
 * Miscellaneous updates.
 *
 * Revision 1.15  2001/04/19 04:43:55  paulk
 * Now escapes char values.
 *
 * Revision 1.14  2001/03/24 05:36:49  paulk
 * Updated to reflect reorganization of debuggee code.
 *
 * Revision 1.13  2000/07/28 06:26:31  paulk
 * Committing all modified files.
 *
 * Revision 1.12  2000/04/10 05:57:30  paulk
 * Publicized some methods.
 *
 * Revision 1.11  2000/04/01 06:02:37  paulk
 * Wrap NaN, Infinity, and -Infinity values in quotes to prevent Lisp evaluation errors.
 *
 * Revision 1.10  2000/03/17 03:35:23  paulk
 * Enhanced getStackFrameRep to return method. Thanks to Paul Michael Reilly <pmr@pajato.com>.
 *
 * Revision 1.9  2000/03/10 06:53:25  paulk
 * Escape quotes in strings.
 *
 * Revision 1.8  2000/03/04 08:58:11  paulk
 * Put quotes around byte, int, and long values to avoid Lisp
 * representation problems. Thanks to Charles Hart <cfhart@Z-TEL.com> for this fix.
 *
 */

// End Rep.java
