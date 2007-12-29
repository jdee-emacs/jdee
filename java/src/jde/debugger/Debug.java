
package jde.debugger;

/**
 * Debug.java
 * <p>
 * keeps state and has variables regarding the variable states
 * <p>
 * Explanation of debug levels:
 * Debug.NONE:     None
 * Debug.APP_IO:   Debug App I/O
 * Debug.JDE_PIPE: Debug JDE-JDEbug pipe
 * Debug.EVENTS:   Debug Event receiving/sending
 * Debug.EXCEPTION:Print stack trace on exception
 * <p>
 * Created: Thu Jul  8 13:03:25 1999
 *
 * @author Amit Kumar
 * @since 0.1
 */

public class Debug implements Protocol {

    // the different debug states
    public final static int NONE      = 0;
    public final static int APP_IO    = 1;
    public final static int JDE_PIPE  = 2;
    public final static int EVENTS    = 4;
    public final static int EXCEPTION = 8;

    public static int debug
	= APP_IO
	| JDE_PIPE
	| EVENTS
      //	| EXCEPTION
	;

    // some useful functions
    public static void printIf(Exception ex) {
	if (set(EXCEPTION)) {
	    System.out.println(BR+"("+JDE_BUG+DEBUG+" \"");
	    ex.printStackTrace();
	    System.out.println("\")"+BR);
	    System.out.flush();
	}
    }

    public static void printIf(int val, Object obj) {
	if (set(val)) {
	    System.out.println(BR+"("+JDE_BUG+DEBUG+" \"");
	    System.out.println(obj);
	    System.out.println("\")"+BR);
	    System.out.flush();
	}
    }
    
    public static void printIf(int val, double obj) {
	if (set(val)) {
	    System.out.println(BR+"("+JDE_BUG+DEBUG+" \"");
	    System.out.println(obj);
	    System.out.println("\")"+BR);
	    System.out.flush();
	}
    }

    public static boolean set(int val) {
	return ((debug & val) != 0);
    }
    
} // Debug
