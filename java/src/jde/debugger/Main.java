package jde.debugger;

import java.io.*;
import java.net.*;
import java.util.*;

/**
 * Main class for JDEbug.
 *
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.6 $
 */
public class Main implements Protocol {

  /**
   * Main method for the JDEbug application.
   * This method starts the debugger command
   * loop.
   *
   * @param args a <code>String[]</code> value
   */
  public static void main(String[] args) {
    if (args.length > 0) {
      System.out.println("Usage: java jde.debugger.Main");
      System.out.flush();
      System.exit(1);
    } else {
      System.out.println(BR+"(" + JDE_INIT_DEBUG_SESSION+")"+BR);
      System.out.flush();
      try {
	  //        JDE.setDebugFlags(0);
        JDEbug.theDebugger.init();
        JDEbug.theDebugger.start();
      } catch (IOException ex) {
        System.out.println("I/O Error");
        System.exit(1);
      }
    }
  }

} // main

/*
 * $Log: Main.java,v $
 * Revision 1.6  2004/12/24 16:05:13  troy
 * Add window to display threads and stacks
 *
 * Revision 1.5  2003/09/21 05:29:51  paulk
 * Turn off debug message flags.
 *
 * Revision 1.4  2003/01/15 05:53:28  paulk
 * Updated some javadoc.
 *
 * Revision 1.3  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 */
