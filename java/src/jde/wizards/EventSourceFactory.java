/*
 *    EventSourceFactory.java
 *    Copyright (C) 2001, 2002, 2003 Stephane Nicolas <s.nicolas@videotron.ca>
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

package jde.wizards;

import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Enumeration;
import java.util.Vector;

public class EventSourceFactory extends InterfaceFactory 
{
  private static EventSourceFactory eventSourceFactory;
  private Class listenerClass;
  private String listenerClassNameFQN;
  private String listenerClassName;
  private String listenerVectorName;

  public EventSourceFactory() {}

  /** 
   * Creates an EventSourceFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public EventSourceFactory(NameFactory factory)
  {
    super(factory);
  }


  /**
   * Makes an implementation of an interface. This method delegates
   * the creation of the implementation to makeInterfaceInternal.
   *
   * @param name Name of interface to be implemented.
   * @param truncate If <code>true</code>, truncate package specifier
   * when generating code.
   */
  public static void makeEventSourceSupportExpression
      (String listenerName, boolean truncate) {

    if (eventSourceFactory == null) {
      eventSourceFactory = new EventSourceFactory();
    }

    eventSourceFactory.flush();
    eventSourceFactory.makeEventSourceSupportExpressionInternal
	(listenerName, truncate);
  }

 /**
   * Makes an implementation of an interface.
   *
   * @param name Name of interface to be implemented.
   * @param truncate If <code>true</code>, truncate package specifier
   * when generating code.
   */
  private void makeEventSourceSupportExpressionInternal
      (String name, boolean truncate) 
  {
    try 
    {
      listenerClass = Class.forName( name );
      listenerClassNameFQN= listenerClass.getName();
      listenerClassName = listenerClass.getName().substring( listenerClass.getName().lastIndexOf('.')+1);
      StringBuffer buf = new StringBuffer( listenerClassName );
      buf.setCharAt( 0, Character.toLowerCase(listenerClassName.charAt( 0 )) );
      listenerVectorName = buf.toString()+"s";
    }
    catch (ClassNotFoundException e) {
      println("(error \"Error: could not find listenerClass named: " + name + ". "
	      + "Note: name must be qualified.\")");
      return;
    }
    super.implementInterface(name, truncate);

  }//met

  public static void getImportedClasses() {
    println(eventSourceFactory.getImportsAsList());
  }

  public void dumpExpression(PrintWriter out, boolean truncate) {
    
    final StringBuffer buf =
      new StringBuffer("(jde-wiz-gen-event-source (list ");

    try {
      registerImport(Class.forName("java.util.Vector"));
      registerImport(Class.forName(listenerClassNameFQN));    
    } catch( Exception ex ) {
    }

    buf.append ("(quote (jde-wiz-gen-listener-registry");
    buf.append (" \"" + listenerClassNameFQN + "\"))");

    SignatureContainer sigs = getSignatures();

    if (!sigs.isEmpty()) {
       
      buf.append ("(quote ");
      buf.append("\"Fire methods for ");
      buf.append(listenerClassName);
      buf.append("\")");
 
      sigs.visit(new SignatureVisitor() { 
	  public void visit(Signature sig, boolean firstOfClass) {

	    if (firstOfClass) {
	      buf.append ("(quote ");
	      buf.append("\"Implementation of ");
	      buf.append(sig.getDeclaringClass().getName());
	      buf.append("\")");
	    }

	    buf.append ("(quote ");
	    buf.append(createEventSourceFireMethodExpression(sig));
	    buf.append(")");
	    
	  }});
    }
		     
    buf.append("))");
    println(buf.toString());
  }


  private String createEventSourceFireMethodExpression(Signature sig)
  {
    String methName = sig.getMethod().getName();
    StringBuffer temp = new StringBuffer( methName );
    temp.setCharAt( 0, Character.toUpperCase( temp.charAt( 0 ) ) );
    String methNameUpCase = temp.toString();

    StringBuffer buf = new StringBuffer();
    buf.append ("(jde-wiz-gen-event-source-fire-method");
    buf.append (" \"" + listenerClassNameFQN + "\"");
    buf.append (" \"" + methName + "\"");
    buf.append (" \"" + sig.baseName(sig.getMethod().getReturnType()) + "\"");
    buf.append (" \"" + sig.getParameters(sig.getMethod().getParameterTypes())
		+ "\")");
    return buf.toString();
  }//met


  public static void main (String[] args) 
  {
    EventSourceFactory.makeEventSourceSupportExpression
	("javax.swing.event.MouseInputListener", true);
  } // end of main ()



  
}//class EventSourceFactory

/*
 * Change Log
 *
 * $Log: EventSourceFactory.java,v $
 * Revision 1.4  2003/09/07 05:29:12  paulk
 * Check for duplicate methods defined by different classes or interfaces.
 * Thanks to Martin Schwamberg.
 *
 * Revision 1.3  2002/05/14 06:38:44  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp" <lipp@danet.de>
 * for proposing and implementing this improvement.
 *
 * Revision 1.2  2001/10/21 14:08:46  paulk
 * Added change log.
 *
 *
 */

// End of EventSourceFactory.java
