/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.2 $
 * $Date: 2003/02/18 05:36:47 $
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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Proxy;
import java.util.Arrays;

import junit.framework.*;

import jde.juci.test.*;

/**
 * Test the <code>ConnectionImpl</code> class.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class ConnectionImplTest
    extends TestCase
{
  /**
   * Creates a new <code>ConnectionImplTest</code> instance.
   *
   * @param name a <code>String</code> value
   */
  public ConnectionImplTest(String name) {
    super(name);
  }

  public static interface CallTest {
    void doIt(Object o);
  }

  //______________________________________________________________________
  //
  // Setup, teardown

  /**
   * Set up the fixture
   */
  protected void setUp() {
    ConnectionFactory.setUnitTesting(true);
  }

  /**
   * Tear down the fixture
   */
  protected void tearDown() {
    ConnectionFactory.setUnitTesting(false);
  }

  //______________________________________________________________________
  //
  // Testing methods

  public void testConnectionProxy1GenElisp() throws Throwable {
    Connection conn = ConnectionFactory.getConnection(CallTest.class);
    StringWriter output = new StringWriter();
    ConnectionImpl connimpl = (ConnectionImpl) Proxy.getInvocationHandler(conn);
    connimpl.setOutput(new PrintWriter(output));
    connimpl.setThreaded(false);

    conn.begin();
    CallTest ct = (CallTest) conn;
    ct.doIt(new Quoted(Arrays.asList(new Object[] {
      new Symbol("apply"),
      new Quoted(new Symbol("+")),
      new Integer(1),
      new Integer(2),
      new Quoted(Arrays.asList(new Object[] {new Integer(3), new Integer(4)}))
    })));
    assertEquals("(jde-juci-invoke-elisp '(jde-juci-connection-impl-test-call-test-do-it '(apply '+ 1 2 '(3 4))))", output.toString().trim());
    conn.end();
  }

  public void testConnectionProxy2CallJava1() throws Exception {
    Connection conn = ConnectionFactory.getConnection(EchoImpl.class, null, null);
    Echo echo = (Echo) conn;
    StringWriter output = new StringWriter();
    ConnectionImpl connimpl = (ConnectionImpl) Proxy.getInvocationHandler(conn);
    connimpl.setOutput(new PrintWriter(new StringWriter()));

    conn.begin();
    echo.ack("hello world");
    connimpl.setOutput(new PrintWriter(output)); // reset output here to
                                                 // ignore the 'nil' that
                                                 // gets output
    conn.end();
    assertEquals("\"hello world\"", output.toString().trim());
  }

  public void testConnectionProxy2CallJava2() throws Exception {
    Echo echo = (Echo) ConnectionFactory.getConnection(EchoImpl.class, null, null);
    assertEquals("hello world", echo.ack("hello world"));
    ((Connection) echo).end();
  }

}

// ConnectionImplTest.java ends here
