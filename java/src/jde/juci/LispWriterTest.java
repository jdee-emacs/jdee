/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.1 $
 * $Date: 2003/02/15 20:58:26 $
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

import java.util.*;
import java.io.*;

import junit.framework.*;

/**
 * Test the {@link LispWriter} class.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class LispWriterTest
    extends TestCase {
  /**
   * Creates a new <code>LispWriterTest</code> instance.
   *
   * @param name a <code>String</code> value
   */
  public LispWriterTest(String name) {
    super(name);
  }

  private LispWriter lwriter;
  private StringWriter output;

  //______________________________________________________________________
  //
  // Setup, teardown

  /**
   * Set up the fixture
   */
  protected void setUp() {
    reset();
  }

  /**
   * Tear down the fixture
   */
  protected void tearDown() {
  }

  protected void reset() {
    output = new StringWriter();
    lwriter = new LispWriter(new PrintWriter(output));
  }

  //______________________________________________________________________
  //
  // Testing methods

  public void testInt1() {
    lwriter.writeInt(1010);
    assertEquals("1010", output.toString());
    reset();
    lwriter.writeUnknown(new Integer(1010));
    assertEquals("1010", output.toString());
  }

  public void testDouble1() {
    lwriter.writeDouble(10.1d);
    assertEquals("10.1", output.toString());
    reset();
    lwriter.writeUnknown(new Double(10.1d));
    assertEquals("10.1", output.toString());
  }

  public void testCons1() {
    lwriter.writeCons(new Cons("", new Symbol("find-buffer-file-type-coding-system")));
    assertEquals("(\"\" . find-buffer-file-type-coding-system)", output.toString());
    reset();
    lwriter.writeUnknown(new Cons("", new Symbol("find-buffer-file-type-coding-system")));
    assertEquals("(\"\" . find-buffer-file-type-coding-system)", output.toString());
  }

  public void testMapAlist() {
    Map m = new HashMap();
    m.put("foo", new Symbol("bar"));
    m.put("baz", new Symbol("quux"));
    lwriter.writeAlist(m);
    assertEquals("'((\"baz\" . quux) (\"foo\" . bar))", output.toString());
    reset();
    lwriter.writeUnknown(m);
    assertEquals("'((\"baz\" . quux) (\"foo\" . bar))", output.toString());
  }

  public void testChars() {
    lwriter.writeChar('a');
    assertEquals("?a", output.toString());
    reset();
    lwriter.writeChar('?');
    assertEquals("?\\?", output.toString());
    reset();
    lwriter.writeChar('\\');
    assertEquals("?\\\\", output.toString());
  }

  public void testForm1() {
    List l = new ArrayList();
    l.add(new Symbol("message"));
    l.add("Hello %s");
    l.add(new Symbol("user-full-name"));
    lwriter.writeForm(l);
    assertEquals("(message \"Hello %s\" user-full-name)", output.toString());
    reset();
    lwriter.writeUnknown(l);
    assertEquals("'(message \"Hello %s\" user-full-name)", output.toString());
  }

  public void testQuoted1() {
    List l = new ArrayList();
    l.add(new Symbol("apply"));
    l.add(new Quoted(new Symbol("+")));
    l.add(new Integer(1));
    l.add(new Integer(2));
    List inner = new ArrayList();
    inner.add(new Integer(3));
    inner.add(new Integer(4));
    l.add(new Quoted(inner));
    lwriter.writeForm(l);
    assertEquals("(apply '+ 1 2 '(3 4))", output.toString());
    reset();
    lwriter.writeUnknown(l);
    assertEquals("'(apply '+ 1 2 '(3 4))", output.toString());
  }

  public void testWriteJdeJuciInvokeElispForm() {
    List eval = new ArrayList();
    eval.add(new Symbol("jde-juci-invoke-elisp"));

    List form = new ArrayList();
    form.add(new Symbol("message"));
    form.addAll(Arrays.asList(new Object[] {"hello %s", "nick"}));

    eval.add(form);
    lwriter.writeForm(eval);
    assertEquals("(jde-juci-invoke-elisp '(message \"hello %s\" \"nick\"))", output.toString());
  }

}

// LispWriterTest.java ends here
