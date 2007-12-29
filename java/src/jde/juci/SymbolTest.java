/*
 * Copyright (C) 2002 by Nick Sieger
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

import junit.framework.*;

/**
 * Test name conversion algorithms in the {@link Symbol} class.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class SymbolTest
    extends TestCase {
  /**
   * Creates a new <code>SymbolTest</code> instance.
   *
   * @param name a <code>String</code> value
   */
  public SymbolTest(String name) {
    super(name);
  }

  //______________________________________________________________________
  //
  // Testing methods

  public void testJava2Elisp1() {
    assertEquals("jde-foo-call-left-right", Symbol.java2Elisp("jdeFooCallLeftRight"));
  }

  public void testJava2Elisp2() {
    assertEquals("jde-foo-call-left-right", Symbol.java2Elisp("jde.foo.Call.leftRight"));
  }

  public void testJava2Elisp3() {
    assertEquals("jde-foo-call-left-right", Symbol.java2Elisp("jde.foo.CALL_LEFT_RIGHT"));
  }
}

// SymbolTest.java ends here
