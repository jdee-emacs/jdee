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

/**
 * Represents a lisp symbol, such as a function name when appearing as the
 * first element of a list.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class Symbol
{
  private String name;

  /**
   * Creates a new <code>Symbol</code> instance.
   */
  public Symbol(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }

  public String toString() {
    return name;
  }

  /**
   * <p>Convert a camel-case java name (such as <code>jdeDoWorkNow</code>) to
   * an elisp name with constituents separated by dashes
   * (<code>jde-do-work-now</code>).</p>
   *
   * <p>In general, convert a possibly fully-qualified java name to an elisp
   * name, by the following algorithm:</p>
   *
   * <ul>
   *   <li>Convert all non-letters and digits to dashes ('-').</li>
   *   <li>Convert the transition from a lowercase letter to an uppercase
   *   letter to a dash.</li>
   *   <li>Downcase all letters.</li>
   * </ul>
   *
   * <p>For example, the following java names all convert to the elisp name
   * <code>jde-foo-call-left-right</code>:</p>
   *
   * <ul>
   *   <li><code>jdeFooCallLeftRight</code></li>
   *   <li><code>jde.foo.Call.leftRight</code></li>
   *   <li><code>jde.foo.CALL_LEFT_RIGHT</code></li>
   * </ul>
   *
   * @param javaName    a java name
   * @return            the converted elisp name
   */
  public static String java2Elisp(String javaName) {

    StringBuffer lispName        = new StringBuffer();
    boolean      lastCharWasDash = false;
    char         prev            = ' ';

    for (int i = 0; i < javaName.length(); i++) {

      char c = javaName.charAt(i);

      if (!Character.isLetterOrDigit(c)) {
        lispName.append('-');
        lastCharWasDash = true;
      } else {
        // add in a dash only if the last character was not a dash and we
        // didn't undergo a case change from lower to upper case
        if (i > 0 && !lastCharWasDash
            && Character.isLetter(prev)
            && Character.isLetter(c)
            && Character.isLowerCase(prev)
            && Character.isUpperCase(c)) {
          lispName.append('-');
        }

        lispName.append(Character.toLowerCase(c));
        lastCharWasDash = false;
      }
      prev = c;
    }

    return lispName.toString();
  }

}

// Symbol.java ends here
