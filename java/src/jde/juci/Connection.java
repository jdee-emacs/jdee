/*
 * Copyright (C) 2002, 2003 by Nick Sieger
 *
 * $Revision: 1.2 $
 * $Date: 2003/02/17 06:10:30 $
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
 * <p>A <code>Connection</code> is the object that is registered in the
 * Beanshell and represents a session for two-way communication between
 * Emacs and Java code.</p>
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public interface Connection {
  /**
   * Indicates the start of the connection's session.
   */
  void begin();

  /**
   * Indicates the end of the connection's lifecycle.  Any remaining results
   * on the connection's result stack are flushed as lisp forms for Emacs to
   * evaluate.
   */
  void end();

  /**
   * Reset the connection, clearing the result stack.  Normally used to
   * clean up after an error.
   */
  void reset();

  /**
   * Add a value to the top of the connection's result stack.
   *
   * @param result a result to be exposed to the other side of the
   *               communication boundary
   */
  void pushResult(Object result);

  /**
   * Examine, but don't remove, the value at the top of the connection's
   * result stack.
   *
   * @return the result at the top of the stack
   * @exception NoMoreResultsException if the result stack is empty
   */
  Object peekResult();

  /**
   * Remove and return the value at the top of the connection's result
   * stack.
   *
   * @return the result at the top of the stack
   * @exception NoMoreResultsException if the result stack is empty
   */
  Object popResult();

  /**
   * Since standard output is used for communication between java code and
   * Emacs, debug logging cannot be done there.  With this method, a
   * filename for logging is set and logging is turned on for the
   * connection.
   *
   * @param filename a log filename
   */
  void setLoggerFilename(String filename);

  /**
   * Evaluate a Beanshell script statement over this connection, so that the
   * script code can itself interact with JUCI to make calls back to Elisp.
   *
   * @param statement a <code>String</code> value
   */
  void evalBshScript(String statement) throws Throwable;
}

// Connection.java ends here
