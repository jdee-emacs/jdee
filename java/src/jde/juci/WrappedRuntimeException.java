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

import java.io.PrintStream;
import java.io.PrintWriter;

/**
 * A nondescript, unchecked exception class that allows chaining/wrapping of
 * exception references so that wrapped and rethrown exceptions can preserve
 * the information from the original exception.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class WrappedRuntimeException
    extends RuntimeException {
    public WrappedRuntimeException() {}

    public WrappedRuntimeException(String msg) {
        super(msg);
    }

    public WrappedRuntimeException(Throwable t, String msg) {
        this(msg);
        wrappedException = t;
    }

    public WrappedRuntimeException(Throwable t) {
        wrappedException = t;
    }

    public void printStackTrace(PrintStream ps) {
        if (wrappedException != null) {
            synchronized (ps) {
                wrappedException.printStackTrace(ps);
                ps.println(super.toString());
            }
        }
        else {
            super.printStackTrace(ps);
        }
    }

    public void printStackTrace() {
        printStackTrace(System.err);
    }

    public void printStackTrace(PrintWriter pw) {
        if (wrappedException != null) {
            synchronized (pw) {
                wrappedException.printStackTrace(pw);
                pw.println(super.toString());
            }
        }
        else {
            super.printStackTrace(pw);
        }
    }

    public String getMessage() {
        String msg = super.getMessage();
        if (wrappedException != null) {
            msg = msg + "\n" + wrappedException.getMessage();
        }
        return msg;
    }

    public String getLocalizedMessage() {
        String msg = super.getLocalizedMessage();
        if (wrappedException != null) {
            msg = msg + "\n" + wrappedException.getLocalizedMessage();
        }
        return msg;
    }

    public Throwable getWrappedException () {
        return wrappedException;
    }

    private Throwable wrappedException = null;
}

// WrappedRuntimeException.java ends here
