package jde.util;

import java.io.*;
import java.util.*;

/**
 * A delegating ClassPathEntry which refuses to clear/reload its
 * delegate.  Used to wrap bootclasspath entries.  An implementation
 * of the Decorator pattern.
 *
 * Copyright (C) 2001 Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Created: Tuesday Aug 14 19:46:52 2001
 *
 * @author Eric D. Friedman
 * @version $Id: ImmutableClassPathEntry.java,v 1.1 2001/08/15 06:31:27 eric Exp $
 */

class ImmutableClassPathEntry extends ClassPathEntry {
    ClassPathEntry delegate;

    /**
     * Creates an instance of ImmutableClassPathEntry which delegates
     * all methods to delegate with the exception of clear/reload
     * requests.
     *
     * @param delegate a <code>ClassPathEntry</code> value
     * @return an <code>ImmutableClassPathEntry</code> value
     */
    ImmutableClassPathEntry (ClassPathEntry delegate){
        super();
        this.delegate = delegate;
    }

    /**
     * invoke load on the delegate if it isn't already loaded.
     *
     * @exception IOException if an error occurs
     */
    void load() throws IOException {
        if (! delegate.isLoaded()) {
            delegate.load();
        } // end of if (delegate.isLoaded())
    }

    /**
     * no-op
     *
     */
    void clear() {
        // no-op
    }

    /**
     * no-op
     *
     * @exception IOException if an error occurs
     */
    void reload() {
        // no-op
    }

    /**
     * forwarded to delegate.
     *
     * @param unqualifiedName a <code>String</code> value
     * @return a <code>List</code> value
     * @exception IOException if an error occurs
     */
    List getClassNames(String unqualifiedName)
        throws IOException {
        return delegate.getClassNames(unqualifiedName);
    }
    
}// ImmutableClassPathEntry
