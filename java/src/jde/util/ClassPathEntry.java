/* 
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
 */

package jde.util;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * An abstraction of ClassPath entries, which can concretely include
 * directories, zipfiles, and jarfiles, some of which are immutable
 * inasmuch as they are part of the JDK and are shared across projects
 * by virtue of being part of the boot classpath.
 *
 * <p>ClassPathEntries are Singleton/Flyweight implementations: they
 * can appear in multiple projects, but there is only ever one
 * instance.</p>
 *
 * Created: Tue Aug 14 19:28:04 2001
 *
 * @author Eric D. Friedman
 * @version $Id: ClassPathEntry.java,v 1.4 2002/09/06 12:57:55 jslopez Exp $
 */

abstract class ClassPathEntry {
    /** a map of unqualified class names to qualified names */
    protected MultiValueMap nameToClassMap = new MultiValueMap();
    /** a flag indicating whether this instance has been loaded or not */
    protected boolean loaded = false;
    private static Map entryMap = new HashMap();
    
    ClassPathEntry () {
    }

    /**
     * Returns the singleton/flyweight instance for
     * <code>resource</code>.  The specific instance type returned is
     * based on the extension of the file or on it being a directory.
     *
     * @param resource a <code>File</code> value
     * @return a <code>ClassPathEntry</code> value
     * @exception IOException if an error occurs
     * @exception IllegalArgumentException if resource is not a
     * zip/jar or a directory.
     */
    static ClassPathEntry instanceForEntry(File resource)
        throws IOException {
        ClassPathEntry entry = null;
        
        if (entryMap.containsKey(resource)) {
            entry = (ClassPathEntry)entryMap.get(resource);
        } else {
            if (resource.getName().toLowerCase().endsWith(".jar")) {
                entry = new ClassPathZip(resource);
            } else if (resource.getName().toLowerCase().endsWith(".zip")) {
                entry = new ClassPathZip(resource);
            } else if (resource.isDirectory()) {
                entry = new ClassPathDir(resource);
            } else {
                entry = null;   // shouldn't be in classpath
            } // end of else

            if (null != entry) {
                entryMap.put(resource,entry);
            }
        }
        return entry;
    }

    /**
     * defines class loading behavior.
     *
     * @exception IOException if an error occurs
     */
    abstract void load() throws IOException;

    /**
     * clears the class list in the entry.
     *
     */
    void clear() {
        nameToClassMap.clear();
        setLoaded(false);
    }

    /**
     * clears and reloads the class list in the entry.
     *
     * @exception IOException if an error occurs
     */
    void reload() throws IOException {
        clear();
        load();
    }

    /**
     * adds an unqualified => qualified mapping.
     *
     * @param qualifiedName a <code>String</code> value
     */
    protected void addClass(String qualifiedName) {
        String unqualified;

        int lastDot = qualifiedName.lastIndexOf('.');
        int innerClass = qualifiedName.indexOf('$');
        if (innerClass < 0) {
            unqualified = qualifiedName.substring(lastDot + 1);
        } else {
            unqualified = qualifiedName.substring(innerClass + 1);
        }
        nameToClassMap.put(unqualified, qualifiedName);
    }
    
    /**
     * Returns the list of qualified names that map to the specified
     * unqualified name.  Lazily loads the classes.
     *
     * @param unqualifiedName a <code>String</code> value
     * @return a <code>List</code> value
     * @exception IOException if an error occurs
     */
    List getClassNames(String unqualifiedName)
        throws IOException {
        if (! isLoaded()) {
            load();
        } // end of if (! isLoaded())
        
        return nameToClassMap.getAsList(unqualifiedName);
    }

    
    /**
     * Get the value of loaded.
     * @return value of loaded.
     */
    public boolean isLoaded() {
        return loaded;
    }

    /**
     * Set the value of loaded.
     *
     * @param loaded a <code>boolean</code> value
     */
    public void setLoaded(boolean loaded) {
        this.loaded = loaded;
    }
}// ClassPathEntry

/*
 * $Log: ClassPathEntry.java,v $
 * Revision 1.4  2002/09/06 12:57:55  jslopez
 * Fixes addClass method to handle inner classes in a friendlier manner.
 * The key for an inner class such as jde.util.A$B was A$B.
 * Now the key for such a class is only B.
 *
 * Revision 1.3  2001/10/17 04:09:33  paulk
 * Cosmetic changes to fit JDE coding style.
 *
 *
 */

// End of ClassPathEntry.java
