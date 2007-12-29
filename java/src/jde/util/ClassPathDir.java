package jde.util;

import java.io.File;

/**
 * A ClassPathEntry that represents a directory in which classes are
 * stored.  This is scanned recursively for classes at load time.
 *
 * Copyright (C) 2001, 2002, 2003 Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
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
 * Created: Tue Aug 14 19:46:52 2001
 *
 * @author Eric D. Friedman
 * @version $Id: ClassPathDir.java,v 1.4 2003/10/20 03:55:49 paulk Exp $
 */

class ClassPathDir extends ClassPathEntry {
    private File directory;
    
    /**
     * Create an instance of ClassPathDir representing
     * <code>directory</code>
     *
     * @param directory a <code>File</code> value
     * @return a <code>ClassPathDir</code> value
     */
    ClassPathDir (File directory) {
        super();
        this.directory = directory;
    }

    /**
     * Perform a recursive scan of the directory and set the loaded
     * flag to true.
     *
     */
    void load() {
        addRecursively(directory,directory);
        setLoaded(true);
    }

    /**
     * Search for classes in <code>directory</code> rooted at
     * <code>rootDir</code>
     *
     * @param directory a <code>File</code> value
     * @param rootDir a <code>File</code> value
     */
    void addRecursively(File directory, File rootDir) {
        String [] files = directory.list();

        if (files == null) {
            System.err.println("Cannot read contents of " + directory + ".");
            return;
        } // end of if ()

        String current;
        String rootPath = rootDir.getPath();
        String currentPath = directory.getPath();
        String packageName = currentPath.substring(rootPath.length());
        StringBuffer buf = new StringBuffer();

        if (packageName.length() > 0) {
           // Not the current directory
           packageName = packageName.replace('\\', '.');
           packageName = packageName.replace('/', '.');
           packageName = packageName.substring(1);
        }
    
        for (int i = 0; i < files.length; i++) {
            current = files[i];
            if (current.toLowerCase().endsWith(".class")) {
                current = current.substring(0, current.length() - 6);
		current = current.replace('$', '.'); // To handle inner-class .class files
                if (currentPath.indexOf(rootPath) != 0) {
                    System.err.println("currentPath doesn't start with rootPath!\n"
                                       + "rootPath: " + rootPath + "\n"
                                       + "currentPath: " + currentPath + "\n");
                } else if (packageName.length() > 0) {
                    // not the default package
                    buf.append(packageName);
                    buf.append('.');
                    buf.append(current);
                    addClass(buf.toString());
                    buf.setLength(0);
                } else {
                    // The default package
                    addClass(current);
                }
            } else {
                // Check if it's a directory to recurse into
                File currentFile = new File(directory, current);
                if (currentFile.isDirectory()) {
                    addRecursively(currentFile,rootDir);
                }
            }
        }
    }

    /**
     * return the directory as our string value.
     *
     * @return a <code>String</code> value
     */
    public String toString() {
        return directory.toString();
    }
}// ClassPathDir

/*
 * $Log: ClassPathDir.java,v $
 * Revision 1.4  2003/10/20 03:55:49  paulk
 * Fix to handle inner classes.
 *
 *
 *
 */

// End of ClassPathDir.java
