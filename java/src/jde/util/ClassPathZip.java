package jde.util;

import java.util.*;
import java.util.zip.*;
import java.io.*;

/**
 * A ClassPathEntry representing a ZIP or JAR file.
 *
 * Copyright (C) 2001, 2002 Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
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
 * @version $Id: ClassPathZip.java,v 1.2 2002/11/30 04:36:06 paulk Exp $
 */

class ClassPathZip extends ClassPathEntry {
    private File zipOrJar;

    /**
     * Creates a ClassPathZip instance representing <code>zipOrJar</code>
     *
     * @param zipOrJar a <code>File</code> value
     * @return a <code>ClassPathZip</code> value
     * @exception IOException if an error occurs
     */
    ClassPathZip (File zipOrJar) {
        super();
        this.zipOrJar = zipOrJar;
    }

    /**
     * Load all of the classes in the zip/jar and set the loaded flag
     * to true.
     *
     * @exception IOException if an error occurs
     */
    void load() throws IOException {
        ZipFile zipFile = new ZipFile(zipOrJar);
        Enumeration en = zipFile.entries();
        while (en.hasMoreElements()) {
            ZipEntry zipEntry = (ZipEntry)en.nextElement();
            String current = zipEntry.getName();
            if (current.toLowerCase().endsWith(".class")) {
                current = current.substring(0, current.length() - 6);
                current = current.replace('/', '.');
                current = current.replace('\\', '.');
		current = current.replace('$', '.');
                super.addClass(current);
            }
        }
        setLoaded(true);
    }

    /**
     * Return the zip/jar name as our string.
     *
     * @return a <code>String</code> value
     */
    public String toString() {
        return zipOrJar.toString();
    }
}// ClassPathZip

/*
 * $Log: ClassPathZip.java,v $
 * Revision 1.2  2002/11/30 04:36:06  paulk
 * load() method now replaces a $ with a period (.) in class names. Thanks to Petter Mahlen.
 * 
 */

// End of ClassPathZip.java
