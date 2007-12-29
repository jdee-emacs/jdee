/*
 * Copyright (c) Paul Kinnucan 2003. All Rights Reserved.
 *
 * $Revision: 1.2 $ 
 * $Date: 2003/09/08 03:42:17 $ 
 *
 * SignatureContainer is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * SignatureContainer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */
package jde.wizards;

import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

/**
 * A container for method signatures. The signatures are stored in
 * vectors with their declaring classes as keys in a hashtable.  This
 * allows them to be pulled out in groups when we print, and keeping
 * the declaring class info lets us put in comments about where the
 * method (or method group) comes from.
 *
 * @author <a href="mailto:mschw@web.de">Martin Schwamberger</a>
 * @version $Revision: 1.2 $
 */
public class SignatureContainer {
  
  /**
   * A table with declaring classes as keys and vectors of method
   * signatures as values
   */
  private Hashtable classes = new Hashtable();
  
  /** 
   * Adds a signature to the container. Signatures are not
   * added if they're already registered because we do not want
   * duplicate method implementations even though a class might
   * implement interfaces that inherit from some common
   * super-interface.
   *
   * @param sig Signature to be stored in the signature table.
   */
  public final void add(Signature sig) {
    
    // There can be only one, even though declaring classes differ.
    if (!alreadyStored(sig)) {
      
      String declaring = sig.getDeclaringClass().getName();
      
      if (classes.containsKey(declaring)) {
          ((Vector) classes.get(declaring)).addElement(sig);
      } else {    
        Vector v = new Vector();
        v.addElement(sig);
        classes.put(declaring, v);
      }
    
    }
  }

  /**
   * Check whether container already contains signature.
   *
   * @param sig a <code>Signature</code> value
   * @return a <code>boolean</code> value
   */
  private final boolean alreadyStored(Signature sig) {
    Enumeration declaringClasses = classes.keys();
    boolean found = false;
    
    while (declaringClasses.hasMoreElements() && !found) {
      String interf = (String) declaringClasses.nextElement();
      Vector v = (Vector) classes.get(interf);
      found = v.contains(sig);
    }

    return found;
  }

  /** 
   * Clear the signature container.
   */
  public void clear() {
    classes.clear();
  }

  /**
   * True if this container is empty of signatures.
   *
   * @return a <code>boolean</code> value
   */
  public boolean isEmpty() {
    return classes.isEmpty();
  }

  /**
   * Visit each signature in the container.
   *
   * @param visitor the SignatureVisitor object visiting the signatures
   */
  public void visit(SignatureVisitor visitor) {
    
    Enumeration declaringClasses = classes.keys();
    while (declaringClasses.hasMoreElements()) {
      
      String className = (String) declaringClasses.nextElement();
      Vector v = (Vector) classes.get(className);
      boolean firstOfClass = true;
      Enumeration e = v.elements();
      
      while (e.hasMoreElements()) {
        Signature sig = (Signature) e.nextElement();
        visitor.visit(sig, firstOfClass);
        firstOfClass = false;
      }
    }
  }
}

/*
 * $Log: SignatureContainer.java,v $
 * Revision 1.2  2003/09/08 03:42:17  paulk
 * Remove DOS line endings.
 *
 * Revision 1.1  2003/09/07 05:32:01  paulk
 * Initial revision.
 *
 */

// End of SignatureContainer.java
