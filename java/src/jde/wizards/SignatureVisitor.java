/*
 * Copyright (c) Paul Kinnucan 2003. All Rights Reserved.
 *
 * $Revision: 1.2 $ 
 * $Date: 2003/09/08 03:43:27 $ 
 *
 * SignatureHandler is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * SignatureHandler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */
package jde.wizards;

interface SignatureVisitor {

  void visit(Signature sig, boolean firstOfClass);
  
}

/*
 * $Log: SignatureVisitor.java,v $
 * Revision 1.2  2003/09/08 03:43:27  paulk
 * Remove DOS line endings.
 *
 * Revision 1.1  2003/09/07 05:32:01  paulk
 * Initial revision.
 *
 */

// End of SignatureVisitor.java
