/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.2 $ 
 * $Date: 1998/11/01 03:49:32 $ 
 *
 * InterfaceFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * InterfaceFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

/**
 * Defines interface for class registries.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.2 $
 */

public interface ClassRegistry  
{
  /** Register the specified class with this registry */
  public void registerImport (Class to_import);

  /** Get the NameFactory service for generating parameter names */
  public NameFactory getNameFactory();

} // ClassRegistry


// End of ClassRegistry.java
