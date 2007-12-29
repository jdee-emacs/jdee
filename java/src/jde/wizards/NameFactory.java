/*
 * Copyright (c) Eric D. Friedman 1998-2002. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998-2002. All Rights Reserved.
 *
 * $Revision: 1.3 $ 
 * $Date: 2002/06/06 05:12:44 $ 
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
 * Interface for class that provides name for method
 * parameters.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.3 $
 */
public interface NameFactory
{
  /**
   * Returns a unique (descriptive?) parameter name for the specified
   * type.
   *
   * @param sig  - signature of the declaring method
   * @param n    - the parameter number whose name we want.
   * @return a made up name for the n'th parameter
   */
  public String getParameterName(Signature sig, int n);
}

/*
 * $Log: NameFactory.java,v $
 * Revision 1.3  2002/06/06 05:12:44  paulk
 * DefaultNameFactory now generates meaningful method parameter names based
 * on the parameter type or the method name. Thanks to Ole Arndt.
 *
 *
 */

// End of NameFactory.java
