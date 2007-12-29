package jde.util;

import java.util.*;

/**
 * A HashMap subclass which adds multiple values with the same key to
 * a List of values.  Single values remain single keys, obviating the
 * need to cart around single-element Lists.
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
 * Created: Tue Aug 14 19:36:50 2001
 *
 * @author Eric D. Friedman
 * @version $Id: MultiValueMap.java,v 1.1 2001/08/15 06:31:27 eric Exp $
 */

public class MultiValueMap extends HashMap {
    public MultiValueMap () {
        super();
    }

    /**
     * Convenience method for retrieving the value for
     * <code>key</code> as a List, even when there is only a single
     * value for that key in the map.  The EMPTY_LIST is returned when
     * no value is found for <code>key</code> making null checks
     * unnecessary.
     *
     * @param key an <code>Object</code> value
     * @return a <code>List</code> value
     */
    public List getAsList(Object key) {
        if (containsKey(key)) {
            Object o = get(key);
            if (o instanceof List) {
                return (List)o;
            } else {
                return Arrays.asList(new Object[] { o });
            } // end of else
        } else {
            return Collections.EMPTY_LIST;
        } // end of else
    }

    /**
     * inserts value into the map for key as a single element or, if
     * values already exist, as an entry in key's list.
     *
     * @param key an <code>Object</code> value
     * @param value an <code>Object</code> value
     * @return null
     */
    public Object put(Object key, Object value) {
        if (containsKey(key)) {
            Object other = get(key);
            if (other instanceof List) {
                ((List)other).add(value);
            } else {
                List l = new ArrayList();
                l.add(other);
                l.add(value);
                super.put(key, l);
            } 
        } else {
            super.put(key,value);
        }
        return null;
    }
}// MultiValueMap
