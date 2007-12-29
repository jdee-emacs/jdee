/*
 * $Id: JDESecurityManager.java,v 1.6 2001/11/08 12:45:10 paulk Exp $
 */
package jde.util;

import java.lang.SecurityException;
import java.lang.SecurityManager;
import java.security.Permission;


/**
 * JDESecurityManager.java
 *
 *    AntServer.java
 *    Copyright (C) 2001 Javier Lopez (jslopez@alum.mit.edu)
 *
 *    $Revision: 1.6 $
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
4 * Created: Tue Oct 30 20:42:21 2001
 *
 * @author <a href="mailto:jslopez@forumsys.com">Javier S. Lopez</a>
 * @version 1.0
 * @since jde-2.2.9beta5
 */
public class JDESecurityManager extends SecurityManager {
    public JDESecurityManager (){
        super();
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkExit(int param1) {
        if (param1 != 7) {
            throw new SecurityException();
        }
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkExec(String param1) {
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkRead(String param1) {
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkPermission(Permission param1) {
    }
    
    /**
     *
     */
    public void checkCreateClassLoader() {
    }
   
    /**
     *
     */
    public void checkPropertiesAccess() {
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkPropertyAccess(String param1) {
    }
    
    /**
     *
     * @param param1 <description>
     * @param param2 <description>
     */
    public void checkAccept(String param1, int param2) {
    }
    
    /**
     *
     * @param param1 <description>
     * @param param2 <description>
     */
    public void checkConnect(String param1, int param2) {
    }
    
    /**
     *
     * @param param1 <description>
     * @param param2 <description>
     * @param param3 <description>
     */
    public void checkConnect(String param1, int param2, Object param3) {
    }
    
    /**
     *
     * @param param1 <description>
     */
    public void checkListen(int param1) {
    }

} // JDESecuritMmanager

/*
 * $Log: JDESecurityManager.java,v $
 * Revision 1.6  2001/11/08 12:45:10  paulk
 * Updated to support Ant.
 *
 * Revision 1.5  2001/11/05 16:30:00  jslopez
 * Adds permissions to connect, accept and
 * list socket connections.
 *
 * Revision 1.4  2001/11/05 13:53:10  jslopez
 * Added more permissions.
 *
 * Revision 1.3  2001/11/05 12:29:45  jslopez
 * Replacing the copyright.
 *
 * Revision 1.2  2001/11/05 02:07:36  jslopez
 * Modified the checkExit to allow to quit the VM when the
 * exit code is 7.
 *
 * Revision 1.1  2001/11/05 01:35:18  jslopez
 * Initial development.
 *
 */
