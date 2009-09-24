;;; jde-ejb.el -- EJB Extensions to Java Development Environment for Emacs
;; $Id$

;; Author: David T. Smith
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools, ejb

;; Copyright (C) 2002, 2003, 2004, David T. Smith
;; Copyright (C) 2009 by Paul Landes

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Gnu Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; This module uses the concept of an Enterprise Java Bean as a component
;; comprised of a class (xxxBean.java), a Remote Interface (xxx.java), a home
;; interface (xxxHome.java), and a deployment descriptor (xxxEJB.xml).  These
;; files are all interrelated and therefore, changes to one should be
;; propagated to all.  The obvious thing is to treat it as an entity
;; with appropriate linkages between the files.  In this version however,
;; I will simply provide a wizard to create all three elements and
;; leave referential integrity alone.

(require 'jde-wiz)
(require 'jde-gen)


(defgroup jde-ejb nil
  "JDE EJB Electric Class Builder"
  :group 'jde
  :prefix "jde-ejb-")

(defvar jde-current-ejb-name ""
"Name used by all EJB components.")

(defvar jde-current-ejb-package ""
"Package that contains all EJB components.")

(defvar jde-ejb-dir nil
  "Directory containing all EJB components.")


;; (makunbound 'jde-ejb-remote-format)
(defcustom jde-ejb-remote-format "%s.java"
  "*Default format for EJB Remote Interface"
  :type 'string
  :group 'jde-ejb)

;; (makunbound 'jde-ejb-home-format)
(defcustom jde-ejb-home-format "%sHome.java"
  "*Default format for EJB Home Interface
Setting this also resets jde-ejb-home to the
name portion of the filename string."
  :type 'string
  :group 'jde-ejb
  :set '(lambda (sym val)
	  (set-default 'jde-ejb-home
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jde-ejb-local-format)
(defcustom jde-ejb-local-format "%sLocal.java"
  "*Default format for EJB Local Interface"
  :type 'string
  :group 'jde-ejb
  :set '(lambda (sym val)
	  (set-default 'jde-ejb-local
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jde-ejb-local-home-format)
(defcustom jde-ejb-local-home-format "%sLocalHome.java"
  "*Default format for EJB LocalHome Interface
Setting this also resets jde-ejb-local-home to the
name portion of the filename string."
  :type 'string
  :group 'jde-ejb
  :set '(lambda (sym val)
	  (set-default 'jde-ejb-local-home
		      (file-name-sans-extension val))
	  (set-default sym val)))


;; (makunbound 'jde-ejb-class-format)
(defcustom jde-ejb-class-format "%sBean.java"
  "*Default format for EJB Class.
Setting this also resets jde-ejb-class to the
name portion of the filename string."
  :type 'string
  :group 'jde-ejb
  :set '(lambda (sym val)
	  (set-default 'jde-ejb-class
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jde-ejb-descriptor-format)
(defcustom jde-ejb-descriptor-format "%sEJB.xml"
  "*Default format for EJB Deployment Descriptor"
  :type 'string
  :group 'jde-ejb)

(defun jde-ejb-format-filename (fmt name &optional dir)
  (let ((thisdir (or dir default-directory)))
    (format "%s/%s" thisdir  (format fmt name))))

;; (makunbound 'jde-ejb-remote-buffer-template)
(defcustom jde-ejb-remote-buffer-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
   "'>'n"
   "\"public interface \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends javax.ejb.EJBObject \""

   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n"

;;;Add standard interface components for Remote Interface
   "'>'n"
   "\"}\">"
   "\" // \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new EJB Remote interface.
This is the interface that contains all user methods.
Setting this variable defines a template instantiation
command `jde-ejb-remote', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-remote
	    (tempo-define-template "java-ejb-remote-buffer-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Java class buffer skeleton."))
	  (set-default sym val)))

;; (makunbound 'jde-ejb-home-buffer-template)
(defcustom jde-ejb-home-buffer-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
   "'>'n"
   "\"public interface \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends javax.ejb.EJBHome \""

   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n"

;;;Add standard interface components for Home Interface
   "'>'n"
   "\"}\">"
   "\" // \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new EJB Home interface.
This interface defines the create/find (for entity beans)/remove
methods. Setting this variable defines a template instantiation
command `jde-ejb-home', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-home
	    (tempo-define-template "java-ejb-home-buffer-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Java class buffer skeleton."))
	  (set-default sym val)))

;;;;;;; start - by yoonforh 2003-01-15 17:09:14
;; (makunbound 'jde-ejb-local-buffer-template)
(defcustom jde-ejb-local-buffer-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
   "'>'n"
   "\"public interface \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends javax.ejb.EJBLocalObject \""

   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n"

;;;Add standard interface components for Remote Interface
   "'>'n"
   "\"}\">"
   "\" // \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new EJB Local interface.
This is the interface that contains all user methods.
Setting this variable defines a template instantiation
command `jde-ejb-local', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-local
	    (tempo-define-template "java-ejb-local-buffer-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Java class buffer skeleton."))
	  (set-default sym val)))

;; (makunbound 'jde-ejb-local-home-buffer-template)
(defcustom jde-ejb-local-home-buffer-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
   "'>'n"
   "\"public interface \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends javax.ejb.EJBLocalHome \""

   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n"

;;;Add standard interface components for LocalHome Interface
   "'>'n"
   "\"}\">"
   "\" // \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new EJB LocalHome interface.
This interface defines the create/find (for entity beans)/remove
methods. Setting this variable defines a template instantiation
command `jde-ejb-local-home', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-local-home
	    (tempo-define-template "java-ejb-local-home-buffer-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Java class buffer skeleton."))
	  (set-default sym val)))


;;;;;;;;;;; end - by yoonforh 2003-01-15 17:09:19

;; (makunbound 'jde-ejb-entity-bean-template)
(defcustom jde-ejb-entitiy-bean-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
    "'>"
   "'>'n"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   " \" implements EntityBean \" "

   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n'n"
   "'>'p'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbCreate\""
    "  nil"
    "  \"CreateException\""
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"


    "(jde-gen-method-signature"
    "   \"public\""
    "  \"void\""
    "  \"ejbActivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbPassivate\""
    "  nil"
    "  nil"
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbLoad\""
    "  nil"
    "  nil"
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbStore\""
    "  nil"
    "  nil"
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbRemove\""
    "  nil"
    "  \"RemoveException\""
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"setEntityContext\""
    "  \"EntityContext ctx\""
    "  nil"
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"unsetEntityContext\""
    "  nil"
    "  nil"
    " )"

    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n '>"
   "\"}\">"
   "\" // \""

   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new Entity Bean class.
Entity beans must have  a Find method using a primary key.
The method is defined in the Home interface and implemented
here under a slightly different name (see EJB specifications for details.
Similarly, any create methods defined in the Home interface
will also be instantiated here, but under a slightly different name.
Setting this variable defines a template instantiation
command `jde-ejb-entity-bean', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-entity-bean
	    (tempo-define-template "java-ejb-entity-bean-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Entity Bean skeleton."))
	  (set-default sym val)))


;; (makunbound 'jde-ejb-session-bean-template)
(defcustom jde-ejb-session-bean-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "jde-ejb-package '>'n"
    "'>"
   "'>'n"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   " \" implements SessionBean \" "

    ;;we open the bracket according to k&r style or not
   "(if jde-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n'n"
   "'>'p'n"

    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbCreate\""
    "  nil"
    "  \"CreateException\""
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbActivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbPassivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbRemove\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jde-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"setSessionContext\""
    "  \"SessionContext ctx\""
    "  nil"
    " )"
    "'>"

;;     ;;we open the bracket according to k&r style or not
;;     "(if jde-gen-k&r "
;;     " ()"
;;     " 'n)"
;;     "\"{\"'>'n"
;;     "\"}\"'>'n 'n"

;;     "(jde-gen-method-signature"
;;     "  \"public\""
;;     "  \"void\""
;;     "  \"unsetSessionContext\""
;;     "  nil"
;;     "  \"RemoteException\""
;;     " )"
;;     "'>"

    ;;we open the bracket according to k&r style or not
    "(if jde-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"
    "'>"
   "\"}\">"
   "\" // \""

   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new Session Bean class.
This creates the class for a session bean.  It includes the
necessary interface implementations.
Setting this variable defines a template instantiation
command `jde-ejb-session-bean', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-ejb-session-bean
	    (tempo-define-template "java-ejb-session-bean-template"
				   (jde-gen-read-template val)
				   nil
				   "Insert a generic Session Bean skeleton."))
	  (set-default sym val)))


;; (makunbound 'jde-ejb-session-descriptor-buffer-template)
(defcustom jde-ejb-session-descriptor-buffer-template
  (list
   "\"<?xml version=\\\"1.0\\\"?>\"'n"
   "\"<!DOCTYPE ejb-jar PUBLIC \\\"-//Sun Microsystems, Inc.//DTD Enterprise JavaBeans 2.0//EN\\\" \" "
   "\" \\\"http://java.sun.com/dtd/ejb-jar_2_0.dtd\\\" > \"'>'n'n"
   "\"<ejb-jar>\"'>'n"
   "\"<enterprise-beans>\" '>'n"
   "\"<session> \"'>'n"
   "\"<ejb-name>\""
   "(format jde-ejb-class  jde-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<home>\""
   "(format (concat \"%s.\" jde-ejb-home) jde-current-ejb-package jde-current-ejb-name)"
   "\"</home>\"'>'n"
   "\"<remote>\""
   "(format \"%s.%s\" jde-current-ejb-package jde-current-ejb-name)"
   "\"</remote>\"'>'n"
   "\"<local-home>\""
   "(format (concat \"%s.\" jde-ejb-local-home) jde-current-ejb-package jde-current-ejb-name)"
   "\"</local-home>\"'>'n"
   "\"<local>\""
   "(format (concat \"%s.\" jde-ejb-local) jde-current-ejb-package jde-current-ejb-name)"
   "\"</local>\"'>'n"
   "\"<ejb-class>\""
   "(format \"%s.%s\" jde-current-ejb-package (format jde-ejb-class  jde-current-ejb-name))"
   "\"</ejb-class>\"'>'n"
   "\"<session-type>Stateless</session-type>\"'>'n"
   "\"<transaction-type>Container</transaction-type>\"'>'n"
   "\"</session>\"'>'n"
   "\"</enterprise-beans>\"'>'n"
   "\"<ejb-client-jar>\""
   "(format \"%sClient.jar\" jde-current-ejb-name )"
   "\"</ejb-client-jar>\"'>'n"
   "\"</ejb-jar>\"'>'n"
   "'>'n")
  "*Template for new EJB Session Bean Deployment Descriptor interface.
This template uses internal functions to get the package and ejb names
interpolated when the XML file is generated from the template.
Setting this variable defines a template instantiation
command `jde-ejb-session-descriptor', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	    (defalias 'jde-ejb-session-descriptor
	      (tempo-define-template "java-ejb-session-descriptor-buffer-template"
				     (jde-gen-read-template val)
				     nil
				     "Insert a generic XML Deployment Descriptor buffer skeleton."))
	    (set-default sym val)))

;; (makunbound 'jde-ejb-entity-descriptor-buffer-template)
(defcustom jde-ejb-entity-descriptor-buffer-template
  (list
   "\"<?xml version=\\\"1.0\\\"?>\"'n"
   "\"<!DOCTYPE ejb-jar PUBLIC \\\"-//Sun Microsystems, Inc.//DTD Enterprise JavaBeans 2.0//EN\\\" \" "
   "\" \\\"http://java.sun.com/dtd/ejb-jar_2_0.dtd\\\" > \"'>'n'n"
   "\"<ejb-jar>\"'>'n"
   "\"<enterprise-beans>\" '>'n"
   "\"<entity> \"'>'n"
   "\"<ejb-name>\""
   "(format jde-ejb-class  jde-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<home>\""
   "(format (concat \"%s.\" jde-ejb-home) jde-current-ejb-package jde-current-ejb-name)"
   "\"</home>\"'>'n"
   "\"<remote>\""
   "(format \"%s.%s\" jde-current-ejb-package jde-current-ejb-name)"
   "\"</remote>\"'>'n"
   "\"<local-home>\""
   "(format (concat \"%s.\" jde-ejb-local-home) jde-current-ejb-package jde-current-ejb-name)"
   "\"</local-home>\"'>'n"
   "\"<local>\""
   "(format (concat \"%s.\" jde-ejb-local) jde-current-ejb-package jde-current-ejb-name)"
   "\"</local>\"'>'n"
   "\"<ejb-class>\""
   "(format \"%s.%s\" jde-current-ejb-package (format jde-ejb-class  jde-current-ejb-name))"
   "\"</ejb-class>\"'>'n"
   "\"<persistence-type>Container</persistence-type>\"'>'n"
   "\"<prim-key-class>\""
   "(format \"%s.%sPK\" jde-current-ejb-package jde-current-ejb-name)"
   "\"</prim-key-class>\"'>'n"
   "\"<reentrant>False</reentrant>\"'>'n"
   "\"</entity>\"'>'n"
   "\"</enterprise-beans>\"'>'n"
   "\"<assembly-descriptor>\"'>'n"
   "\"<container-transaction>\"'>'n"
   "\"<method>\"'>'n"
   "\"<ejb-name>\""
   "(format jde-ejb-class  jde-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<method-intf>Local</method-intf>\"'>'n"
   "\"<method-name>*</method-name>\"'>'n"
   "\"</method>\"'>'n"
   "\"<method>\"'>'n"
   "\"<ejb-name>\""
   "(format jde-ejb-class  jde-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<method-intf>Remote</method-intf>\"'>'n"
   "\"<method-name>*</method-name>\"'>'n"
   "\"</method>\"'>'n"
   "\"<trans-attribute>Required</trans-attribute>\"'>'n"
   "\"</container-transaction>\"'>'n"
   "\"</assembly-descriptor>\"'>'n"
   "\"</ejb-jar>\"'>'n"
   "'>'n")
  "*Template for new EJB Entity Bean Deployment Descriptor interface.
This template uses internal functions to get the package and ejb names
interpolated when the XML file is generated from the template.
Setting this variable defines a template instantiation
command `jde-ejb-session-descriptor', as a side-effect."
  :group 'jde-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	    (defalias 'jde-ejb-entity-descriptor
	      (tempo-define-template "java-ejb-entity-descriptor-buffer-template"
				     (jde-gen-read-template val)
				     nil
				     "Insert a generic XML Deployment Descriptor buffer skeleton."))
	    (set-default sym val)))

;;;###autoload
(defun jde-ejb-session-bean-buffer (ejb-name)
  "Create a new Java buffer containing an EJB session bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according
to the EJB naming convention."
  (interactive
   (let* ((insert-default-directory t)
	  (file (read-file-name "EJB Name (no extension): ")))
     (setq jde-ejb-dir  (file-name-directory file))
     (list (file-name-sans-extension (file-name-nondirectory file)))))

  ;; Find the package name
  (setq jde-current-ejb-name ejb-name)
  (jde-ejb-gen-bean 'session))

;;;###autoload
(defun jde-ejb-entity-bean-buffer ( ejb-name)
  "Create a new Java buffer containing an EJB entity bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jde-ejb templates.  This includes naming the files according
to the EJB naming convention."
  (interactive
   (let* ((insert-default-directory t)
	  (file (read-file-name "EJB Name (no extension): ")))
     (setq jde-ejb-dir  (file-name-directory file))
     (list (file-name-sans-extension (file-name-nondirectory file)))))

  ;; Find the package name
  (setq jde-current-ejb-name ejb-name)
  (jde-ejb-gen-bean 'entity))

(defun jde-ejb-gen-bean (beantype)
  "Internal function used by session and entity bean creators.
This command uses jde package wizards and template commands to build
the bean skeleton using the Bean name and package name supplied by the
Bean-specific interactive function"
(let* ((jde-ejb-package (jde-gen-get-package-statement))
       (jde-bean (format "jde-ejb-%s-bean" beantype))
       (jde-desc (format "jde-ejb-%s-descriptor" beantype)))

;; We use the package to generate a default directory
  (setq jde-current-ejb-package   (cadr (split-string jde-ejb-package "[ ;]+")))
  (find-file (jde-ejb-format-filename jde-ejb-remote-format jde-current-ejb-name jde-ejb-dir))
  (tempo-template-java-ejb-remote-buffer-template)
  (find-file (jde-ejb-format-filename jde-ejb-home-format jde-current-ejb-name jde-ejb-dir ))
  (tempo-template-java-ejb-home-buffer-template)
  (find-file (jde-ejb-format-filename jde-ejb-local-format jde-current-ejb-name jde-ejb-dir))
  (tempo-template-java-ejb-local-buffer-template)
  (find-file (jde-ejb-format-filename jde-ejb-local-home-format jde-current-ejb-name jde-ejb-dir ))
  (tempo-template-java-ejb-local-home-buffer-template)
  (find-file (jde-ejb-format-filename jde-ejb-descriptor-format jde-current-ejb-name jde-ejb-dir))
  (funcall (intern-soft jde-desc))
  (find-file (jde-ejb-format-filename jde-ejb-class-format jde-current-ejb-name jde-ejb-dir))
  (funcall (intern-soft jde-bean))
  (goto-char (point-min))
  (search-forward "{")
  (backward-char 1)
  (c-indent-exp)
  (tempo-forward-mark)))

(provide 'jde-ejb)

;; End of jde-ejb.el
