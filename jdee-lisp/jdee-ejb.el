;;; jdee-ejb.el -- EJB Extensions to Java Development Environment for Emacs
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

;; FIXME: this gives a byte-compile warning "Error: Selecting deleted buffer"
(require 'jdee-gen)
(require 'tempo)

(defgroup jdee-ejb nil
  "JDE EJB Electric Class Builder"
  :group 'jdee
  :prefix "jdee-ejb-")

(defvar jdee-current-ejb-name ""
"Name used by all EJB components.")

(defvar jdee-current-ejb-package ""
"Package that contains all EJB components.")

(defvar jdee-ejb-dir nil
  "Directory containing all EJB components.")


;; (makunbound 'jdee-ejb-remote-format)
(defcustom jdee-ejb-remote-format "%s.java"
  "*Default format for EJB Remote Interface"
  :type 'string
  :group 'jdee-ejb)

;; (makunbound 'jdee-ejb-home-format)
(defcustom jdee-ejb-home-format "%sHome.java"
  "*Default format for EJB Home Interface
Setting this also resets jdee-ejb-home to the
name portion of the filename string."
  :type 'string
  :group 'jdee-ejb
  :set '(lambda (sym val)
	  (set-default 'jdee-ejb-home
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jdee-ejb-local-format)
(defcustom jdee-ejb-local-format "%sLocal.java"
  "*Default format for EJB Local Interface"
  :type 'string
  :group 'jdee-ejb
  :set '(lambda (sym val)
	  (set-default 'jdee-ejb-local
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jdee-ejb-local-home-format)
(defcustom jdee-ejb-local-home-format "%sLocalHome.java"
  "*Default format for EJB LocalHome Interface
Setting this also resets jdee-ejb-local-home to the
name portion of the filename string."
  :type 'string
  :group 'jdee-ejb
  :set '(lambda (sym val)
	  (set-default 'jdee-ejb-local-home
		      (file-name-sans-extension val))
	  (set-default sym val)))


;; (makunbound 'jdee-ejb-class-format)
(defcustom jdee-ejb-class-format "%sBean.java"
  "*Default format for EJB Class.
Setting this also resets jdee-ejb-class to the
name portion of the filename string."
  :type 'string
  :group 'jdee-ejb
  :set '(lambda (sym val)
	  (set-default 'jdee-ejb-class
		      (file-name-sans-extension val))
	  (set-default sym val)))

;; (makunbound 'jdee-ejb-descriptor-format)
(defcustom jdee-ejb-descriptor-format "%sEJB.xml"
  "*Default format for EJB Deployment Descriptor"
  :type 'string
  :group 'jdee-ejb)

(defun jdee-ejb-format-filename (fmt name &optional dir)
  (let ((thisdir (or dir default-directory)))
    (format "%s/%s" thisdir  (format fmt name))))

;; (makunbound 'jdee-ejb-remote-buffer-template)
(eval-and-compile
  (defcustom jdee-ejb-remote-buffer-template
    (list
     "(funcall jdee-gen-boilerplate-function)"
     "jdee-ejb-package '>'n"
     "'>'n"
     "\"public interface \""
     "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
     "\" extends javax.ejb.EJBObject \""

     "(if jdee-gen-k&r "
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
command `jdee-ejb-remote', as a side-effect."
    :group 'jdee-ejb
    :type '(repeat string)
    :set '(lambda (sym val)
	    (defalias 'jdee-ejb-remote
	      (tempo-define-template "java-ejb-remote-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic Java class buffer skeleton."))
	    (set-default sym val)))

  (defalias 'jdee-ejb-remote
    (tempo-define-template "java-ejb-remote-buffer-template"
			   (jdee-gen-read-template jdee-ejb-remote-buffer-template)
			   nil
			   "Insert a generic Java class buffer skeleton."))
  )

;; (makunbound 'jdee-ejb-home-buffer-template)
(eval-and-compile
  (defcustom jdee-ejb-home-buffer-template
    (list
     "(funcall jdee-gen-boilerplate-function)"
     "jdee-ejb-package '>'n"
     "'>'n"
     "\"public interface \""
     "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
     "\" extends javax.ejb.EJBHome \""

     "(if jdee-gen-k&r "
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
command `jdee-ejb-home', as a side-effect."
    :group 'jdee-ejb
    :type '(repeat string)
    :set '(lambda (sym val)
	    (defalias 'jdee-ejb-home
	      (tempo-define-template "java-ejb-home-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic Java class buffer skeleton."))
	    (set-default sym val)))

  (defalias 'jdee-ejb-home
    (tempo-define-template "java-ejb-home-buffer-template"
			   (jdee-gen-read-template jdee-ejb-home-buffer-template)
			   nil
			   "Insert a generic Java class buffer skeleton."))
  )

;;;;;;; start - by yoonforh 2003-01-15 17:09:14
;; (makunbound 'jdee-ejb-local-buffer-template)
(eval-and-compile
  (defcustom jdee-ejb-local-buffer-template
    (list
     "(funcall jdee-gen-boilerplate-function)"
     "jdee-ejb-package '>'n"
     "'>'n"
     "\"public interface \""
     "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
     "\" extends javax.ejb.EJBLocalObject \""

     "(if jdee-gen-k&r "
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
command `jdee-ejb-local', as a side-effect."
    :group 'jdee-ejb
    :type '(repeat string)
    :set '(lambda (sym val)
	    (defalias 'jdee-ejb-local
	      (tempo-define-template "java-ejb-local-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic Java class buffer skeleton."))
	    (set-default sym val)))

  (defalias 'jdee-ejb-local
    (tempo-define-template "java-ejb-local-buffer-template"
			   (jdee-gen-read-template jdee-ejb-local-buffer-template)
			   nil
			   "Insert a generic Java class buffer skeleton."))
  )

;; (makunbound 'jdee-ejb-local-home-buffer-template)
(eval-and-compile
  (defcustom jdee-ejb-local-home-buffer-template
    (list
     "(funcall jdee-gen-boilerplate-function)"
     "jdee-ejb-package '>'n"
     "'>'n"
     "\"public interface \""
     "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
     "\" extends javax.ejb.EJBLocalHome \""

     "(if jdee-gen-k&r "
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
command `jdee-ejb-local-home', as a side-effect."
    :group 'jdee-ejb
    :type '(repeat string)
    :set '(lambda (sym val)
	    (defalias 'jdee-ejb-local-home
	      (tempo-define-template "java-ejb-local-home-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic Java class buffer skeleton."))
	    (set-default sym val)))

  (defalias 'jdee-ejb-local-home
    (tempo-define-template "java-ejb-local-home-buffer-template"
			   (jdee-gen-read-template jdee-ejb-local-home-buffer-template)
			   nil
			   "Insert a generic Java class buffer skeleton."))
  )


;;;;;;;;;;; end - by yoonforh 2003-01-15 17:09:19

;; (makunbound 'jdee-ejb-entity-bean-template)
(defcustom jdee-ejb-entitiy-bean-template
  (list
   "(funcall jdee-gen-boilerplate-function)"
   "jdee-ejb-package '>'n"
    "'>"
   "'>'n"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   " \" implements EntityBean \" "

   "(if jdee-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n'n"
   "'>'p'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbCreate\""
    "  nil"
    "  \"CreateException\""
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"


    "(jdee-gen-method-signature"
    "   \"public\""
    "  \"void\""
    "  \"ejbActivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbPassivate\""
    "  nil"
    "  nil"
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbLoad\""
    "  nil"
    "  nil"
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbStore\""
    "  nil"
    "  nil"
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbRemove\""
    "  nil"
    "  \"RemoveException\""
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"setEntityContext\""
    "  \"EntityContext ctx\""
    "  nil"
    " )"

    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "'>"
    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"unsetEntityContext\""
    "  nil"
    "  nil"
    " )"

    "(if jdee-gen-k&r "
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
command `jdee-ejb-entity-bean', as a side-effect."
  :group 'jdee-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jdee-ejb-entity-bean
	    (tempo-define-template "java-ejb-entity-bean-template"
				   (jdee-gen-read-template val)
				   nil
				   "Insert a generic Entity Bean skeleton."))
	  (set-default sym val)))


;; (makunbound 'jdee-ejb-session-bean-template)
(defcustom jdee-ejb-session-bean-template
  (list
   "(funcall jdee-gen-boilerplate-function)"
   "jdee-ejb-package '>'n"
    "'>"
   "'>'n"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   " \" implements SessionBean \" "

    ;;we open the bracket according to k&r style or not
   "(if jdee-gen-k&r "
   " ()"
   " '>'n)"
   "\"{\"'>'n'n"
   "'>'p'n"

    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbCreate\""
    "  nil"
    "  \"CreateException\""
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbActivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbPassivate\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"ejbRemove\""
    "  nil"
    "  nil"
    " )"
    "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
    " ()"
    " 'n)"
    "\"{\"'>'n"
    "\"}\"'>'n 'n"

    "(jdee-gen-method-signature"
    "  \"public\""
    "  \"void\""
    "  \"setSessionContext\""
    "  \"SessionContext ctx\""
    "  nil"
    " )"
    "'>"

;;     ;;we open the bracket according to k&r style or not
;;     "(if jdee-gen-k&r "
;;     " ()"
;;     " 'n)"
;;     "\"{\"'>'n"
;;     "\"}\"'>'n 'n"

;;     "(jdee-gen-method-signature"
;;     "  \"public\""
;;     "  \"void\""
;;     "  \"unsetSessionContext\""
;;     "  nil"
;;     "  \"RemoteException\""
;;     " )"
;;     "'>"

    ;;we open the bracket according to k&r style or not
    "(if jdee-gen-k&r "
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
command `jdee-ejb-session-bean', as a side-effect."
  :group 'jdee-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jdee-ejb-session-bean
	    (tempo-define-template "java-ejb-session-bean-template"
				   (jdee-gen-read-template val)
				   nil
				   "Insert a generic Session Bean skeleton."))
	  (set-default sym val)))


;; (makunbound 'jdee-ejb-session-descriptor-buffer-template)
(defcustom jdee-ejb-session-descriptor-buffer-template
  (list
   "\"<?xml version=\\\"1.0\\\"?>\"'n"
   "\"<!DOCTYPE ejb-jar PUBLIC \\\"-//Sun Microsystems, Inc.//DTD Enterprise JavaBeans 2.0//EN\\\" \" "
   "\" \\\"http://java.sun.com/dtd/ejb-jar_2_0.dtd\\\" > \"'>'n'n"
   "\"<ejb-jar>\"'>'n"
   "\"<enterprise-beans>\" '>'n"
   "\"<session> \"'>'n"
   "\"<ejb-name>\""
   "(format jdee-ejb-class  jdee-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<home>\""
   "(format (concat \"%s.\" jdee-ejb-home) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</home>\"'>'n"
   "\"<remote>\""
   "(format \"%s.%s\" jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</remote>\"'>'n"
   "\"<local-home>\""
   "(format (concat \"%s.\" jdee-ejb-local-home) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</local-home>\"'>'n"
   "\"<local>\""
   "(format (concat \"%s.\" jdee-ejb-local) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</local>\"'>'n"
   "\"<ejb-class>\""
   "(format \"%s.%s\" jdee-current-ejb-package (format jdee-ejb-class  jdee-current-ejb-name))"
   "\"</ejb-class>\"'>'n"
   "\"<session-type>Stateless</session-type>\"'>'n"
   "\"<transaction-type>Container</transaction-type>\"'>'n"
   "\"</session>\"'>'n"
   "\"</enterprise-beans>\"'>'n"
   "\"<ejb-client-jar>\""
   "(format \"%sClient.jar\" jdee-current-ejb-name )"
   "\"</ejb-client-jar>\"'>'n"
   "\"</ejb-jar>\"'>'n"
   "'>'n")
  "*Template for new EJB Session Bean Deployment Descriptor interface.
This template uses internal functions to get the package and ejb names
interpolated when the XML file is generated from the template.
Setting this variable defines a template instantiation
command `jdee-ejb-session-descriptor', as a side-effect."
  :group 'jdee-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	    (defalias 'jdee-ejb-session-descriptor
	      (tempo-define-template "java-ejb-session-descriptor-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic XML Deployment Descriptor buffer skeleton."))
	    (set-default sym val)))

;; (makunbound 'jdee-ejb-entity-descriptor-buffer-template)
(defcustom jdee-ejb-entity-descriptor-buffer-template
  (list
   "\"<?xml version=\\\"1.0\\\"?>\"'n"
   "\"<!DOCTYPE ejb-jar PUBLIC \\\"-//Sun Microsystems, Inc.//DTD Enterprise JavaBeans 2.0//EN\\\" \" "
   "\" \\\"http://java.sun.com/dtd/ejb-jar_2_0.dtd\\\" > \"'>'n'n"
   "\"<ejb-jar>\"'>'n"
   "\"<enterprise-beans>\" '>'n"
   "\"<entity> \"'>'n"
   "\"<ejb-name>\""
   "(format jdee-ejb-class  jdee-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<home>\""
   "(format (concat \"%s.\" jdee-ejb-home) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</home>\"'>'n"
   "\"<remote>\""
   "(format \"%s.%s\" jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</remote>\"'>'n"
   "\"<local-home>\""
   "(format (concat \"%s.\" jdee-ejb-local-home) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</local-home>\"'>'n"
   "\"<local>\""
   "(format (concat \"%s.\" jdee-ejb-local) jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</local>\"'>'n"
   "\"<ejb-class>\""
   "(format \"%s.%s\" jdee-current-ejb-package (format jdee-ejb-class  jdee-current-ejb-name))"
   "\"</ejb-class>\"'>'n"
   "\"<persistence-type>Container</persistence-type>\"'>'n"
   "\"<prim-key-class>\""
   "(format \"%s.%sPK\" jdee-current-ejb-package jdee-current-ejb-name)"
   "\"</prim-key-class>\"'>'n"
   "\"<reentrant>False</reentrant>\"'>'n"
   "\"</entity>\"'>'n"
   "\"</enterprise-beans>\"'>'n"
   "\"<assembly-descriptor>\"'>'n"
   "\"<container-transaction>\"'>'n"
   "\"<method>\"'>'n"
   "\"<ejb-name>\""
   "(format jdee-ejb-class  jdee-current-ejb-name)"
   "\"</ejb-name>\"'>'n"
   "\"<method-intf>Local</method-intf>\"'>'n"
   "\"<method-name>*</method-name>\"'>'n"
   "\"</method>\"'>'n"
   "\"<method>\"'>'n"
   "\"<ejb-name>\""
   "(format jdee-ejb-class  jdee-current-ejb-name)"
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
command `jdee-ejb-session-descriptor', as a side-effect."
  :group 'jdee-ejb
  :type '(repeat string)
  :set '(lambda (sym val)
	    (defalias 'jdee-ejb-entity-descriptor
	      (tempo-define-template "java-ejb-entity-descriptor-buffer-template"
				     (jdee-gen-read-template val)
				     nil
				     "Insert a generic XML Deployment Descriptor buffer skeleton."))
	    (set-default sym val)))

;;;###autoload
(defun jdee-ejb-session-bean-buffer (ejb-name)
  "Create a new Java buffer containing an EJB session bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jdee-ejb templates.  This includes naming the files according
to the EJB naming convention."
  (interactive
   (let* ((insert-default-directory t)
	  (file (read-file-name "EJB Name (no extension): ")))
     (setq jdee-ejb-dir  (file-name-directory file))
     (list (file-name-sans-extension (file-name-nondirectory file)))))

  ;; Find the package name
  (setq jdee-current-ejb-name ejb-name)
  (jdee-ejb-gen-bean 'session))

;;;###autoload
(defun jdee-ejb-entity-bean-buffer ( ejb-name)
  "Create a new Java buffer containing an EJB entity bean class of the same name.
This command also creates buffers with the EJB Home and EJB Remote interfaces
and the XML Deployment descriptor defined
by the jdee-ejb templates.  This includes naming the files according
to the EJB naming convention."
  (interactive
   (let* ((insert-default-directory t)
	  (file (read-file-name "EJB Name (no extension): ")))
     (setq jdee-ejb-dir  (file-name-directory file))
     (list (file-name-sans-extension (file-name-nondirectory file)))))

  ;; Find the package name
  (setq jdee-current-ejb-name ejb-name)
  (jdee-ejb-gen-bean 'entity))

(defun jdee-ejb-gen-bean (beantype)
  "Internal function used by session and entity bean creators.
This command uses jde package wizards and template commands to build
the bean skeleton using the Bean name and package name supplied by the
Bean-specific interactive function"
(let* ((jdee-ejb-package (jdee-gen-get-package-statement))
       (jdee-bean (format "jdee-ejb-%s-bean" beantype))
       (jdee-desc (format "jdee-ejb-%s-descriptor" beantype)))

;; We use the package to generate a default directory
  (setq jdee-current-ejb-package   (cadr (split-string jdee-ejb-package "[ ;]+")))
  (find-file (jdee-ejb-format-filename jdee-ejb-remote-format jdee-current-ejb-name jdee-ejb-dir))
  (tempo-template-java-ejb-remote-buffer-template)
  (find-file (jdee-ejb-format-filename jdee-ejb-home-format jdee-current-ejb-name jdee-ejb-dir ))
  (tempo-template-java-ejb-home-buffer-template)
  (find-file (jdee-ejb-format-filename jdee-ejb-local-format jdee-current-ejb-name jdee-ejb-dir))
  (tempo-template-java-ejb-local-buffer-template)
  (find-file (jdee-ejb-format-filename jdee-ejb-local-home-format jdee-current-ejb-name jdee-ejb-dir ))
  (tempo-template-java-ejb-local-home-buffer-template)
  (find-file (jdee-ejb-format-filename jdee-ejb-descriptor-format jdee-current-ejb-name jdee-ejb-dir))
  (funcall (intern-soft jdee-desc))
  (find-file (jdee-ejb-format-filename jdee-ejb-class-format jdee-current-ejb-name jdee-ejb-dir))
  (funcall (intern-soft jdee-bean))
  (goto-char (point-min))
  (search-forward "{")
  (backward-char 1)
  (require 'cc-cmds)
  (c-indent-exp)
  (tempo-forward-mark)))

(provide 'jdee-ejb)

;; End of jdee-ejb.el
