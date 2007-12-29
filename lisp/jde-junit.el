;; jde-junit.el --- runs the junit test in the current buffer.
;; $Revision: 1.5.2.1 $

;; Author: Paul Kinnucan
;; Author: Torsten Geise <torsten.geise@freenet.de>
;; Maintainer: Paul Kinnucan
;; Keywords: tools, processes

;; Copyright (C) 2004, 2005, 2006 Paul Kinnucan, Torsten Geise

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is developed to perform junit test cases from within the 
;; current buffer. Most defuns are copied from jde sources and modified to
;; do the right things to JUnit. 
;; This package should be best integrated to the JDEE. That means this 
;; package uses the jde project files to store junit specific settings and
;; so on.

;; Please send any comments, bugs, or upgrade requests to
;; Torsten Geise at torsten.geise@freenet.de.


(require 'jde)

(defgroup jde-junit nil
  "JDE JUnit"
  :group 'jde
  :prefix "jde-junit-")

(defcustom jde-junit-working-directory ""
  "*Path of the working directory for the test run.
If you specify a path, the JDE launches the test run from the
directory specified by the path. Otherwise the test run will be launched 
from the current buffer's directory"
  :group 'jde-junit
  :type 'file)

;; (makunbound 'jde-junit-testrunner-type)
(defcustom jde-junit-testrunner-type "junit.textui.TestRunner"
  "Defines the test runner to be used. If you specify a custom
test runner, enter the class name of the test runner in the
edit field."
  :group 'jde-junit
  :tag "Test Runner"
  :type '(choice
	  (const :tag "Text UI" :value "junit.textui.TestRunner")
	  (const :tag "Swing GUI" :value "junit.swingui.TestRunner")
	  (const :tag "AWT GUI" :value "junit.awtui.TestRunner")
	  (string :tag "Custom UI")))

;; (makunbound 'jde-junit-tester-name-tag)
(defcustom jde-junit-tester-name-tag (cons "T" "prefix")
  "Specifies a tag appended or prefixed to the name of a testee class to
create the name of the corresponding tester class, e.g., T or Test, as
in TFoo or FooTest."
  :group 'jde-junit
  :tag "Test Class Name Tag"
  :type '(cons
	  (string :tag "Tag" :value "T")
	  (choice :tag "Tag Type"
	   (const :tag "Prefix" :value t)
	   (const :tag "Suffix" :value nil))))


;;JUnit templates

(defun jde-junit-get-tester-name (testee-name)
  "Gets the name of a tester class from the name
of the testee class by appending or prefixing
`jde-junit-tester-name-tag'."
  (let ((tag (car jde-junit-tester-name-tag))
	(prefixp (cdr jde-junit-tester-name-tag)))
  (if prefixp
      (concat tag testee-name)
    (concat testee-name tag))))

(defun jde-junit-get-testee-name (tester-name)
  "Gets the name of a testee class from the name
of the tester class by removing prefixed or
affixed `jde-junit-tester-name-tag'."
  (let ((tag (car jde-junit-tester-name-tag))
	(prefixp (cdr jde-junit-tester-name-tag)))
    (if prefixp
	(progn
	  (string-match 
	   (concat "^" tag "\\(.*\\)")
	   tester-name))
      (progn
	(string-match 
	 (concat tag "\\(.*\\)" tag "$")
	 tester-name)))
    (substring tester-name (match-beginning 1) (match-end 1))))


(defcustom jde-junit-test-class-template
  (list
   "(funcall jde-gen-boilerplate-function)"
   "(jde-gen-get-package-statement)"
   "\"import junit.framework.Test;\" '>'n"
   "\"import junit.framework.TestCase;\" '>'n"
   "\"import junit.framework.TestSuite;\" '>'n"
   "'n"
   "(progn (require 'jde-javadoc) (jde-javadoc-insert-start-block))"
   "\" * \""
   "\" Unit Test for class \""
   "(jde-junit-get-testee-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))) '>'n"
   "\" \" (jde-javadoc-insert-empty-line)"
   "\" \" (jde-javadoc-insert-empty-line)"
   "\" * Created: \" (current-time-string) '>'n"
   "\" \" (jde-javadoc-insert-empty-line)"
   "\" \" (jde-javadoc-insert 'tempo-template-jde-javadoc-author-tag)"
   "\" \" (jde-javadoc-insert 'tempo-template-jde-javadoc-version-tag)"
   "\" \" (jde-javadoc-insert 'tempo-template-jde-javadoc-end-block \"*/\")"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends TestCase \" "
    
   "(if jde-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"
   "'n" 
   
   " \" /** \" '>'n"
   " \"* Creates a new <code>\""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\"</code> instance.\" '>'n"
   " \"*\" '>'n" 
   " \"* @param name test name\" '>'n"
   " \"*/\"'>'n"

   "\"public \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" (String name)\""

   "(if jde-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"
   "\"super(name);\"'>'n"

   "\"}\"'>"
   "'>'n"
   "'n"
    
   "\"/**\" '>'n"
   "\"* @return a <code>TestSuite</code>\" '>'n"
   "\"*/\" '>'n"
   "\"public static TestSuite suite()\" '>" 
    
   "(if jde-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"

   "\"TestSuite suite = new TestSuite ();\" '>'n"
   "'>'n"
   "\"return suite;\" '>'n"
   "\"}\"'>'n'n"
   
   "\"/** \" '>'n"
   "\"* Entry point \" '>'n"
   "\"*/ \" '>'n"
   "\"public static void main(String[] args) \""
   "(if jde-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"
   "\"junit.textui.TestRunner.run(suite());\"'>'n"
   "\"}\"'>'n"
   
   "\"}\">"
   "\"// \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new Java class.
Setting this variable defines a template instantiation
command `jde-junit-test-class', as a side-effect."
  :group 'jde-junit
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-junit-test-class-internal
	    (tempo-define-template
             "java-junit-test-class-buffer-template"
             (jde-gen-read-template val)
             nil
             "Insert a generic JUnit test class buffer skeleton."))
	  (set-default sym val)))

;;;###autoload
(defun jde-junit-test-class ()
  "Instantiate a test class template."
  (interactive)
  (jde-junit-test-class-internal))

;;;###autoload
(defun jde-junit-test-class-buffer ()
  "Create a buffer containing a skeleton unit test class having the same name as the
root name of the buffer. This command prompts you to enter the file name
of the test class. It assumes that the file name has the form CLASSTest.java
where CLASS is the name of the class to be tested, e.g., MyAppTest.java. Use 
`jde-gen-junit-add-test-to-suite' to add tests to the test suite. Use of
tests generated with this template requires the JUnit test framework. For
more information, see http://www.junit.org."
  (interactive)
  (let ((tester-name
	 (jde-junit-get-tester-name 
	  (file-name-sans-extension 
	   (file-name-nondirectory buffer-file-name)))))
    (find-file (concat tester-name ".java"))
    (jde-junit-test-class-internal)
    (beginning-of-buffer)
    (search-forward "{")
    (backward-char 1)
    (c-indent-exp)
    (tempo-forward-mark)))

(defcustom jde-junit-add-test-to-suite-template
  '(
    "\"suite.addTest(new \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"(\\\"\" (P \"Test Name: \") \"\\\") \""
    
    "(if jde-gen-k&r "
    "()"
    "'>'n)"
    "\"{\"'>'n"
    
    "\"public void runTest()\""
    
    "(if jde-gen-k&r "
    "()"
    "'>'n)"
    "\"{\"'>'n"
    
    "(P \"Method to call: \") \"();\"'>'n"
    "\"}\"'>'n"
    "\"});\"'>'n"
   )
  "*Template for generating a test case for suite."
  :group 'jde-junit
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jde-junit-add-test-to-suite-internal
	    (tempo-define-template
	     "Adding JUnit test to suit"
	     (jde-gen-read-template val)
	     nil
	     "Insert JUnit test to suite."))
	  (set-default sym val)))


;;;###autoload
(defun jde-junit-add-test-to-suite ()
  "Instantiate an addTest method."
  (interactive)
  (jde-junit-add-test-to-suite-internal))


;;;###autoload
(defun jde-junit-run ()
  "Starts junit testrunner with buffer corresponding class name."
  (interactive)
   (if (equal major-mode 'jde-mode)
       (let ((vm (jde-run-get-vm))
	     (working-directory 
	      (if (string= jde-junit-working-directory "")
		  default-directory
		(jde-normalize-path 'jde-junit-working-directory))))
	 (oset vm :main-class jde-junit-testrunner-type )
	 (jde-run-set-app-args (concat (jde-db-get-package)
				       (file-name-sans-extension 
					(file-name-nondirectory (buffer-file-name)))))
	 (cd working-directory)
	 (jde-run-vm-launch vm))
     (error "The jde-junit-run command works only in a Java source buffer.")))

;;;###autoload
(defun jde-junit-show-options ()
  "Show the JDE JUnit Options panel."
  (interactive)
  (customize-apropos "jde-junit" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)



(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'test-jde-junit 'regression-suite t)
  (setq test-jde-junit
   '("test-jde-junit"
     ;; Each test in the suite is of the form:
     ;;   ([description] probe grader)
     ;;   DESCRIPTION - string
     ;;   PROBE -  a sexp which runs the actual test
     ;;   GRADER - the desired result or a sexp which determines
     ;;   how we did
     ("Test jde-junit-get-tester-name function"
      (jde-junit-get-tester-name "DynamicClassLoader")
      "TDynamicClassLoader"
      )
      )))


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (defun jde-junit-self-test () 
    "Runs jde-dbs self tests."
    (interactive)
    (apply 'regress 
	   (list test-jde-dbs-proc))))

(provide 'jde-junit)

;; Change History
;; $Log: jde-junit.el,v $
;; Revision 1.5.2.1  2006/03/05 03:49:50  paulk
;; Fix typo in jde-unit-get-testee-name. Thanks to Christophe Garion [garion@supaero.fr]
;;
;; Revision 1.5  2005/01/18 04:58:35  paulk
;; Fix a bug in jde-junit-run command that causes a Lisp error whenever it is run.
;;
;; Revision 1.4  2004/11/13 17:01:39  jslopez
;; Removes control characters.
;;
;; Revision 1.3  2004/10/18 04:20:15  paulk
;; Add unit test support.
;;

;; end of jde-junit.el
