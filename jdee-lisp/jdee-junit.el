;; jdee-junit.el --- runs the junit test in the current buffer.
;; $Id$

;; Author: Paul Kinnucan
;; Author: Torsten Geise <torsten.geise@freenet.de>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
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


(require 'jdee)
(require 'jdee-run);; jdee-run-get-vm jdee-run-vm-launch
(require 'jdee-db);; jdee-db-get-package

;; FIXME: (require 'cc-cmds) doesn't work
(declare-function c-indent-exp "cc-cmds" (&optional shutup-p))

(defgroup jdee-junit nil
  "JDE JUnit"
  :group 'jdee
  :prefix "jdee-junit-")

(defcustom jdee-junit-working-directory ""
  "*Path of the working directory for the test run.
If you specify a path, the JDE launches the test run from the
directory specified by the path. Otherwise the test run will be launched
from the current buffer's directory"
  :group 'jdee-junit
  :type 'file)

;; (makunbound 'jdee-junit-testrunner-type)
(defcustom jdee-junit-testrunner-type "junit.textui.TestRunner"
  "Defines the test runner to be used. If you specify a custom
test runner, enter the class name of the test runner in the
edit field."
  :group 'jdee-junit
  :tag "Test Runner"
  :type '(choice
	  (const :tag "Text UI" :value "junit.textui.TestRunner")
	  (const :tag "Swing GUI" :value "junit.swingui.TestRunner")
	  (const :tag "AWT GUI" :value "junit.awtui.TestRunner")
	  (string :tag "Custom UI")))

;; (makunbound 'jdee-junit-tester-name-tag)
(defcustom jdee-junit-tester-name-tag (cons "T" "prefix")
  "Specifies a tag appended or prefixed to the name of a testee class to
create the name of the corresponding tester class, e.g., T or Test, as
in TFoo or FooTest."
  :group 'jdee-junit
  :tag "Test Class Name Tag"
  :type '(cons
	  (string :tag "Tag" :value "T")
	  (choice :tag "Tag Type"
	   (const :tag "Prefix" :value t)
	   (const :tag "Suffix" :value nil))))


;;JUnit templates

(defun jdee-junit-get-tester-name (testee-name)
  "Gets the name of a tester class from the name
of the testee class by appending or prefixing
`jdee-junit-tester-name-tag'."
  (let ((tag (car jdee-junit-tester-name-tag))
	(prefixp (cdr jdee-junit-tester-name-tag)))
  (if prefixp
      (concat tag testee-name)
    (concat testee-name tag))))

(defun jdee-junit-get-testee-name (tester-name)
  "Gets the name of a testee class from the name
of the tester class by removing prefixed or
affixed `jdee-junit-tester-name-tag'."
  (let ((tag (car jdee-junit-tester-name-tag))
	(prefixp (cdr jdee-junit-tester-name-tag)))
    (if prefixp
	(progn
	  (string-match
	   (concat "^" tag "\\(.*\\)")
	   tester-name))
      (progn
	(string-match
	 (concat "\\(.*\\)" tag "$")
	 tester-name)))
    (substring tester-name (match-beginning 1) (match-end 1))))


(defcustom jdee-junit-test-class-template
  (list
   "(funcall jdee-gen-boilerplate-function)"
   "(jdee-gen-get-package-statement)"
   "\"import junit.framework.Test;\" '>'n"
   "\"import junit.framework.TestCase;\" '>'n"
   "\"import junit.framework.TestSuite;\" '>'n"
   "'n"
   "(progn (require 'jdee-javadoc) (jdee-javadoc-insert-start-block))"
   "\" * \""
   "\" Unit Test for class \""
   "(jdee-junit-get-testee-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))) '>'n"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" * Created: \" (current-time-string) '>'n"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-author-tag)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-version-tag)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-end-block \"*/\")"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" extends TestCase \" "

   "(if jdee-gen-k&r "
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

   "(if jdee-gen-k&r "
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
   "\"public static TestSuite suite() \" '>"

   "(if jdee-gen-k&r "
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
   "(if jdee-gen-k&r "
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
command `jdee-junit-test-class', as a side-effect."
  :group 'jdee-junit
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jdee-junit-test-class-internal
	    (tempo-define-template
	     "java-junit-test-class-buffer-template"
	     (jdee-gen-read-template val)
	     nil
	     "Insert a generic JUnit test class buffer skeleton."))
	  (set-default sym val)))

(defalias 'jdee-junit-test-class-internal
  (tempo-define-template
   "java-junit-test-class-buffer-template"
   (jdee-gen-read-template jdee-junit-test-class-template)
   nil
   "Insert a generic JUnit test class buffer skeleton."))

(defcustom jdee-junit4-test-class-template
  (list
   "(funcall jdee-gen-boilerplate-function)"
   "(jdee-gen-get-package-statement)"
   "\"import junit.framework.JUnit4TestAdapter;\" '>'n"
   "\"import org.junit.Assert;\" '>'n"
   "\"import static org.junit.Assert.*;\" '>'n"
   "\"import org.junit.Test;\" '>'n"
   "'n"
   "(progn (require 'jdee-javadoc) (jdee-javadoc-insert-start-block))"
   "\" * \""
   "\" Unit Test for class \""
   "(jdee-junit-get-testee-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))) '>'n"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" * Created: \" (current-time-string) '>'n"
   "\" \" (jdee-javadoc-insert-empty-line)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-author-tag)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-version-tag)"
   "\" \" (jdee-javadoc-insert 'tempo-template-jdee-javadoc-end-block \"*/\")"
   "\"public class \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\" \" "

   "(if jdee-gen-k&r "
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
   "\"() \""

   "(if jdee-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"

   "\"}\"'>"
   "'>'n"
   "'n"
   "\"/**\" '>'n"
   "\"* @return a <code>TestMethod</code>\" '>'n"
   "\"*/\" '>'n"
   "\"@Test\" '>'n"
   "\"public void testMethod() \" '>"

   "(if jdee-gen-k&r "
   "() "
   "'>'n)"
   "\"{\"'>'n"
   "\"}\"'>'n'n"

   "\"/** \" '>'n"
   "\"* Test Adapter \" '>'n"
   "\"*/ \" '>'n"
   "\"public static junit.framework.Test suite() \""
   "(if jdee-gen-k&r "
   "()"
   "'>'n)"
   "\"{\"'>'n"
   "\"return new JUnit4TestAdapter(\""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "\".class);\"'>'n"
   "\"}\"'>'n"

   "\"}\">"
   "\"// \""
   "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
   "'>'n")
  "*Template for new Java class.
Setting this variable defines a template instantiation
command `jdee-junit4-test-class', as a side-effect."
  :group 'jdee-junit
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jdee-junit4-test-class-internal
	    (tempo-define-template
	     "java-junit4-test-class-buffer-template"
	     (jdee-gen-read-template val)
	     nil
	     "Insert a generic JUnit 4 test class buffer skeleton."))
	  (set-default sym val)))

(defalias 'jdee-junit4-test-class-internal
  (tempo-define-template
   "java-junit4-test-class-buffer-template"
   (jdee-gen-read-template jdee-junit4-test-class-template)
   nil
   "Insert a generic JUnit 4 test class buffer skeleton."))

;;;###autoload
(defun jdee-junit-test-class ()
  "Instantiate a test class template."
  (interactive)
  (jdee-junit-test-class-internal))

;;;###autoload
(defun jdee-junit4-test-class ()
  "Instantiate a test class template."
  (interactive)
  (jdee-junit4-test-class-internal))

;;;###autoload
(defun jdee-junit-test-class-buffer ()
  "Create a buffer containing a skeleton unit test class having the same name as the
root name of the buffer. This command prompts you to enter the file name
of the test class. It assumes that the file name has the form CLASSTest.java
where CLASS is the name of the class to be tested, e.g., MyAppTest.java. Use
`jdee-gen-junit-add-test-to-suite' to add tests to the test suite. Use of
tests generated with this template requires the JUnit test framework. For
more information, see http://www.junit.org."
  (interactive)
  (let ((tester-name
	 (jdee-junit-get-tester-name
	  (file-name-sans-extension
	   (file-name-nondirectory buffer-file-name)))))
    (find-file (concat tester-name ".java"))
    (jdee-junit-test-class-internal)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char 1)
    (c-indent-exp)
    (tempo-forward-mark)))

;;;###autoload
(defun jdee-junit4-test-class-buffer ()
  "Create a buffer containing a skeleton unit test class having
the same name as the root name of the buffer. This command
prompts you to enter the file name of the test class. It assumes
that the file name has the form CLASSTest.java where CLASS is the
name of the class to be tested, e.g., MyAppTest.java. Use of
tests generated with this template requires the JUnit test
framework. For more information, see http://www.junit.org."
  (interactive)
  (let ((tester-name
	 (jdee-junit-get-tester-name
	  (file-name-sans-extension
	   (file-name-nondirectory buffer-file-name)))))
    (find-file (concat tester-name ".java"))
    (jdee-junit4-test-class-internal)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char 1)
    (c-indent-exp)
    (tempo-forward-mark)))

(defcustom jdee-junit-add-test-to-suite-template
  '(
    "\"suite.addTest(new \""
    "(file-name-sans-extension (file-name-nondirectory buffer-file-name))"
    "\"(\\\"\" (P \"Test Name: \") \"\\\") \""

    "(if jdee-gen-k&r "
    "()"
    "'>'n)"
    "\"{\"'>'n"

    "\"public void runTest()\""

    "(if jdee-gen-k&r "
    "()"
    "'>'n)"
    "\"{\"'>'n"

    "(P \"Method to call: \") \"();\"'>'n"
    "\"}\"'>'n"
    "\"});\"'>'n"
   )
  "*Template for generating a test case for suite."
  :group 'jdee-junit
  :type '(repeat string)
  :set '(lambda (sym val)
	  (defalias 'jdee-junit-add-test-to-suite-internal
	    (tempo-define-template
	     "Adding JUnit test to suit"
	     (jdee-gen-read-template val)
	     nil
	     "Insert JUnit test to suite."))
	  (set-default sym val)))

(defalias 'jdee-junit-add-test-to-suite-internal
  (tempo-define-template
   "Adding JUnit test to suit"
   (jdee-gen-read-template jdee-junit-add-test-to-suite-template)
   nil
   "Insert JUnit test to suite."))

;;;###autoload
(defun jdee-junit-add-test-to-suite ()
  "Instantiate an addTest method."
  (interactive)
  (jdee-junit-add-test-to-suite-internal))


;;;###autoload
(defun jdee-junit-run ()
  "Starts junit testrunner with buffer corresponding class name."
  (interactive)
   (if (equal major-mode 'jdee-mode)
       (let ((vm (jdee-run-get-vm))
	     (working-directory
	      (if (string= jdee-junit-working-directory "")
		  default-directory
		(jdee-normalize-path 'jdee-junit-working-directory))))
	 (oset vm :main-class jdee-junit-testrunner-type )
	 (jdee-run-set-app-args (concat (jdee-db-get-package)
				       (file-name-sans-extension
					(file-name-nondirectory (buffer-file-name)))))
	 (cd working-directory)
	 (jdee-run-vm-launch vm))
     (error "The jdee-junit-run command works only in a Java source buffer.")))

;;;###autoload
(defun jdee-junit-show-options ()
  "Show the JDE JUnit Options panel."
  (interactive)
  (customize-apropos "jdee-junit" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-junit)

;; End of jdee-junit.el
