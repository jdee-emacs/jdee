;; jdee-junit.el --- runs the junit test in the current buffer.

;; Author: Paul Kinnucan
;; Author: Torsten Geise <torsten.geise@freenet.de>
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
;; current buffer.  Most defuns are copied from jde sources and modified to
;; do the right things to JUnit.
;; This package should be best integrated to the JDEE.  That means this
;; package uses the jde project files to store junit specific settings and
;; so on.

;;; Code:

(require 'jdee) ;; FIXME: removing this line breaks tests!
(require 'jdee-files)
(require 'jdee-gen)
(require 'jdee-parse);; jdee-parse-get-package
(require 'jdee-project-file)
(require 'jdee-run);; jdee-run-get-vm jdee-run-vm-launch

;; FIXME: (require 'cc-cmds) doesn't work
(declare-function c-indent-exp "cc-cmds" (&optional shutup-p))

(defgroup jdee-junit nil
  "JDEE JUnit"
  :group 'jdee
  :prefix "jdee-junit-")

(defcustom jdee-junit-test-class-generator 'jdee-junit4-test-class-internal
  "Which template to use to fill in a new unit test.
This is a function that takes no arguments and inserts the contents in to
the current buffer.  The tempo package makes a good template.
See `jdee-junit4-test-class-internal' as an example."
  :group 'jdee-junit
  :type 'symbol)

(defcustom jdee-junit-working-directory ""
  "Path of the working directory for the test run.
If you specify a path, the JDEE launches the test run from the
directory specified by the path.  Otherwise the test run will be
launched from the current buffer's directory"
  :group 'jdee-junit
  :type 'file)

(defcustom jdee-junit-testrunner-type "junit.textui.TestRunner"
  "Defines the test runner to be used.
If you specify a custom test runner, enter the class name
of the test runner in the edit field."
  :group 'jdee-junit
  :tag "Test Runner"
  :type '(choice
	  (const :tag "Text UI" :value "junit.textui.TestRunner")
	  (const :tag "Swing GUI" :value "junit.swingui.TestRunner")
	  (const :tag "AWT GUI" :value "junit.awtui.TestRunner")
	  (string :tag "Custom UI")))

(defcustom jdee-junit-tester-name-tag (cons "Test" nil)
  "Specifies a prefix or suffix to use in test class name.
It will be concatenated with tested class name, e.g.: T or Test, as in TFoo
or FooTest.  Having a test suffix plays nicely with `projectile-mode'."
  :group 'jdee-junit
  :tag "Test Class Name Tag"
  :type '(cons
	  (string :tag "Tag" :value "T")
	  (choice :tag "Tag Type"
                  (const :tag "Prefix" :value t)
                  (const :tag "Suffix" :value nil))))

;;JUnit templates

(defun jdee-junit-get-tester-name (testee-name)
  "Get the name of a tester class based on `TESTEE-NAME'.
The `testee-name' is joined with `jdee-junit-tester-name-tag'."
  (let ((tag (car jdee-junit-tester-name-tag))
	(prefixp (cdr jdee-junit-tester-name-tag)))
    (if prefixp
        (concat tag testee-name)
      (concat testee-name tag))))

(defun jdee-junit-get-testee-name (tester-name)
  "Get the name of a testee class from the `TESTER-NAME'.
The result comes from removing prefixed or affixed `jdee-junit-tester-name-tag'."
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
  "Template for new Java class.
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
   "\"import org.junit.Assert;\" '>'n"
   "\"import org.junit.Test;\" '>'n"
   "'n"
   "\"import static org.junit.Assert.*;\" '>'n"
   "\"import junit.framework.JUnit4TestAdapter;\" '>'n"
   "'n"
   "(progn (require 'jdee-javadoc) (jdee-javadoc-insert-start-block))"
   "\" * \""
   "\" Unit Test for class {@link \""
   "(jdee-junit-get-testee-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))) \"}.\" '> 'n"
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
   "\"* Sample unit test\" '>'n"
   "\"*/\" '>'n"
   "\"@Test\" '>'n"
   "\"public void testMethod() throws Exception \" '>"

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

(defvar jdee-junit-test-path "src/test/java")
(defvar jdee-junit-test-extension ".java")

(defun jdee-junit-test-class-dir ()
  "Return directory where test sources are located."
  (let*((prj-dir (file-name-directory jdee-current-project))
        (test-source-dir (expand-file-name jdee-gen-test-path prj-dir)))
    test-source-dir))

;;;###autoload
(defun jdee-junit-test-class-buffer ()
  "Create a buffer containing a skeleton unit test class.
The buffer name will have the same name as the root name of the buffer.
This command prompts you to enter the file name of the test class.  It assumes
that the file name has the form CLASSTest.java where CLASS is the name of the
class to be tested, e.g., MyAppTest.java.
Use `jdee-gen-junit-add-test-to-suite' to add tests to the test suite.  Use of
tests generated with this template requires the JUnit test framework."
  (interactive)
  (let* ((tester-name
          (jdee-junit-get-tester-name
           (file-name-sans-extension
            (file-name-nondirectory buffer-file-name))))
         (test-class-name (format "%s%s" tester-name jdee-junit-test-extension))
         (package (replace-regexp-in-string "[.]$" "" (jdee-parse-get-package)))
         (_ (jdee-gen-get-package-statement package)) ;; called to set jdee-gen-package-name
         (dir (expand-file-name (jdee-package-to-slashes jdee-gen-package-name) (jdee-junit-test-class-dir)))
         (full-path (expand-file-name test-class-name dir))
         (buf (or (get-buffer test-class-name)
                  (and (file-exists-p full-path) (find-file full-path)))))
    (if (and buf (< 0 (buffer-size buf)))
        (display-buffer buf)
      (let ((buf (get-buffer-create test-class-name)))
        (with-current-buffer buf
          (jdee-gen-get-package-statement package) ;; called to set jdee-gen-package-name
          (setq default-directory dir
                buffer-file-name full-path)
          (rename-buffer test-class-name)
          (funcall jdee-junit-test-class-generator)
          (set-auto-mode)
          (goto-char (point-min))
          (re-search-forward "@link")
          (c-indent-line)
          (re-search-forward "public class \\w+ {")
          (backward-char 1)
          (c-indent-exp)
          (tempo-forward-mark)
          (display-buffer (current-buffer)))))))

;;;###autoload
(defun jdee-junit4-test-class-buffer ()
  "Create a buffer containing a JUnit4 test skeleton."
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
  "Template for generating a test case for suite."
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
  "Start JUnit testrunner with buffer corresponding class name."
  (interactive)
  (if (equal major-mode 'jdee-mode)
      (let ((vm (jdee-run-get-vm))
            (working-directory
             (if (string= jdee-junit-working-directory "")
                 default-directory
               (jdee-normalize-path 'jdee-junit-working-directory))))
        (oset vm :main-class jdee-junit-testrunner-type )
        (jdee-run-set-app-args (concat (jdee-parse-get-package)
				       (file-name-sans-extension
					(file-name-nondirectory (buffer-file-name)))))
        (cd working-directory)
        (jdee-run-vm-launch vm))
    (error "The jdee-junit-run command works only in a Java source buffer")))

;;;###autoload
(defun jdee-junit-show-options ()
  "Show the JDEE JUnit Options panel."
  (interactive)
  (customize-apropos "jdee-junit" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-junit)

;;; jdee-junit.el ends here
