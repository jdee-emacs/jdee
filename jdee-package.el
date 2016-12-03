;; jdee-package.el -- Java package statement generator
;; $Id$

;; Copyright (C) 1998, 2000, 2001, 2002 by David Ponce
;; Copyright (C) 2009 by Paul Landes

;; Author:     David Ponce <david@dponce.com>
;; Maintainer: David Ponce
;;             Paul Landes <landes <at> mailc dt net>
;; Created:    September 28 1998

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This package automatically generates a Java package statement.  The
;; package name is deducted from the current classpath setting of
;; JDE.  When a directory found in classpath is a root of the current
;; buffer default directory, the relative path of the default
;; directory from the classpath one becomes the package name by
;; substituting directory separators by '.'.
;;
;; For example:
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java")
;;
;;   For the file "~/java/FR/test/MyClass.java", the package name
;;   generated will be "FR.test".
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java" "~/java/test")
;;
;;   For the file "~/java/test/MyClass.java",
;;   `jdee-package-default-package-comment' will be generated because
;;   the default package can be used.
;;
;;   The JDE classpath setting is:
;;
;;    ("~/java")
;;
;;   For the file "/usr/java/MyClass.java", no package name will be
;;   generated because the directory "/usr/java" is not accessible
;;   from classpath.
;;
;; Usage:
;;
;; M-x `jdee-package-update' to update the Java package statement or
;; insert a new one at beginning of current buffer.
;;
;; The function `jdee-package-generate-package-statement' should be
;; used in Java template to automatically generate the package
;; statement.
;;
;; Customization:
;;
;; The following variables could be set:
;;
;; - `jdee-package-load-hook' hook run when package has been loaded.
;;
;; - `jdee-package-package-comment' Java line comment appended to the
;;   generated package statement.  Default is " // Generated package
;;   name"
;;
;; - `jdee-package-default-package-comment' Java line comment generated
;;   when the default package is used.  Default is "// Default package
;;   used"
;;
;; - `jdee-package-search-classpath-variables' list of variables where
;;   to search the current classpath list.  Default is
;;   '(`jdee-compile-option-classpath' `jdee-global-classpath')

;;; Code:

(require 'cl-lib)

;; FIXME: refactor
(defvar jdee-resolve-relative-paths-p);; jde
(defvar jdee-sourcepath);; jde
(declare-function jdee-normalize-path "jdee" (path &optional symbol))
(declare-function jdee-expand-wildcards-and-normalize "jdee" (path &optional symbol))

(defconst jdee-package-unknown-package-name
  "*unknown*"
  "The string returned when a package name can't be generated.")

(defconst jdee-package-package-regexp
  "package .*;.*$"
  "The regexp used to find the Java package statement.")

(defgroup jdee-package nil
  "jdee-package package customization"
  :group 'jdee
  :prefix "jdee-package-")

(defcustom jdee-package-load-hook nil
  "*Hook run when package has been loaded."
  :group 'jdee-package
  :type 'hook)

(defcustom jdee-package-package-comment
  ""
  "*Java line comment appended to the generated package statement.
An empty string suppress the generation of this comment."
  :group 'jdee-package
  :type 'string)

(defcustom jdee-package-default-package-comment
  ""
  "*Java line comment generated when the default package is used.
An empty string suppress the generation of this comment."
  :group 'jdee-package
  :type 'string)

(defcustom jdee-package-search-classpath-variables
  '(jdee-compile-option-classpath jdee-global-classpath)
  "*Specify the variables where to search the current classpath list.
The first one which has a non nil value will be used by jdee-package."
  :group 'jdee-package
  :type '(repeat variable))


(defun jdee-package-get-classpath ()
  "Return the current classpath list.
That is to say the first non-nil value found in the variables given by
`jdee-package-search-classpath-variables'."
  (let ((search-in jdee-package-search-classpath-variables)
	(classpath))
    (while (and search-in (not classpath))
      (setq classpath (symbol-value (car search-in))
	    search-in (cdr search-in)))
    classpath))

(defun jdee-package-get-directories-in-classpath ()
  "Return the list of directories found in classpath."
  (cl-mapcan
   (lambda (path)
       (if (or jdee-resolve-relative-paths-p
	      (not   (string= path "."))) ; "." is ignored in classpath
	   (let ((path (jdee-normalize-path path)))
	     (if (file-directory-p path)
		 (list (file-name-as-directory path))))))
   (jdee-package-get-classpath)))


(defun jdee-package-search-package-directories ()
  "Return a list of package directory candidates or nil if not found."
  (let ((dir (jdee-normalize-path default-directory))
	;; case-insensitive for Windows
	(case-fold-search (eq system-type 'windows-nt)))
    (cl-mapcan
     (lambda (root)
	 (let ((root (regexp-quote root)))
	   (message "Seaching %S in %S..." root dir)
	   (and (string-match root dir)
		(list (substring dir (match-end 0))))))
     (append (jdee-package-get-directories-in-classpath)
	     (mapcar
	      (lambda (p)
		(file-name-as-directory p))
	      (jdee-expand-wildcards-and-normalize jdee-sourcepath 'jdee-sourcepath))))))

(defun jdee-package-best-package-candidate (candidates)
  "Return the best package directory candidate from CANDIDATES.
The best is the shortest one that could be found."
  (car (sort candidates
	     (lambda (dir1 dir2)
		 (string-match (regexp-quote dir1) dir2)))))

(defun jdee-package-get-package-directory ()
  "Return the package directory, if found; otherwise,
`jdee-package-unknown-package-name'."
  (or (jdee-package-best-package-candidate
       (jdee-package-search-package-directories))
      jdee-package-unknown-package-name))


(defun jdee-package-to-slashes (package)
  (subst-char-in-string ?. ?/ package))

(defun jdee-package-convert-directory-to-package (dir)
  "Convert directory path DIR to a valid Java package name.
Replace ?/ by ?. and remove extra ?/ at end."
  (if (string= dir "")
      ""
    (subst-char-in-string
     ?/ ?.
     (substring (file-name-as-directory dir) 0 -1)
     t)))

(defun jdee-package-generate-package-statement ()
  "Generate a Java package statement for the Java source file in the
current buffer, if the package name can be determined; otherwise,
an empty string."
  (let* (
	 ;; The JDE always uses ?/ as directory separator so ensure
	 ;; [X]Emacs uses the same one when running on Windows!
	 (directory-sep-char ?/)
	 (package-name (jdee-package-get-package-directory)))
    (cond
     ((string= package-name jdee-package-unknown-package-name)
      (message
       "The current directory is not accessible from classpath.")
      "")
     ((string= package-name "")
      (message "Default package used.")
      jdee-package-default-package-comment)
     (t
      (message "package %s;%s"
	       (jdee-package-convert-directory-to-package package-name)
	       jdee-package-package-comment)))))

;;;###autoload
(defun jdee-package-update ()
  "Create or update the package statement in the current Java source
file buffer based on the file's location relative to the root of
the package directory as specified by one of the entries in
`jdee-package-search-classpath-variables' or `jdee-sourcepath'.
If these variables do not specify the root of the package directory,
this command does nothing. This command signals an error if the
 major mode of the current buffer is not `jdee-mode'."
  (interactive)
  (or (eq major-mode 'jdee-mode)
      (error "Invalid major mode found.  Must be `jdee-mode'"))
  (let ((package (jdee-package-generate-package-statement)))
    (unless (string= package "")
      (goto-char (point-min))
      (if (re-search-forward jdee-package-package-regexp nil t)
	  (replace-match package)
	(insert package)
	(newline)))))

;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-package)
(run-hooks 'jdee-package-load-hook)

;; End of jdee-package.el
