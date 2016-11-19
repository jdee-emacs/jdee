;;; jdee-test.el -- Integrated Development Environment for Java.

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004, 2005, 2008 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes
;; Copyright (C) 2006-2007 by Suraj Acharya

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

;; This is one of a set of packages that make up the Java Development
;; Environment (JDE) for Emacs. See the JDE User's Guide for more
;; information. It includes code for using the Eclipse compiler
;; originally written by Suraj Acharya.

;; This provides access to unit testing support

;; The latest version of the JDE is available at
;; <URL:http://jdee.sourceforge.net/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Landes <landes <at> mailc dt net>

;;; Code:

(defgroup jdee-test-options nil
  "JDE Unit Testing Options"
  :group 'jdee
  :prefix "jdee-test-option-")

(defcustom jdee-test-function 'jdee-junit-run
    "The function to run to do a unit test"
  :group 'jdee-compile-options
  :type 'function)

(defun jdee-test-function-default ()
  "Default unit test function.  
Currently just tells the user unit testing is not supported"
  (message "Unit test support is not yet implemented"))

;;;###autoload
(defun jdee-test-unittest ()
  "Perform unit test.  Delegates to the function specified by `jdee-test-function'."
  (interactive)
  (funcall jdee-test-function))

(provide 'jdee-test)

;;; jdee-test.el ends here
