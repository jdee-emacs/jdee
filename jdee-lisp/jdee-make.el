;;; jdee-make.el -- make support for JDEE

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004 Paul Kinnucan.
;; Copyright (C) 2009 by Paul Landes

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

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'jdee-files)
(require 'jdee-project-file)

(defgroup jdee-make nil
  "JDEE Make Interface"
  :group 'jdee
  :prefix "jdee-make-")

(defcustom jdee-make-program "make"
  "*Specifies name of make program."
 :group 'jdee-make
 :type 'string)

(defcustom jdee-make-working-directory ""
  "*Path of the working directory to use in 'make' build mode. This
string must end in a slash, for example, c:/foo/bar/ or ./  .
If this string is empty, the 'make' build mode uses the current file
location as its working directory."
  :group 'jdee-make
  :type 'string)

(defcustom jdee-make-enable-find nil
"*Specify whether jdee-make find the Makefile based on your current
directory. If non-nil, we will search up the directory hierarchy from the
current directory for the build definition file. Also note that, if non-nil,
this will relax the requirement for an explicit jde project file."
   :group 'jdee-make
   :type 'boolean)

(defcustom jdee-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jdee-make
  :type 'string)

(defcustom jdee-make-finish-hook
  '(jdee-compile-finish-refresh-speedbar jdee-compile-finish-update-class-info)
  "List of functions to be invoked when compilation of a
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string
describing how the compilation finished."
  :group 'jdee-make
  :type 'hook)

(defvar jdee-interactive-make-args ""
"String of compiler arguments entered in the minibuffer.")

(defcustom jdee-read-make-args nil
"*Specify whether to prompt for additional make arguments.
If this variable is non-nil, and if `jdee-build-use-make' is non nil
the jdee-build command prompts you to enter additional make
arguments in the minibuffer. These arguments are appended to those
specified by customization variables. The JDE maintains a history
list of arguments entered in the minibuffer."
  :group 'jdee-make
  :type 'boolean
)


(defun jdee-make-make-command (more-args)
  "Constructs the java compile command as: jdee-compiler + options + buffer file name."
  (concat jdee-make-program " " jdee-make-args
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "))

(defun jdee-make-find-build-file (dir)
  "Find the next Makefile upwards in the directory tree from DIR.
Returns nil if it cannot find a project file in DIR or an ascendmake directory."
  (let ((file (cl-find "Makefile"
		       (directory-files dir) :test 'string=)))

    (if file
	(setq file (expand-file-name file dir))
      (if (not (jdee-root-dir-p dir))
	  (setq file (jdee-make-find-build-file (concat dir "../")))))

    file))

;;;###autoload
(defun jdee-make ()
  "Run the make program specified by `jdee-make-program' with the
command-line arguments specified by `jdee-make-args'. If
`jdee-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jdee-make-args'."
  (interactive)
  (if jdee-read-make-args
      (setq jdee-interactive-make-args
            (read-from-minibuffer
             "Make args: "
             jdee-interactive-make-args
             nil nil
             '(jdee-interactive-make-arg-history . 1)))
    (setq jdee-interactive-make-args ""))

  (let ((make-command
	 (jdee-make-make-command
	  jdee-interactive-make-args))
	(save-default-directory default-directory)
	(default-directory
	  (if (string= jdee-make-working-directory "")
	      (if jdee-make-enable-find
		  (let ((jdee-make-buildfile
			 (jdee-make-find-build-file default-directory)))
		    (if jdee-make-buildfile
			(file-name-directory jdee-make-buildfile)
		      default-directory))
		default-directory)
	    (jdee-normalize-path 'jdee-make-working-directory))))


    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jdee-make from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (eq system-type 'windows-nt)
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jdee-make
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-functions
          (lambda (buf msg)
            (run-hook-with-args 'jdee-make-finish-hook buf msg)
            (setq compilation-finish-functions nil)))

    (cd default-directory)
    (compilation-start make-command)
    (cd save-default-directory)))

;;;###autoload
(defun jdee-make-show-options ()
  "Show the JDEE Make Options panel."
  (interactive)
  (customize-apropos "jdee-make" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-make)

;;; jdee-make.el ends here
