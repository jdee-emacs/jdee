;;; jde-make.el -- Integrated Development Environment for Java.
;; $Revision: 1.17 $ 

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004 Paul Kinnucan.

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

(require 'compile)

(defgroup jde-make nil
  "JDE Make Interface"
  :group 'jde
  :prefix "jde-make-")

(defcustom jde-make-program "make"
  "*Specifies name of make program."
 :group 'jde-make
 :type 'string)

(defcustom jde-make-working-directory ""
  "*Path of the working directory to use in 'make' build mode. This
string must end in a slash, for example, c:/foo/bar/ or ./  .
If this string is empty, the 'make' build mode uses the current file
location as its working directory."
  :group 'jde-make
  :type 'string)

(defcustom jde-make-args ""
  "*Specifies arguments to be passed to make program."
  :group 'jde-make
  :type 'string)

(defcustom jde-make-finish-hook 
  '(jde-compile-finish-refresh-speedbar jde-compile-finish-update-class-info)
  "List of functions to be invoked when compilation of a 
Java source file terminates. Each function should accept
two arguments: the compilation buffer and a string 
describing how the compilation finished."
  :group 'jde-make
  :type 'hook)

(defvar jde-interactive-make-args ""
"String of compiler arguments entered in the minibuffer.")

(defcustom jde-read-make-args nil
"*Specify whether to prompt for additional make arguments.
If this variable is non-nil, and if `jde-build-use-make' is non nil
the jde-build command prompts you to enter additional make
arguments in the minibuffer. These arguments are appended to those 
specified by customization variables. The JDE maintains a history 
list of arguments entered in the minibuffer."
  :group 'jde-make
  :type 'boolean
)


(defun jde-make-make-command (more-args)
  "Constructs the java compile command as: jde-compiler + options + buffer file name."
  (concat jde-make-program " " jde-make-args
	  (if (not (string= more-args ""))
	      (concat " " more-args))
	  " "))


;;;###autoload
(defun jde-make ()
  "Run the make program specified by `jde-make-program' with the
command-line arguments specified by `jde-make-args'. If
`jde-read-make-args' is nonnil, this command also prompts you to enter
make arguments in the minibuffer and passes any arguments that you
enter to the make program along with the arguments specified by
`jde-make-args'."
  (interactive)
  (if jde-read-make-args
      (setq jde-interactive-make-args
	      (read-from-minibuffer 
	       "Make args: "
	       jde-interactive-make-args
	       nil nil
	       '(jde-interactive-make-arg-history . 1))))

  (let ((make-command
	 (jde-make-make-command 
	  jde-interactive-make-args))
	(save-default-directory default-directory)
	(default-directory 
	  (if (string= jde-make-working-directory "")
	      default-directory
	    (jde-normalize-path 'jde-make-working-directory))))


    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jde-make from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (and (eq system-type 'windows-nt)
	     (not jde-xemacsp))	
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jde-make
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (setq compilation-finish-function 
      (lambda (buf msg) 
	(run-hook-with-args 'jde-make-finish-hook buf msg)
	(setq compilation-finish-function nil)))

    (cd default-directory)
    (compile-internal make-command "No more errors")
    (cd save-default-directory)))

;;;###autoload
(defun jde-make-show-options ()
  "Show the JDE Make Options panel."
  (interactive)
  (customize-apropos "jde-make" 'groups))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-make)

;; $Log: jde-make.el,v $
;; Revision 1.17  2004/08/21 04:30:43  paulk
;; Update the JDEE wizard class list after making a project.
;;
;; Revision 1.16  2003/09/16 05:13:39  paulk
;; Update jde-make command to issue a cd to the default directory before invoking
;; make and to restore the original cwd afterwards. This fixes a bug in the
;; implementation of the jde-make-working-directory variable.
;;
;; Revision 1.15  2002/11/21 04:18:41  paulk
;; These packages, when autoloaded, now register and initialize the customization variables
;; that they define to the values specified in the current project file.
;;
;; Revision 1.14  2002/10/22 05:02:09  paulk
;; Put make customization variables in their own group and provide an autoloaded command for displaying them as a group.
;;
;; Revision 1.13  2002/09/16 05:14:18  paulk
;; Adds a jde-make-finish-hook variable that allows you to specify functions
;; to run when make finishes. By default the variable is set to functions
;; that update the speedbar and the completion cache. Thanks to Sandip Chitale.
;;
;; Revision 1.12  2002/03/22 05:24:32  paulk
;; Expanded documentation for the jde-make command.
;;
;; Revision 1.11  2001/07/16 13:39:33  paulk
;; Added note to the doc for jde-make-working-directory that the path must end in a path separator.
;;
;; Revision 1.10  2001/05/31 04:00:50  paulk
;; Backed out the previous change which was NOT a bug.
;;
;; Revision 1.9  2001/05/31 01:09:13  paulk
;; Small bug fix. Thanks to Luis Miguel Hernanz Iglesias <luish@germinus.com>.
;;
;; Revision 1.8  2001/04/16 05:59:17  paulk
;; Normalize paths. Thanks to Nick Sieger.
;;
;; Revision 1.7  2000/08/09 03:29:26  paulk
;; Added jde-make-working-directory variable. Thanks to Laurent Latil <Laurent.Latil@france.sun.com>
;;
;; Revision 1.6  1999/04/27 16:44:49  paulk
;; Updated to allow interactive entry of make arguments. Thanks to Yarek J. Kowalik <jgk@klg.com> for providing this enhancement.
;;
;; Revision 1.5  1999/01/17 00:43:57  paulk
;; Removed two line feeds at the end of make command as they appeared to
;; confuse GNU make for NT.
;;
;; Revision 1.4  1998/11/27 09:38:23  paulk
;; Changed to use compile mode as suggested by Robert Grace <rmg2768@draper.com>.
;;
;; Revision 1.3  1998/05/29 01:46:39  paulk
;; Added dummy function for jde-make-mode to facilitate autoloading.
;;
;; Revision 1.2  1998/05/27 06:04:52  paulk
;; Added autoload comments.
;;
;; Revision 1.1  1998/03/27 04:44:36  kinnucan
;; Initial revision
;;

;; End of jde-make.el