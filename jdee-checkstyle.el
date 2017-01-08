;;; jdee-checkstyle.el --- Checkstyle interface for JDEE

;; Copyright (C) 2001, 2002, 2003, 2004 Markus Mohnen and Paul Kinnucan
;; Copyright (C) 2009 by Paul Landes

;; Authors: Markus Mohnen and Paul Kinnucan
;; Maintainers: Markus Mohnen and Paul Landes
;; Created: 06 Jun 2001
;;
;;
;; Keywords: Java coding standard checker Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; jdee-checkstyle|Markus Mohnen|
;; |Checkstyle interface for JDE
;; |$Date$|$Revision$|~/packages/jdee-checkstyle.el

;;; Commentary:

;;; This package provides an interface from JDEE to Oliver Burn's
;;; CheckStyle (see http://checkstyle.sourceforge.net/) a development
;;; tool to help programmers write Java code that adheres to a coding
;;; standard.

;;; Usage:
;;
;;  M-x `jdee-checkstyle' to check the java file in the current buffer.
;;

;;; Customization:
;;
;;  M-x `jdee-checkstyle-customize' to customize all the jdee-checkstyle options.

;;; Code:

(require 'jdee-compile)
(require 'jdee-classpath)
(require 'jdee-files)
(require 'jdee-project-file)
(require 'jdee-run)
(require 'jdee-util)

(defconst jdee-checkstyle-version "3.1")

(defgroup jdee-checkstyle nil
  "This group specifies options for the JDEE's interface to the CheckStyle
package (http://checkstyle.sourceforge.net). The CheckStyle package
checks Java source files for conformity to a specified coding
style."
  :group 'jdee)

(defcustom jdee-checkstyle-class "com.puppycrawl.tools.checkstyle.Main"
  "*Java checker class.
Specifies the class of the the program to be used to check the source
in the current buffer. The default is the checkstyle program."
  :group 'jdee-checkstyle
  :type 'string)

(defcustom jdee-checkstyle-classpath nil
  "*Specify paths of classes required to run the jdee-checkstyle application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jdee-global-classpath' option."
  :group 'jdee-checkstyle
  :type '(repeat (file :tag "Path")))

(defcustom jdee-checkstyle-read-args nil
  "*Specify whether to prompt for additional checker arguments.
If this variable is non-nil, the jdee-checkstyle command prompts
you to enter additional checker arguments in the minibuffer.
These arguments are appended to those specified by customization
variables. The JDE maintains a history list of arguments
entered in the minibuffer."
  :group 'jdee-checkstyle
  :type 'boolean)

(defvar jdee-checkstyle-interactive-args ""
  "String of checker arguments entered in the minibuffer.")

(defvar jdee-checkstyle-interactive-arg-history nil
  "History of checker arguments entered in the minibuffer.")

;; (makunbound 'jdee-checkstyle-style)
(defcustom jdee-checkstyle-style nil
  "*Style used to check this project's Java code. \"Sun\"
checks for conformity to the Java code style standard established by
Sun Microsystems. \"Custom\" specifies a a user-defined
style. Selecting this option causes Emacs to display an edit
field. Enter the path of a CheckStyle configuration file that defines
the custom coding style in this field (see the CheckStyle
documentation for information on configuration files). Use
`jdee-checkstyle-properties' to specify the values of properties that
the configuration file reads from the CheckStyle command line."
   :group 'jdee-checkstyle
   :type '(choice (const :tag "Sun" :value nil)
		 (file :menu-tag "Custom" :tag "Config. File")))


(defcustom jdee-checkstyle-expanded-properties nil
  "*Specify the values of the expanded properties specified by the
`jdee-checkstyle-style' configuration file. (See the CheckStyle
documentation for information about expanded properties.) To enter a
property, select the INS button. Emacs displays a Property Name field
and a Property Value field for the property. Enter the name of the
property, for example, checkstyle.header.file, in the Property Name
field; enter its value, for example, docs/java.header, in the Property
Value field.  Repeat this process to display additional
properties. You can specify as many properties as you like in
this way. To delete a property, select the DEL button next
to the property."
  :group 'jdee-checkstyle
  :type '(repeat (cons
		  (string :tag "Property Name")
		  (string :tag "Property Value"))))

;; (makunbound 'jdee-checkstyle-expanded-properties-file)
(defcustom jdee-checkstyle-expanded-properties-file nil
  "*Path of a file that specifies the values of a configuration
file's expanded properties. If this option is set, the JDEE ignores
the settings of the `jdee-checkstyle-expanded-properties' variable."
   :group 'jdee-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Properties File" :tag "Path")))

;; (makunbound 'jdee-checkstyle-module-package-names-file)
(defcustom jdee-checkstyle-module-package-names-file nil
  "*Path of a file that specifies the package names of
custom style checking modules used by this project."
   :group 'jdee-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Package Names File" :tag "Path")))

;; (makunbound 'jdee-checkstyle-output-file)
(defcustom jdee-checkstyle-output-file nil
  "*Path of a file to store CheckStyle's output."
   :group 'jdee-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Output File" :tag "Path")))


;; (makunbound 'jdee-checkstyle-output-format)
(defcustom jdee-checkstyle-output-format nil
  "*Format of CheckStyle's output. Options are plain or XML."
  :group 'jdee-checkstyle
  :type '(choice (const :tag "Plain" :value nil)
		 (const :tag "XML" :value "xml")))


;; (makunbound 'jdee-checkstyle-source-dir)
(defcustom jdee-checkstyle-source-dir nil
  "*Path of a directory to check. If you specify a
path, CheckStyle checks all the files in the specified
directory. Otherwise, it checks the file in the current
buffer."
   :group 'jdee-checkstyle
   :type '(choice (const :tag "None" :value nil)
		  (file :menu-tag "Source Directory" :tag "Path")))


;; (makunbound 'jdee-checkstyle-finish-hook)
(defcustom jdee-checkstyle-finish-hook
  '(jdee-compile-finish-kill-buffer)
  "List of functions to be invoked when CheckStyle terminates.  Each
function should accept two arguments: the compilation buffer and a
string describing how the compilation finished."
  :group 'jdee-checkstyle
  :type 'hook)


;; (makunbound 'jdee-checkstyle-source-file-extension)
(defcustom jdee-checkstyle-source-file-extension nil
  "*Extension of Java source files (if not java)."
  :group 'jdee-checkstyle
  :type '(choice (const :tag "java" :value nil)
		 (string :menu-tag "other" :tag "Extension")))


(defmethod jdee-checkstyle-get-property-args ((this jdee-run-vm))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jdee-run-option-properties))


;;;###autoload
(defun jdee-checkstyle-customize ()
  "Set Java style checking options."
  (interactive)
  (customize-group "jdee-checkstyle"))


(defclass jdee-checkstyle-checker ()
  ((buffer           :initarg :buffer
		     :type buffer
		     :documentation
		     "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (interactive-args :initarg :interactive-args
                     :initform: nil
		     :type list
		     :documentation
		     "Arguments entered in the minibuffer."))
  "Class of Java style checkers.")

(defmethod jdee-checkstyle-create-checker-buffer ((this jdee-checkstyle-checker))
  (save-excursion
    (let ((buf (get-buffer-create "*check style*"))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (boundp 'compilation-enter-directory-regexp-alist)
				  compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (boundp 'compilation-leave-directory-regexp-alist)
				  compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (boundp 'compilation-file-regexp-alist)
				 compilation-file-regexp-alist))
	  (nomessage-regexp-alist (if (boundp 'compilation-nomessage-regexp-alist)
				      compilation-nomessage-regexp-alist))
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)

      ;; Make sure a style checker process is not
      ;; already running.
      (let ((check-proc (get-buffer-process (current-buffer))))
	(if check-proc
	    (if (or (not (eq (process-status check-proc) 'run))
		    (yes-or-no-p
                     "A check style process is running; kill it?"))
		(condition-case ()
		    (progn
		      (interrupt-process check-proc)
		      (sit-for 1)
		      (delete-process check-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))

      ;; In case the checker buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode)
      (setq buffer-read-only nil)

      (set (make-local-variable 'compilation-finish-functions)
	   (lambda (buf msg)
	     (run-hook-with-args 'jdee-checkstyle-finish-hook buf msg)
	     (setq compilation-finish-functions nil)))
      (if (boundp 'compilation-error-message)
	  (set (make-local-variable 'compilation-error-message) error-message))
      (set (make-local-variable 'compilation-error-regexp-alist)
           error-regexp-alist)
      (dolist (elt `((compilation-enter-directory-regexp-alist
                      ,enter-regexp-alist)
                     (compilation-leave-directory-regexp-alist
                      ,leave-regexp-alist)
                     (compilation-file-regexp-alist
                      ,file-regexp-alist)
                     (compilation-nomessage-regexp-alist
                      ,nomessage-regexp-alist)))
        (if (boundp (car elt))
            (set (make-local-variable (car elt)) (second elt))))

      (if (boundp 'compilation-directory-stack)
	  (setq default-directory thisdir
		compilation-directory-stack (list default-directory))))))

(defmethod jdee-checkstyle-get-property-args ((this jdee-checkstyle-checker))
    "Get property arguments."
    (mapcar
     (lambda (prop)
       (format "-D%s=%s" (car prop) (cdr prop)))
     jdee-checkstyle-expanded-properties))

(defmethod jdee-checkstyle-exec ((this jdee-checkstyle-checker))

  (jdee-checkstyle-create-checker-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if compilation-process-setup-function
      (funcall compilation-process-setup-function))

  (let* ((outbuf (oref this :buffer))
	 (vm-path (oref (jdee-run-get-vm) :path))
	 (source-file
	  (concat (file-name-nondirectory buffer-file-name)))
	 (jdee-java-directory
	  (concat
	   (jdee-find-jdee-data-directory)
	   "java/"))
	 (args (append
		(unless jdee-checkstyle-expanded-properties-file
		  (jdee-checkstyle-get-property-args this))
		(oref this :interactive-args)
		(list "-classpath"
		      (if jdee-checkstyle-classpath
			  (jdee-build-classpath jdee-checkstyle-classpath)
			(jdee-normalize-path
			 (expand-file-name "lib/checkstyle-all.jar" jdee-java-directory))))
		(list jdee-checkstyle-class)
		(list "-c"
		      (if jdee-checkstyle-style
			  (jdee-normalize-path jdee-checkstyle-style)
			(concat (jdee-find-jdee-data-directory) "java/lib/sun_checks.xml")))
		(if jdee-checkstyle-expanded-properties-file
		    (list "-p" (jdee-normalize-path jdee-checkstyle-expanded-properties-file)))
		(if jdee-checkstyle-module-package-names-file
		    (list "-n" (jdee-normalize-path jdee-checkstyle-module-package-names-file)))
		(if jdee-checkstyle-output-format
		    (list "-f" jdee-checkstyle-output-format))
		(if jdee-checkstyle-output-file
		    (list "-o" (jdee-normalize-path jdee-checkstyle-output-file)))
		(if jdee-checkstyle-source-file-extension
		    (list "-e" jdee-checkstyle-source-file-extension))
		(if jdee-checkstyle-source-dir
		    (list "-r" (jdee-normalize-path jdee-checkstyle-source-dir))
		  (list source-file)))))

    (with-current-buffer outbuf

      (insert (format "cd %s\n" default-directory))

      (insert (concat
	       vm-path
	       " "
	       (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process
			  (downcase mode-name)
			  outbuf
			  vm-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))



;;;###autoload
(defun jdee-checkstyle ()
  "Checks the Java program in the current buffer.
This command invokes the style checker specified by `jdee-checkstyle-class'
with the options specif2ied by the JDEE customization variables
that begin with `jdee-checkstyle'. If the variable
`jdee-checkstyle-read-args' is non-nil, this command reads
additional compilation options from the minibuffer, with
history enabled."
  (interactive)

  (if jdee-checkstyle-read-args
      (setq jdee-checkstyle-interactive-args
	    (read-from-minibuffer
	     "Check args: "
	     jdee-checkstyle-interactive-args
	     nil nil
	     '(jdee-checkstyle-interactive-arg-history . 1))))

  (let ((checker (jdee-checkstyle-checker
		  "checker"
		  :interactive-args (if jdee-checkstyle-read-args
					jdee-checkstyle-interactive-args))))

    ;; Force save-some-buffers to use the minibuffer
    ;; to query user about whether to save modified buffers.
    ;; Otherwise, when user invokes jdee-checkstyle from
    ;; menu, save-some-buffers tries to popup a menu
    ;; which seems not to be supported--at least on
    ;; the PC.
    (if (eq system-type 'windows-nt)
	(let ((temp last-nonmenu-event))
	  ;; The next line makes emacs think that jdee-checkstyle
	  ;; was invoked from the minibuffer, even when it
	  ;; is actually invoked from the menu-bar.
	  (setq last-nonmenu-event t)
	  (save-some-buffers (not compilation-ask-about-save) nil)
	  (setq last-nonmenu-event temp))
      (save-some-buffers (not compilation-ask-about-save) nil))

    (jdee-checkstyle-exec checker)))

;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-checkstyle)

;;; jdee-checkstyle.el ends here
