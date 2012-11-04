;;; jde-db.el -- Debugger mode for jdb.
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 2000, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
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
;; Boston, MA 02111-1307, US

;; Commentary:

;; This package interfaces emacs to jdb, the debugger
;; distributed as part of JavaSoft's Java
;; Development Kit (JDK).

;;; Code:

(require 'jde-parse)
(require 'eieio)
(require 'jde-util)
(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(unless (fboundp 'make-overlay)
  (require 'overlay))


;; quiet "reference to free variable" build-time warnings
(defvar jde-global-classpath)
(defvar jde-sourcepath)
(defvar jde-debugger)
(declare-function jde-expand-wildcards-and-normalize "jde" (path &optional symbol))

;; ======================================================================
;; jde-db variables


(defcustom jde-db-query-missing-source-files t
  "If nonnil, this variable causes the debugger to query you
for the path of a class source file that it cannot find in
`jde-sourcepath'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-db-mode-hook nil
  "*Customization hook for jde-db inferior mode."
  :group 'jde-project
  :type 'hook
)

(defcustom jde-db-initial-step-p t
  "*If non-nil, this option causes the debugger
to issue a step-into command after launching
a program. This causes the vm to step to the
first line of the debuggee program."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-db-read-vm-args nil
"*Read vm arguments from the minibuffer.
If this variable is non-nil, the jde-db command reads vm arguments
from the minibuffer and appends them to those specified by
the `jde-db-option' variable group."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-vm-arg-history nil
"History of vm arguments read from the minibuffer")

(defcustom jde-db-read-app-args nil
"*Read arguments to be passed to application from the minibuffer."
  :group 'jde-project
  :type 'boolean)

(defvar jde-db-interactive-app-arg-history nil
"History of application arguments read from the minibuffer")

(defcustom jde-db-classic-mode-vm nil
"Runs applications in the classic (i.e., not HotSpot) mode when
debugging."
  :group 'jde-project
  :type 'boolean)

(defgroup jde-db-options nil
  "JDE Debugger Options"
  :group 'jde
  :prefix "jde-db-option-")

(defcustom jde-db-option-classpath nil
"*Specify paths of classes required to run this application.
The JDE uses the specified paths to construct a -classpath
argument to pass to the Java interpreter. This option overrides the
`jde-global-classpath' option."
  :group 'jde-db-options
  :type '(repeat (file :tag "Path")))

(defcustom jde-db-option-verbose (list nil nil nil)
  "*Print messages about the running process.
The messages are printed in the run buffer."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "\n  %[%v%] %h \n"
			 :doc "Print classes loaded.
Prints a message in the run buffer each time a class is loaded.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print memory freed.
Prints a message in the run buffer each time the garbage collector
frees memory.")
	       (checkbox :format "%[%v%] %h \n"
			 :doc "Print JNI info.
Prints JNI-related messages including information about which native
methods have been linked and warnings about excessive creation of
local references.")))

(defcustom jde-db-option-properties nil
  "*Specify property values.
Enter the name of the property, for example, awt.button.color, in the
Property Name field; enter its value, for example, green, in the
Property Value field. You can specify as many properties as you like."
  :group 'jde-db-options
  :type '(repeat (cons
		  (string :tag "Property Name")
		  (string :tag "Property Value"))))

(defcustom jde-db-option-heap-size (list
				    (cons 1 "megabytes")
				    (cons 16 "megabytes"))
"*Specify the initial and maximum size of the interpreter heap."
:group 'jde-db-options
:type '(list
	(cons (integer :tag "Start")
	     (radio-button-choice (const "bytes")
				  (const "kilobytes")
				  (const "megabytes")
				  (const "gigabytes")))
	(cons (integer :tag "Max")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))))


(defcustom jde-db-option-stack-size (list
				     (cons 128 "kilobytes")
				     (cons 400 "kilobytes"))
  "*Specify size of the C and Java stacks."
  :group 'jde-db-options
  :type '(list
	  (cons (integer :tag "C Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))
	  (cons (integer :tag "Java Stack")
	       (radio-button-choice (const "bytes")
				    (const "kilobytes")
				    (const "megabytes")
				    (const "gigabytes")))))

(defcustom jde-db-option-garbage-collection (list t t)
  "*Specify garbage collection options."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect garbage asynchronously.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Collect unused classes.")))

(defcustom jde-db-option-java-profile (cons nil "./java.prof")
  "*Enable Java profiling."
  :group 'jde-db-options
  :type '(cons boolean
	       (file :tag "File"
		     :help-echo
"Specify where to put profile results here.")))

(defcustom jde-db-option-heap-profile (cons nil
					    (list "./java.hprof"
						  5
						  20
						  "Allocation objects"))
"*Output heap profiling data."
  :group 'jde-db-options
  :type '(cons boolean
	       (list
		(string :tag "Output File Path")
		(integer :tag "Stack Trace Depth")
		(integer :tag "Allocation Sites")
		(radio-button-choice :format "%t \n%v"
				     :tag "Sort output based on:"
		 (const "Allocation objects")
		 (const "Live objects")))))

(defcustom jde-db-option-verify (list nil t)
  "*Verify classes."
  :group 'jde-db-options
  :type '(list :indent 2
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Executed code in all classes.")
	       (checkbox :format "%[%v%] %t \n"
			 :tag "Classes loaded by a classloader.")))

(defcustom jde-db-option-host ""
  "Host of a remote process to which you wish to attach. This
option is invalid for JDK verions greater than JDK 1.1.x."
  :group 'jde-db-options
  :type 'string)


;; (makunbound 'jde-db-option-connect-socket)
(defcustom jde-db-option-connect-socket (list nil "4444")
  "Specify address of socket to be used to connect the debugger and a
debuggee process. Selecting \"Prompt\" from the customization value
menu causes the debugger to prompt you to enter the shared memory name
when you command it to attach to or listen for an existing
process. Selecting \"Specify\" allows you to specify a default socket
host and port to be used by the debugger. "
  :group 'jde-db-options
  :type '(choice
	  (const :menu-tag "Prompt" nil)
	  (list
	   :menu-tag "Specify" :tag "Socket Address" :inline nil
	   (choice
	    :tag "Host"
	    (const :menu-tag "Local" nil)
	    (string :menu-tag "Remote" :tag "Name"))
	   (choice
	    :tag "Port"
	    (const :menu-tag "Default" "4444")
	    (string :menu-tag "Custom")))))




;; (makunbound 'jde-db-option-connect-shared-memory-name)
(defcustom jde-db-option-connect-shared-memory-name "javadebug"
  "Specify name to use to establish a shared memory connection
between the debugger and a debuggee process. Selecting \"Prompt\" from
the customization value menu causes a debugger attach or listen
command, e.g., `jde-jdb-attach-via-shared-memory', to prompt you to
enter the shared memory name. Selecting \"Specify\" allows you to
specify a name of your choosing."
  :group 'jde-db-options
  :type '(choice
	  (const :menu-tag "Prompt" nil)
	  (string :menu-tag "Specify" :tag "Name")))


(defcustom jde-db-option-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java interpreter. It is an alternative to using JDE Run Option
variables, such as `jde-run-option-stack-size', to specify Java
interpreter options. Also, it makes it possible to use the JDE with
interpreters that accept command line arguments not supported by
the JDE Run Option variable set."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))


(defcustom jde-db-option-application-args nil
  "*Specify command-line arguments to pass to the application.
The JDE passes the specified arguments to the application on
the command line."
  :group 'jde-db-options
  :type '(repeat (string :tag "Argument")))

(defmacro jde-assert-source-or-debug-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert
    (or
     (eq major-mode 'jde-mode)
     (and (slot-boundp 'jde-db-debugger 'the-debugger)
	  (eq (current-buffer)
	      (oref (oref 'jde-db-debugger the-debugger) buffer))))
    nil
    "This command works only in a Java source or debug buffer."))

(defcustom jde-db-log-debugger-output-flag nil
  "Log raw debugger output to a buffer. This variable is intended
to be used for debugging the JDEE's debuggers."
  :group 'jde-db-options
  :type 'boolean)

(defun jde-db-log-debugger-output (output)
  (if jde-db-log-debugger-output-flag
      (let ((buf (get-buffer "debugger output")))
	(when (not buf)
	  (setq buf (get-buffer-create  "debugger output"))
	  (pop-to-buffer buf))
	(with-current-buffer buf
	  (goto-char (point-max))
	  (insert-string output)))))

(defun jde-db-get-debuggee-status ()
  "Get the`jde-db-debuggee-status' of the
current debuggee process."
  (if (slot-boundp 'jde-db-debugger 'the-debugger)
      (let* ((debugger (oref 'jde-db-debugger the-debugger))
	     (debuggee (oref debugger debuggee)))
	(oref debuggee status))))


(defun jde-db-debuggee-stopped-p ()
  "Return t if current debuggee process is stopped."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status stopped-p))))

(defun jde-db-debuggee-suspended-p ()
  "Return t if current debuggee process is suspended."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status suspended-p))))

(defun jde-db-debuggee-running-p ()
  "Return t if current debuggee process is running."
  (let ((status (jde-db-get-debuggee-status)))
     (if status
	 (oref status running-p))))


;;;###autoload
(defun jde-db-set-debugger (name is-executable)
  "Specify the pathname of the debugger, if an executable, or the
debugger's fully qualified class name, if a class."
  (interactive
   "sEnter name of Java interpreter: \nsIs %s executable? (yes): ")
  (let ((db name)
	(type
	 (if (stringp is-executable)
	     (if (or
		  (string= is-executable "")
		  (eq (aref is-executable 0) ?y))
		 "Executable"
	       "Class")
	   "Executable")))
    (setq jde-db-debugger (cons "Other" (cons db type)))))

;;;###autoload
(defun jde-db-set-args (args)
  "Specify the arguments (except -classpath) to be passed to the debugger."
  (interactive
   "sEnter arguments: ")
  (setq jde-db-option-vm-args (jde-run-parse-args args)))

;;;###autoload
(defun jde-db-set-app-args (args)
  "Specify the arguments to be passed to the Java application class."
  (interactive
   "sEnter arguments: ")
  (setq jde-db-option-application-args (jde-run-parse-args args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoints                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jde-db-spec-breakpoint-face-colors (cons "black" "green")
"*Specifies the foreground and background colors used to highlight
the line at which you have specified that a breakpoint to be set."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground")
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-spec-breakpoint-face)
	  (set-face-foreground 'jde-db-spec-breakpoint-face (car val))
	  (set-face-background 'jde-db-spec-breakpoint-face (cdr val))
	  (set-default sym val)))

(defcustom jde-db-requested-breakpoint-face-colors (cons "black" "yellow")
"*Specifies the foreground and background colors used to highlight
the line at which you have requested a breakpoint to be set."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground")
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-requested-breakpoint-face)
	  (set-face-foreground 'jde-db-requested-breakpoint-face (car val))
	  (set-face-background 'jde-db-requested-breakpoint-face (cdr val))
	  (set-default sym val)))

(defcustom jde-db-active-breakpoint-face-colors (cons "black" "red")
"*Specifies the foreground and background colors used to highlight
a line where an active breakpoint exists."
  :group 'jde-project
  :type '(cons :tag "Colors"
	  (string :tag "Foreground")
	  (string :tag "Background"))
  :set '(lambda (sym val)
	  (make-face 'jde-db-active-breakpoint-face)
	  (set-face-foreground 'jde-db-active-breakpoint-face (car val))
	  (set-face-background 'jde-db-active-breakpoint-face (cdr val))
	  (set-default sym val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Marker Class                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-db-breakpoint-marker ()
  ((marker :initarg :marker
	   :documentation
	   "Overlay in Emacs, extent in XEmacs"))
  "Indicates the location of breakpoints in a source buffer. This class
uses overlays as markers in Emacs and extents in XEmacs.")

(defmethod initialize-instance ((this jde-db-breakpoint-marker) &rest fields)
  "Create a breakpoint overlay at LINE in FILE."

  ;; Call parent initializer.
  (call-next-method)

  (oset this marker
	(if (featurep 'xemacs)
	    (if (or (not (extent-at (jde-line-beginning-position)))
		    (not (jde-db-breakpoint-marker-p
			   (extent-at (jde-line-beginning-position)))))
		(make-extent
		 (jde-line-beginning-position)
		 (1+ (jde-line-end-position))))
	  (make-overlay
	   (jde-line-beginning-position)
	   (1+ (jde-line-end-position))
	   (current-buffer) nil t))))


(defmethod jde-db-breakpoint-marker-set-face ((this jde-db-breakpoint-marker) face)
  "Apply FACE to OVERLAY."
  (let ((marker (oref this marker)))
    (if (featurep 'xemacs)
	(progn
	  (set-extent-face marker face)
	  (set-extent-priority marker 98))
      (progn
	(overlay-put marker  'face face)
	(overlay-put marker 'priority 98)))))

(defun jde-db-breakpoint-marker-p (marker)
  "Return t if overlay is a breakpoint marker overlay."
    (let ((marker-face
	   (if (featurep 'xemacs)
	       (extent-property marker 'face nil)
	     (overlay-get marker 'face))))
      (or
       (eq marker-face 'jde-db-spec-breakpoint-face)
       (eq marker-face 'jde-db-requested-breakpoint-face)
       (eq marker-face 'jde-db-active-breakpoint-face))))

(defmethod jde-db-breakpoint-marker-delete ((this jde-db-breakpoint-marker))
  "Remove breakpoint overlay at LINE in FILE."
  (if (featurep 'xemacs)
      (delete-extent (oref this marker))
    (delete-overlay (oref this marker))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-breakpoint ()
  ((id       :initarg :id
	     :type integer
	     :documentation
	     "Identifies this breakpoint.")
   (file     :initarg :file
	     :initform ""
	     :type string
	     :documentation
	     "Pathname of file containing this breakpoint.")
   (line     :initarg :line
	     :type integer
	     :documentation
	     "Number of line at which breakpoint is set.")
   (marker   :initarg :marker
	     :type (or null jde-db-breakpoint-marker)
	     :initform nil
	     :documentation
	     "Marker used to highlight breakpoint line.")
   (class    :initarg :class
	     :type string
	     :documentation
	     "Qualified name of class containing breakpoint.")
   (status   :initarg status
	     :type symbol
	     :initform specified
	     :documentation
	     "Status of this breakpoint. Legal values are `specified', `requested', `active'."))
  (:allow-nil-initform t)
  "Class of breakpoints.")


(defmethod initialize-instance ((this jde-db-breakpoint) &rest fields)
  "Constructor for a breakpoint specification."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this file))

  (oset this
	marker
	(jde-db-breakpoint-marker "breakpoint marker"))

  (jde-db-breakpoint-marker-set-face
   (oref this marker) 'jde-db-spec-breakpoint-face))

(defmethod jde-db-breakpoint-get-line ((this jde-db-breakpoint))
  "Get the number of the line at which this breakpoint is set."
  (with-current-buffer (find-file-noselect (oref this file))
    (if (oref this marker)
	(let ((marker-start
	       (if (featurep 'xemacs)
		   (extent-start-position (oref (oref this marker) marker))
		 (overlay-start (oref (oref this marker) marker)))))
	  (jde-get-line-at-point marker-start))
      (oref this line))))

(defvar jde-db-breakpoints nil
"Current breakpoints.")


(defun jde-db-get-breakpoint-marker (file line)
  "Get breakpoint marker at FILE and LINE."
  (let ((bp (jde-db-find-breakpoint file line)))
    (if bp
	(oref bp marker))))

(defun jde-db-mark-breakpoint-specified (file line)
  "Changes the face of the breakpoint marker at LINE in FILE
to the specified face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-spec-breakpoint-face))))

(defun jde-db-mark-breakpoint-active (file line)
  "Changes the face of the breakpoint marker at LINE in FILE
to the active face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-active-breakpoint-face))))

(defun jde-db-mark-breakpoint-requested (file line)
  "Changes the face of the breakpoint marker at LINE in FILE
to the active face."
  (let ((marker (jde-db-get-breakpoint-marker file line)))
    (if marker
	(jde-db-breakpoint-marker-set-face marker 'jde-db-requested-breakpoint-face))))

(defun jde-db-set-all-breakpoints-specified ()
  "Changes the face of all breakpoints to `jde-db-spec-breakpoint-face'
and sets the status of all breakpoints to `specified'."
  (loop for bp-assoc in jde-db-breakpoints do
	(let* ((bp (cdr bp-assoc))
	       (marker (oref bp marker)))
	  (oset bp status 'specified)
	  (if marker
	      (jde-db-breakpoint-marker-set-face marker 'jde-db-spec-breakpoint-face)))))

(defun jde-db-delete-breakpoint (bp)
  "Delete the breakpoint at LINE in FILE."
  (setq jde-db-breakpoints
	;; bp will be in the list so don't run the risk of using a
	;; deleted extent.
	(let ((bpline (jde-db-breakpoint-get-line bp)))
	  (remove-if
	   (lambda (assoc-x)
	     (let* ((xbp (cdr assoc-x))
		    (xfile (oref xbp file))
		    (deletep
		     (and
		      (string= (oref bp file) xfile)
		      (equal bpline (jde-db-breakpoint-get-line  xbp)))))
	       (if deletep
		   (jde-db-breakpoint-marker-delete
		    (oref bp marker)))
	       deletep))
	   jde-db-breakpoints))))

(defun jde-db-clear-breakpoints ()
  "Clear all breakpoints from all buffers."
  (mapc
   (lambda (assoc-x)
     (let* ((xbp (cdr assoc-x))
	    (file (oref xbp file))
	    (buf (find-buffer-visiting file)))
       (if buf
	   (with-current-buffer buf
	     (let ((xmarker (oref xbp marker)))
	       (jde-db-breakpoint-marker-delete xmarker))))))
      jde-db-breakpoints)
  (setq jde-db-breakpoints nil))

(defvar jde-db-bp-list nil)
(defun jde-debug-list-breakpoints (&optional active)
  "Brings a list of all set breakpoints. It allows the user to jump to a
particular breakpoint and to select breakpoints to be clear."
  (interactive "i")
  (if jde-db-breakpoints
      (progn
	(switch-to-buffer "*Breakpoints List*")
	(kill-all-local-variables)
	(make-local-variable 'jde-db-bp-list)
	(setq jde-db-bp-list nil)
	(let ((inhibit-read-only t))
	  (erase-buffer))
	(setq active (not active))
	(widget-insert "Breakpoints:\n\n")
	(mapc
	 (lambda (assoc-x)
	   (let* ((xbp (cdr assoc-x))
		  (id (oref xbp id))
		  (class (oref xbp class))
		  (file (oref xbp file))
		  (line (oref xbp line))
		  (status (oref xbp status)))
	     (widget-create
	      'checkbox
	      :notify (lambda (widget &rest ignore)
			(if (widget-value widget)
			    (setq jde-db-bp-list
				  (delete (widget-get widget :id)
					  jde-db-bp-list))
			  (setq jde-db-bp-list
				(append jde-db-bp-list
					(list (widget-get widget :id))))))
	      :id id
	      active)
	   (if (not active)
		(setq jde-db-bp-list (append jde-db-bp-list (list id))))
	    (widget-insert " ")
	    (widget-create 'push-button
			   :notify (lambda (widget &rest ignore)
				     (progn
				       (find-file-other-window
					(widget-get widget :file))
				       (goto-char (point-min))
				       (forward-line 
					(1- (widget-get widget :line)))))

			   :button-face
			    (cond
			     ((eq status 'specified)
			      'jde-db-spec-breakpoint-face)
			     ((eq status 'active)
			      'jde-db-active-breakpoint-face)
			     (t 'jde-db-requested-breakpoint-face))
			   :file file
			   :line line
			   (format "%s:%d" class line))
	    (widget-insert "\n")))
	 jde-db-breakpoints)
	(widget-insert "\n")
	(widget-create 'push-button
		       :notify (lambda (&rest ignore)
				 (jde-debug-list-breakpoints t))
		       "Clear All")
	(widget-insert " ")
	(widget-create 'push-button
		       :notify (lambda (&rest ignore)
				 (progn
				   (jde-db-process-breakpoints)
				   (kill-buffer "*Breakpoints List*")))
		       "Apply Form")
	(use-local-map widget-keymap)
	(widget-insert "\n")
	(widget-setup))
    (message "No breakpoints")))

(defun jde-db-process-breakpoints ()
  "Deletes all the breakpoints found in `jde-db-bp-list'"
  (if jde-db-bp-list
      (if (jde-db-debuggee-running-p)
	  (let* ((debugger (oref 'jde-db-debugger the-debugger))
		 (bp-cmd (oref (oref debugger cmd-set) clear-bp)))
	    (oset
	     bp-cmd
	     breakpoints
	     (mapcar
	      (lambda (assoc-x)
		(jde-db-find-breakpoint-by-id assoc-x))
	      jde-db-bp-list))
	    (jde-db-exec-cmd debugger bp-cmd))
	(loop for bp-assoc in jde-db-bp-list do
	      (let ((bp (jde-db-find-breakpoint-by-id bp-assoc)))
		(jde-db-delete-breakpoint bp))))))

(defun jde-db-breakpoints-add (bp)
  "Adds this breakpoint to the list of breakpoints."
  (setq jde-db-breakpoints
	(cons (cons (oref bp id) bp)
	      jde-db-breakpoints)))


(defun jde-db-find-breakpoint-by-id (id)
  "Finds the breakpoint object with ID"
  (cdr (find-if
	(lambda (assoc-x)
	  (let ((bp (cdr assoc-x)))
	    (= (oref bp id) id)))
	jde-db-breakpoints)))

(defun jde-db-find-breakpoint (file line)
  "Finds the breakpoint object for the breakpoint at FILE and LINE."
  (cdr (find-if
	(lambda (assoc-x)
	  (let ((bp (cdr assoc-x)))
	       (and (string= (oref bp file) file)
		    (equal (jde-db-breakpoint-get-line bp) line))))
	jde-db-breakpoints)))


(defvar jde-db-breakpoint-id-counter 0
"Counter for generating breakpoint ids")

(defun jde-db-nullify-breakpoint-markers ()
  "Set the marker field for each breakpoint
in the current buffer to nil."
 (when (eq major-mode 'jde-mode)
   (let ((file (buffer-file-name)))
     (loop for bp-assoc in jde-db-breakpoints do
	   (let ((bp (cdr bp-assoc)))
	     (when (string= (oref bp file) file)
	       (oset bp line (jde-db-breakpoint-get-line bp))
	       (oset bp marker nil)))))))

(add-hook 'kill-buffer-hook 'jde-db-nullify-breakpoint-markers)

(defun jde-db-remark-breakpoints ()
  "Highlights all breakpoints in the current buffer if not
already highlighted."
  (save-excursion
    (loop for bp-assoc in jde-db-breakpoints do
	  (let* ((bp (cdr bp-assoc))
		 (file (buffer-file-name))
		 (line (oref bp line))
		 (status (oref bp status)))
	    (if (string-equal file (oref bp file))
		(progn
		  (goto-char (point-min))
		  (forward-line (1- line))
		  (oset bp
			marker
			(jde-db-breakpoint-marker "breakpoint marker"))
		  (cond
		   ((eq status 'specified)
		    (jde-db-mark-breakpoint-specified file line))
		   ((eq status 'requested)
		    (jde-db-mark-breakpoint-requested file line))
		   ((eq status 'active)
		    (jde-db-mark-breakpoint-active file line))
		   (t
		    (error "Unknown breakpoint status: %s"
			   (symbol-name status))))))))
    ))


(add-hook 'jde-mode-hook 'jde-db-remark-breakpoints)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Cursor Handling                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-db-query-source-file (class)
  (let ((source-file
	 (read-file-name
	  (format "Cannot find %s source. Enter path: " class))))
  (if (and
       source-file
       (file-exists-p source-file)
       (not (file-directory-p source-file)))
      (find-file-noselect source-file))))

(defun jde-db-find-class-source (class)
  "Find and open the source file for a class. CLASS is the fully
qualified name of the class. If this function is unable to find the
source for CLASS in `jde-sourcepath' and
`jde-db-query-missing-source-files' is nonnil, this function queries
the user for the path to the source file. If successful, this function
returns an unselected buffer containing the source file for the
class. Otherwise, it returns nil."
  (let* ((source-file (jde-find-class-source-file class))
	 (source-buffer
	  (if source-file
	      (find-file-noselect source-file)
	    (if jde-db-query-missing-source-files
		(jde-db-query-source-file class)))))
    source-buffer))

(defun jde-db-set-debug-cursor (class file line)
  "Shows the source at LINE in CLASS."
  (let* ((buffer (jde-db-find-class-source class))
	 (window
	  (and buffer
	       (or (get-buffer-window buffer)
		   (selected-window))))
	  pos)
    (if buffer
	(progn
	  (if (not (get-buffer-window buffer))
	      (set-window-buffer window buffer))
	  (with-current-buffer buffer
	    (save-restriction
	      (widen)
	      (goto-char (point-min))
	      (forward-line (1- line))
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Status Class                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-debuggee-status ()
  ((running-p   :initarg :running-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debuggee process is running.")
   (stopped-p   :initarg :stopped-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debuggee process is stopped.")
   (suspended-p :initarg :suspended-p
		:type boolean
		:initform nil
		:documentation
		"Non-nil if debuggee process is suspended."))
  "Status of debuggee process.")

(defmethod initialize-instance ((this jde-db-debuggee-status) &rest fields)
  "Status of debuggee process."
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-connector () ()
  "Proxy for debugger connectors.")

(defclass jde-db-launch-connector (jde-db-connector) ()
  "Launched by the debugger.")

(defclass jde-db-socket-connector (jde-db-connector)
  ((port :initarg :port
	 :type (or null string)
	 :initform nil
	 :documentation
	 "Port to the debuggee process."))
  "Connect via a socket.")

(defclass jde-db-shared-memory-connector (jde-db-connector)
  ((name  :initarg :name
	  :type (or null string)
	  :initform nil
	  :documentation
	  "Shared memory name of debuggee process."))
  "Connect via a shared-memory transport (Windows only).")


(defclass jde-db-attach-connector (jde-db-connector) ()
  "Attaches to debuggee.")

(defclass jde-db-listen-connector (jde-db-connector) ()
  "Listens for debuggee.")


(defclass jde-db-socket-attach-connector (jde-db-socket-connector
					  jde-db-attach-connector)
  ((host        :initarg :host
		:type (or null string)
		:initform nil
		:documentation
		"Host on which the debuggee process runs."))
  "Attach via a socket.")

(defclass jde-db-shared-memory-attach-connector (jde-db-shared-memory-connector
						 jde-db-attach-connector)
  ()
  "Attach via a shared memory connection.")

(defclass jde-db-socket-listen-connector (jde-db-socket-connector
					  jde-db-listen-connector)
  ()
  "Listen via a socket.")

(defclass jde-db-shared-memory-listen-connector (jde-db-shared-memory-connector
						 jde-db-listen-connector)
  ()
  "Listen via a shared memory connection.")


(defclass jde-db-debuggee ()
  ((status      :initarg :status
		:type jde-db-debuggee-status
		:documentation
		"Status of debuggee process.")

  (stack-depth  :initarg :stack-depth
		:type string
		:initform ""
		:documentation
		"Stack depth."))
  "Program being debugged.")

(defmethod initialize-instance ((this jde-db-debuggee) &rest fields)
  "Constructs an instance of a debuggee."
  (call-next-method))


(defclass jde-db-debuggee-app (jde-db-debuggee)
  ((main-class  :initarg :main-class
		:type string
		:documentation
		"Qualified name of debuggee main class.")

   (connector   :initarg :connector
		:type jde-db-connector
		:documentation
		"Type of connector between this debuggee and the debugger."))
  "Application being debugged.")

(defclass jde-db-debuggee-applet (jde-db-debuggee)
  ((doc :initarg :doc
	:type string
	:documentation
	"Path of applet HTML document."))
  "Applet being debugged.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debugger Command Line Commands                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-cmd ()
  ((name           :initarg :name
		   :type string
		   :documentation
		   "Name of command.")
   (debugger       :initarg :debugger
		   :type jde-db-debugger
		   :documentation
		   "Debugger."))
  "Super class of debugger commands.")


(defmethod initialize-instance ((this jde-db-cmd) &rest fields)
  "Constructor for debugger commands."
  (call-next-method))

(defmethod jde-db-cmd-init ((this jde-db-cmd))
  "The debugger invokes this method before executing the
command.")

(defmethod jde-db-cmd-make-command-line ((this jde-db-cmd))
  "Creates the command line for this command."
  (oref this name))

(defmethod jde-db-cmd-notify-response ((this jde-db-cmd) response)
  "Invoked when the debugger responds to the command. RESPONSE
is the response.")

(defmethod jde-db-cmd-response-p ((this jde-db-cmd) output)
  "Returns nonnil if external debugger's output is a
response to this command."
  t)


(defclass jde-db-cmd-breakpoint (jde-db-cmd)
  ((breakpoints :initarg :breakpoints
		:type list
		:documentation
		"List of breakpoint specification."))
  "Class of breakpoint commands.")

(defclass jde-db-cmd-launch (jde-db-cmd)
  ()
   "Launch a debuggee process.")

(defclass jde-db-cmd-launch-app (jde-db-cmd-launch)
  ((main-class :initarg :main-class
	       :type string
	       :documentation
	       "Main class of applications to be debugged."))
   "Launch an application in debug mode.")

(defmethod initialize-instance ((this jde-db-cmd-launch-app) &rest fields)
  (call-next-method)
  (oset this name "launch application"))

(defclass jde-db-cmd-launch-applet (jde-db-cmd-launch)
  ((doc  :initarg :doc
	 :type string
	 :documentation
	 "Path of applet document."))
   "Launch an applet in debug mode.")

(defmethod initialize-instance ((this jde-db-cmd-launch-applet) &rest fields)
  (call-next-method)
  (oset this name "launch applet"))

;; Generic Debugger Command Set.

(defclass jde-db-cmd-set ()
  ((debugger          :initarg :debugger
		      :type jde-db-debugger
		      :documentation
		      "Debugger that owns this command set.")
   (launch-app        :initarg :launch-app
		      :type jde-db-cmd-launch-app
		      :documentation
		      "Launch debuggee application")
   (launch-applet     :initarg :launch-applet
		      :type jde-db-cmd-launch-applet
		      :documentation
		      "Launch debuggee applet")
   (run               :initarg :run
		      :type jde-db-cmd
		      :documentation
		      "Starts the current debuggee application.")
   (cont              :initarg :cont
		      :type jde-db-cmd
		      :documentation
		      "Continues the current debuggee application.")
   (quit              :initarg :quit
		      :type jde-db-cmd
		      :documentation
		      "Quit debugging the current application.")
   (step-over         :initarg :step-over
		      :type jde-db-cmd
		      :documentation
		      "Step to the next line in the current frame.")
   (step-into         :initarg :step-into
		      :type jde-db-cmd
		      :documentation
		      "Step to the next line in the current program.")
   (step-out          :initarg :step-out
		      :type jde-db-cmd
		      :documentation
		      "Continue to the end of the current method.")
   (up                :initarg :up
		      :type jde-db-cmd
		      :documentation
		      "Move up the stack.")
   (down              :initarg :down
		      :type jde-db-cmd
		      :documentation
		      "Move down the stack.")
   (where             :initarg :where
		      :type jde-db-cmd
		      :documentation
		      "Point to the current stopping point.")
   (set-bp            :initarg :set-bp
		      :type jde-db-cmd
		      :documentation
		      "Cmd that asks debugger to set a breakpoint.")
   (clear-bp          :initarg :clear-bp
		      :type jde-db-cmd
		      :documentation
		      "Cmd that asks debugger to set a breakpoint."))
  "Set of debugger commands implemented by this debugger.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debug Process Listener                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-listener ()
  ((debugger   :initarg :debugger
	       :type jde-db-debugger
	       :documentation
	       "The debugger"))
  "Listens to the output from the debugger.")

(defmethod jde-db-listener-filter-output ((this jde-db-listener) output)
  "Filters the output of the debugger."
  output)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Class of JDE Debuggers                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jde-db-debugger ()
  ((name          :initarg :name
		  :type string
		  :initform "Java debugger"
		  :documentation
		  "Name of this Java debugger.")
   (buffer-name   :initarg :buffer-name
		  :initform "Java Debugger"
		  :type string
		  :documentation
		  "Name of buffer used to interact with debugger.")
   (buffer        :initarg :buffer
		  :type buffer
		  :documentation
		  "Buffer used to interact with debugger.")

   (process       :initarg :process
		  :documentation
		  "Debugger process.")

   (running-p     :initarg :process
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if debugger process is running.")

   (proc-filter   :initarg :proc-filter
		  :type function
		  :documentation
		  "Function used to parse debug output.")

   (listeners     :initarg :listeners
		  :type list
		  :initform nil
		  :documentation
		  "List of debugger output listeners.")

   (cmd-set       :initarg :cmd-set
		  :type jde-db-cmd-set
		  :documentation
		  "Commands implemented by this debugger.")

   (next-cmd      :initarg :next-cmd
		  :type list
		  :initform nil
		  :documentation
		  "Next command(s) to execute.")

   (last-cmd      :initarg :last-cmd
		  :type (or null jde-db-cmd)
		  :documentation
		  "Last command send to the debugger.")

   (debuggee      :initarg :debuggee
		  :type jde-db-debuggee
		  :documentation
		  "Application process being debugged.")

   (the-debugger  :type jde-db-debugger
		  :allocation :class
		  :documentation
		  "The currently active debugger."))
   "Class of Java debuggers.")

(defmethod initialize-instance ((this jde-db-debugger) &rest fields)
  "Constructor for generic debugger."
  (oset this cmd-set
	(jde-db-cmd-set "Generic commands" :debugger this))
  (oset this last-cmd nil))


(defmethod jde-db-create-debuggee-app ((this jde-db-debugger) main-class))

(defmethod jde-db-create-debuggee-applet ((this jde-db-debugger applet-doc)))

(defmethod jde-db-ready-p ((this jde-db-debugger) output)
  "Nonnil if OUTPUT indicates that the debugger is
ready to accept the next command."
  (and output
       (or
	(string-match ">[ ]*$" output)
	(string-match "[a-zA-Z0-9]+\[[0-9]+\][ ]*$" output)
	(string-match "VM Started:[ ]*$" output))))


(defmethod jde-db-process-debugger-output ((this jde-db-debugger) output)
  "Process debugger output."
  (jde-db-log-debugger-output (concat "<<" output ">>"))
  (let ((proc (oref this process))
	(listeners (oref this listeners))
	(response output)
	(last-cmd (oref this last-cmd)))

    (loop for listener in listeners do
	  (setq output
		(jde-db-listener-filter-output listener output)))

    (comint-output-filter proc output)

    (if last-cmd
	(jde-db-cmd-notify-response last-cmd response))

    (if (jde-db-ready-p this (car (last (split-string output "\n"))))
	(jde-db-exec-next-cmd this))))

(defmethod jde-db-add-listener ((this jde-db-debugger) listener)
  "Adds LISTENER to the list of listeners listening for response
from the debugger. LISTENER must be an object of type
`jde-db-listener'."
  (assert (typep listener jde-db-listener))
  (oset this listeners (cons listener (oref this listeners))))

(defmethod jde-db-remove-listener ((this jde-db-debugger) listener)
  "Removes LISTENER from the list of listeners listening for a
response from the debugger.  LISTENER must be an object of type
`jde-db-listener'."
  (assert (typep listener jde-db-listener))
  (oset this listeners (remove listener (oref this listeners))))

(defmethod jde-db-set-process-filter ((this jde-db-debugger))
  "Set debugger process output filter. The default method sets a
function that invokes `jde-db-process-debugger-output'."
  (set-process-filter
   (oref this process)
   (lambda (process output)
     (jde-db-process-debugger-output
      (oref 'jde-db-debugger the-debugger) output))))

(defmethod jde-db-notify-process-exit ((this jde-db-debugger) msg)
  "The default debugger process sentinel invokes this method
when the debugger process terminates."
  (let ((proc (oref this process)))
    (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 ;; Stop displaying an arrow in a source file.
	   (setq overlay-arrow-position nil)
	   (set-process-buffer proc nil))
	  ((memq (process-status proc) '(signal exit))
	   ;; Stop displaying an arrow in a source file.
	   (setq overlay-arrow-position nil)
	   (let* ((obuf (current-buffer)))
	     ;; save-excursion isn't the right thing if
	     ;;  process-buffer is current-buffer
	     (unwind-protect
		 (progn
		   ;; Write something in debugger buffer and hack its mode line,
		   (set-buffer (process-buffer proc))
		   ;; Fix the mode line.
		   (setq mode-line-process
			 (concat ":"
				 (symbol-name (process-status proc))))
		   (force-mode-line-update)
		   (if (eobp)
		       (insert ?\n mode-name " " msg)
		     (save-excursion
		       (goto-char (point-max))
		       (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the command buffer.
	     (set-buffer obuf)))))))


(defmethod jde-db-notify-process-status-change ((this jde-db-debugger) msg)
  "The debugger process sentinel invokes this method when the status of
the debugger process changes. The default method invokes
`jde-db-notify-process-exit'."
  (jde-db-notify-process-exit this msg))


(defmethod jde-db-set-process-sentinel ((this jde-db-debugger))
  (set-process-sentinel
   (oref this process)
   (lambda (process msg)
       (jde-db-notify-process-status-change
	(oref 'jde-db-debugger the-debugger) msg))))

(defmethod jde-db-exec-next-cmd ((this jde-db-debugger))
  "Executes the next command on the debugger's pending
command list."
  (let ((curr-cmd (car (oref this next-cmd))))
    (if curr-cmd
	(progn
	  (oset this next-cmd (cdr (oref this next-cmd)))
	  (oset this last-cmd curr-cmd)
	  (jde-db-cmd-init curr-cmd)
	  (with-current-buffer (oref this buffer)
	    (let ((proc (oref this process))
		  (cmd-line (jde-db-cmd-make-command-line curr-cmd)))
	      (if cmd-line
		  (progn
		    (goto-char (point-max))
		    (insert cmd-line)
		    (comint-send-input)))))))))

(defmethod jde-db-exec-cmds ((this jde-db-debugger) cmds)
  "Executes list of debugger CMDS."
  (oset this next-cmd cmds)
  (jde-db-exec-next-cmd this))

(defmethod jde-db-exec-cmd ((this jde-db-debugger) cmd)
  "Executes CMD."
  (assert (and cmd (typep cmd 'jde-db-cmd)))
  (jde-db-exec-cmds this (list cmd)))

(defmethod jde-db-classpath-arg ((this jde-db-debugger))
  "Generate the -classpath command line argument for jdb."

  ;; Set the classpath option. Use the local
  ;; classpath, if set; otherwise, the global
  ;; classpath.
  (let ((classpath
	 (if jde-db-option-classpath
	     jde-db-option-classpath
	   jde-global-classpath))
	(symbol
	 (if jde-db-option-classpath
	     'jde-db-option-classpath
	   'jde-global-classpath)))
    (if classpath
	(list
	 "-classpath"
	 (jde-build-classpath
	  classpath symbol)))))

(defmethod jde-db-classic-mode-arg ((this jde-db-debugger))
  "Generate the classic mode command-line argument for jdb."
  (if jde-db-classic-mode-vm
      (list "-classic")))

(defmethod jde-db-property-args ((this jde-db-debugger))
  "Generate property arguments."
  (if jde-db-option-properties
      (mapcar
       (lambda (prop)
	 (concat "-D" (car prop) "=" (cdr prop)))
       jde-db-option-properties)))


(defmethod jde-db-verbose-args ((this jde-db-debugger))
  "Get the debugger verbosity arguments for jdb."
  (let ((print-classes-loaded
	 (nth 0 jde-db-option-verbose))
	(print-memory-freed
	 (nth 1 jde-db-option-verbose))
	(print-jni-info
	 (nth 2 jde-db-option-verbose))
	options)

    (if print-classes-loaded
	(add-to-list 'options "-verbose:class"))

    (if print-memory-freed
	(add-to-list 'options "-verbosegc"))

    (if print-jni-info
	(add-to-list options "-verbosejni"))

    options))

(defmethod jde-db-heap-size-args ((this jde-db-debugger))
  "Generate heap size options."
  (let* ((memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")
	       (cons "gigabytes" "g")))
	 (start-cons (nth 0 jde-db-option-heap-size))
	 (start-size (format "%d%s" (car start-cons)
			     (cdr (assoc (cdr start-cons)
					 memory-unit-abbrevs))))
	 (max-cons (nth 1 jde-db-option-heap-size))
	 (max-size (format "%d%s" (car max-cons)
			   (cdr (assoc (cdr max-cons)
				       memory-unit-abbrevs))))
	 options)
    (if (not (string= start-size "1m"))
	(setq options
	      (append options (list (concat "-Xms" start-size)))))
    (if (not (string= max-size "16m"))
	(setq options
	      (append options (list (concat "-Xmx" max-size)))))
    options))

(defmethod jde-db-stack-size-args ((this jde-db-debugger))
  "Generate stack size arguments."
  (let* ((memory-unit-abbrevs
	 (list (cons "bytes" "")
	       (cons "kilobytes" "k")
	       (cons "megabytes" "m")
	       (cons "gigabytes" "g")))
	 (c-cons (nth 0 jde-db-option-stack-size))
	 (c-size (format "%d%s" (car c-cons)
			 (cdr (assoc (cdr c-cons)
				     memory-unit-abbrevs))))
	 (java-cons (nth 1 jde-db-option-stack-size))
	 (java-size (format "%d%s" (car java-cons)
			    (cdr (assoc (cdr java-cons)
					memory-unit-abbrevs))))
	 option)

    (if (not (string= c-size "128k"))
	(setq option
	      (append option (list (concat "-Xss" c-size)))))

    (if (not (string= java-size "400k"))
	(setq option
	      (append option (list (concat "-Xoss" java-size)))))
    option))

(defmethod jde-db-garbage-collection-args ((this jde-db-debugger))
  "Set garbage collection options."
  (let ((no-gc-asynch (not
		       (nth 0 jde-db-option-garbage-collection)))
	(no-gc-classes (not
			(nth 1 jde-db-option-garbage-collection)))
	options)

    (if no-gc-asynch
	(setq options (append options '("-Xnoasyncgc"))))

    (if no-gc-classes
	(setq options (append options '("-Xnoclassgc"))))

    options))

(defmethod jde-db-garbage-collection-arg ((this jde-db-debugger))
  "Generate Java profile arg."
  (let ((profilep (car jde-db-option-java-profile))
	(file (cdr jde-db-option-java-profile)))

    (if profilep
	(if (string= file "./java.prof")
	    (list "-Xprof")
	  (list (concat "-Xprof:" file))))))


(defmethod jde-db-heap-profile-arg ((this jde-db-debugger))
  "Generate heap profile option."
  (let* ((profilep (car jde-db-option-heap-profile))
	 (prof-options (cdr jde-db-option-heap-profile))
	 (file (nth 0 prof-options))
	 (depth (nth 1 prof-options))
	 (top (nth 2 prof-options))
	 (sort
	  (downcase (substring (nth 3 prof-options) 0 1))))
    (if profilep
	(if (and (string= file "./java.hprof")
		 (equal depth 5)
		 (equal top 20)
		 (string= sort "a"))
	    (list "-Xhprof")
	  (list
	   (format
	    "-Xhprof:file=%s,depth=%d,top=%d,sort=%s"
	    file depth top sort))))))

(defmethod jde-db-verify-arg ((this jde-db-debugger))
  ;; Set verify options.
  (let ((verify-all (nth 0 jde-db-option-verify))
	(verify-remote (nth 1 jde-db-option-verify)))
    (if verify-all
	(list"-Xverify")
      ;      (if verify-remote
      ;	  (list "-Xverifyremote"))
      (if (and
	   (not verify-all)
	   (not verify-remote))
	  (list "-Xnoverify")))))


(defmethod jde-db-command-line-args ((this jde-db-debugger))
  "Generate command line args."
  (if jde-db-option-vm-args
      (mapcar
       (lambda (arg)
	 arg)
       jde-db-option-vm-args)))


(defmethod jde-db-host-arg ((this jde-db-debugger))
  (if (not (string= jde-db-option-host ""))
      (list "-host" jde-db-option-host)))

(defmethod jde-db-launch-arg ((this jde-db-debugger))
  "Argument that tells the debugger to launch the
debuggee vm immediately instead of waiting for a
run command. Only the new (JDK 1.3) version of jdb
provides this option."
  nil)

(defmethod jde-db-get-vm-args ((this jde-db-debugger))
  (append
   (jde-db-classic-mode-arg this)
   (jde-db-launch-arg this)
   (jde-db-classpath-arg this)
   (jde-db-property-args this)
   (jde-db-verbose-args this)
   (jde-db-heap-size-args this)
   (jde-db-command-line-args this)))

(defmethod jde-db-debugger-get-working-dir ((this jde-db-debugger))
  (if (string= jde-run-working-directory "")
      default-directory
    (jde-normalize-path 'jde-run-working-directory)))

(defmethod jde-db-debugger-get-prog-args ((this jde-db-debugger))
  )

(defmethod jde-db-debugger-start ((this jde-db-debugger))
  "Start the debugger.")


(defmethod jde-db-debugger-launch ((this jde-db-debugger) main-class)
  "Launch the application whose main class is MAIN-CLASS in debug mode.")


(defmethod jde-db-debugger-connect ((this jde-db-debugger) &optional listenp)
  "Connect the debugger to an existing process.")


(defun jde-db-get-the-debugger ()
  "Get the currently selected debugger. This function
returns an eieio object of type `jde-db-debugger'."
  (if (string= (car jde-debugger) "JDEbug")
      jde-dbs-the-debugger
    (jde-jdb-get-jdb)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Generic Debug Commands                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following commands serve as a generalized interface between the
;; JDE user and JDE-supported debuggers, e.g., jdb or JDEbug.

;; This section is a work in progress. It entails generalizing the
;; existing jdb and JDEbug commands and replacing those commands
;; with the generalized commands.

;;;###autoload
(defun jde-debug ()
  "Run the debugger specified by `jde-debugger' on the Java application
whose source resides in the current buffer. This command determines
the main class of the application either from the variable
`jde-run-application-class' or from the source in the current
buffer. If `jde-run-application-class' does not specify a class, the
main class is assumed to be the class defined by the current source
buffer. This command creates a command buffer for the debug session."
  (interactive)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; This is a temporary hack until I
  ;; wire up the JDEbug to this command.
  (if (string= (car jde-debugger) "JDEbug")
      (jde-bug-debug-app)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let* ((debugger (jde-db-get-the-debugger))
	   (application-main-class
	    (let ((main-class jde-run-application-class))
	      (if (or
		   (not main-class)
		   (string= main-class ""))
		  (setq main-class
			(if (buffer-file-name)
			    (concat (jde-db-get-package)
				    (file-name-sans-extension
				     (file-name-nondirectory (buffer-file-name))))
			  (read-string "Java class to debug: "))))
	      main-class))
	   (cmd-set (oref debugger cmd-set))
	   (launch-cmd (oref cmd-set launch-app)))


      (jde-db-create-debuggee-app debugger application-main-class)

      (if (not (oref debugger running-p))
	  (jde-db-debugger-start debugger))

      (oset-default 'jde-db-debugger the-debugger debugger)

      ;; Forward to the debugger any breakpoint requests made
      ;; by the user before launching the application.
      (if jde-db-breakpoints
	  (let ((bp-cmd (oref (oref debugger cmd-set) set-bp)))
	    (oset
	     bp-cmd
	     breakpoints
	     (mapcar (lambda (assoc-x) (cdr assoc-x)) jde-db-breakpoints))
	    (jde-db-exec-cmds debugger (list launch-cmd bp-cmd)))
	(jde-db-exec-cmd debugger launch-cmd)))))

(defun jde-debugger-running-p ()
  "Returns nonnil if the debugger is running."
  (or
   (and
    (string= (car jde-debugger) "JDEbug")
    (jde-dbs-debugger-running-p)
    (jde-dbs-get-target-process))
   (let ((debugger
	  (if (slot-boundp (jde-db-get-the-debugger) 'the-debugger)
	      (oref (jde-db-get-the-debugger) the-debugger))))
    (and debugger
	 (oref debugger running-p)))))

(defun jde-debug-applet-init (applet-class applet-doc-path)
  (let* ((debugger (jde-db-get-the-debugger))
	 (cmd-set (oref debugger cmd-set))
	 (launch-cmd (oref cmd-set launch-applet))
	 (debug-buf-name (concat "*debug-" applet-class "*"))
	 (applet-doc (file-name-nondirectory applet-doc-path))
	 (applet-doc-directory (file-name-directory applet-doc-path))
	 (source-directory default-directory)
	 (working-directory
	  (if applet-doc-directory
	      applet-doc-directory
	    source-directory)))

    (jde-db-create-debuggee-applet debugger applet-doc-path)

    (oset-default 'jde-db-debugger the-debugger debugger)

    ;; Forward to the debugger any breakpoint requests made
    ;; by the user before launching the application.
    (if jde-db-breakpoints
	(let ((bp-cmd (oref (oref debugger cmd-set) set-bp)))
	  (oset
	   bp-cmd
	   breakpoints
	   (mapcar (lambda (assoc-x) (cdr assoc-x)) jde-db-breakpoints))
	  (jde-db-exec-cmds debugger (list launch-cmd bp-cmd)))
      (jde-db-exec-cmd debugger launch-cmd))))




(defun jde-debug-applet-internal (applet-doc)
  (let ((applet-class jde-run-application-class))
    (if (or
	 (not applet-class)
	 (string= applet-class ""))
	(setq applet-class
	      (concat (jde-db-get-package)
		      (file-name-sans-extension
		       (file-name-nondirectory (buffer-file-name))))))
    (jde-debug-applet-init applet-class  applet-doc)))

;;;###autoload
(defun jde-debug-applet (&optional doc)
  "Runs an applet in the jdb debugger. This function prompts you to enter
the path of an html document that displays the applet. If you
do not enter a path, this function next checks
whether `jde-run-applet-doc' specifies a document. If so, it displays
that specified document. Next, it checks whether the current directory
contains any html files. If so, it displays the first html file that
it finds. If if cannot find an html file, it signals an error.  This
function runs appletviewer in jdb to permit debugging. On startup, it
sets a breakpoint in the init method of the class specified by
`jde-run-application-class' or in the class corresponding to the Java
file in the current buffer."
  (interactive
   (let ((insert-default-directory nil))
     (list (read-file-name "Applet doc: " nil nil nil jde-run-applet-last-doc))))
  (setq jde-run-applet-last-doc doc)
  (let ((applet-doc-path
	 (if doc
	     doc
	   (if (and jde-run-applet-doc
		    (not (string= jde-run-applet-doc "")))
	       jde-run-applet-doc
	     (car (jde-run-find-html-files))))))
    (if applet-doc-path
	(jde-debug-applet-internal applet-doc-path)
      (signal 'error "Could not find html document to display applet."))))



(defun jde-debug-run ()
  "Start the current debuggee application."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status running-p))
	(error "Application %s is already running."
	       (oref debuggee main-class))
      (let* ((cmd-set (oref debugger cmd-set))
	     (run (oref cmd-set run)))
	(oset debuggee-status running-p t)
	(oset debuggee-status stopped-p nil)
	(oset debuggee-status suspended-p nil)
	(jde-db-exec-cmd debugger run)))))


(defun jde-debug-cont ()
  "Continues the current debuggee application from its current
stopping point."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (or
	      (oref debuggee-status stopped-p)
	      (oref debuggee-status suspended-p)))
	(let* ((cmd-set (oref debugger cmd-set))
	       (cont (oref cmd-set cont)))
	  (oset debuggee-status stopped-p nil)
	  (oset debuggee-status suspended-p nil)
	  (jde-db-exec-cmd debugger cont))
      (let ((class (oref debuggee main-class)))
	(message "Application %s is not stopped" class)))))


(defun jde-debug-quit ()
  "Quit debugging the current application."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status running-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (quit (oref cmd-set quit)))
	  (jde-db-exec-cmd debugger quit))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not running" class)))))

(defun jde-debug-step-over ()
  "Step to the next line in the current stack frame."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-over (oref cmd-set step-over)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-over))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-step-into ()
  "Step to the next line in the current program."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-into (oref cmd-set step-into)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-into))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-step-out ()
  "Continue execution to the end of the current method."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (step-out (oref cmd-set step-out)))
	  (oset debuggee-status stopped-p nil)
	  (jde-db-exec-cmd debugger step-out))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))


(defun jde-debug-up ()
  "Move up the stack."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (up (oref cmd-set up)))
	  (jde-db-exec-cmd debugger up))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-down ()
  "Move down the stack."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (down (oref cmd-set down)))
	  (jde-db-exec-cmd debugger down))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-debug-where ()
  "Show current stopping point."
  (interactive)
  (jde-assert-source-or-debug-buffer)
  (let* ((debugger (oref 'jde-db-debugger the-debugger))
	 (debuggee (oref debugger debuggee))
	 (debuggee-status (oref debuggee status)))
    (if (and (oref debugger running-p)
	     (oref debuggee-status stopped-p))
	(let* ((cmd-set (oref debugger cmd-set))
	       (where (oref cmd-set where)))
	  (jde-db-exec-cmd debugger where))
      (let ((class (oref debuggee main-class)))
	(error "Application %s is not stopped" class)))))

(defun jde-db-spec-breakpoint ()
  "Creates a specification for the breakpoint at the
current line in the current file. Returns an object of
type `jde-db-breakpoint'."
  (let ((file (buffer-file-name)))
    (setq jde-db-breakpoint-id-counter
	  (1+ jde-db-breakpoint-id-counter))
    (jde-db-breakpoint
     (format "breakpoint: %s %d"
	     (file-name-nondirectory file)
	     jde-db-breakpoint-id-counter)
     :id   jde-db-breakpoint-id-counter
     :file file
     :class (concat (jde-db-get-package)
		    (jde-db-get-class)))))


(defun jde-debug-set-breakpoint ()
  "Ask debugger to set a breakpoint at the current line
in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line)))
    (unless bp
      (setq bp (jde-db-spec-breakpoint))
      (oset bp line line)
      (jde-db-breakpoints-add bp)
      (if (jde-db-debuggee-running-p)
	  (let* ((debugger (oref 'jde-db-debugger the-debugger))
		 (bp-cmd (oref (oref debugger cmd-set) set-bp)))
	    (oset bp-cmd breakpoints (list bp))
	    (jde-db-exec-cmd debugger bp-cmd))))))

(defun jde-debug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (jde-assert-source-buffer)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line)))
    (if bp
	(if (jde-db-debuggee-running-p)
	    (let* ((debugger (oref 'jde-db-debugger the-debugger))
		   (bp-cmd (oref (oref debugger cmd-set) clear-bp)))
	      (oset bp-cmd breakpoints (list bp))
	      (jde-db-exec-cmd debugger bp-cmd))
	  (jde-db-delete-breakpoint bp)))))

(defun jde-debug-toggle-breakpoint ()
  "Sets or clears a breakpoint at the current line."
  (interactive)
  (assert (eq major-mode 'jde-mode) nil
	  "This command works only in a Java source buffer.")
  (let*  ((file (buffer-file-name))
	  (line (jde-get-line-at-point))
	  (bp (jde-db-find-breakpoint file line)))
    (assert (jde-db-src-dir-matches-file-p file) nil
	    "You cannot set a breakpoint in a file that is not in `jde-sourcepath'.")
    (if bp
	(jde-debug-clear-breakpoint)
      (jde-debug-set-breakpoint))))

(defun jde-debug-clear-breakpoints()
  "Clear all existing breakpoints."
  (interactive)
  (if jde-db-breakpoints
      (if (jde-db-debuggee-running-p)
	  (let* ((debugger (oref 'jde-db-debugger the-debugger))
		 (bp-cmd (oref (oref debugger cmd-set) clear-bp)))
	    (oset
	     bp-cmd
	     breakpoints
	     (mapcar
	      (lambda (assoc-x) (cdr assoc-x))
	      jde-db-breakpoints))
	    (jde-db-exec-cmd debugger bp-cmd))
	(loop for bp-assoc in jde-db-breakpoints do
	      (let ((bp (cdr bp-assoc)))
		(jde-db-delete-breakpoint bp))))))


(defvar jde-db-minibuffer-local-map nil
  "Keymap for minibuffer prompting of jdb startup command.")
(if jde-db-minibuffer-local-map
    ()
  (setq jde-db-minibuffer-local-map (copy-keymap minibuffer-local-map))
  (define-key
    jde-db-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename))


(defun class-from-file-name (file-name)
  (file-name-sans-extension (file-name-nondirectory file-name)))


(defun jde-db-get-vm-args-from-user ()
  (if jde-db-read-vm-args
      (jde-run-parse-args
       (read-from-minibuffer
	"Vm args: "
	(car jde-db-interactive-vm-arg-history)
	nil nil
	'jde-db-interactive-vm-arg-history))))

(defun jde-db-get-app-args-from-user ()
  (if jde-db-read-app-args
      (jde-run-parse-args
       (read-from-minibuffer
	"Application args: "
	(car jde-db-interactive-app-arg-history)
	nil nil
	'jde-db-interactive-app-arg-history))))


(defun jde-db-get-package ()
  "Return the package of the class whose source file resides in the current
buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*\\<\\(package\\) +\\([^ \t\n]*\\) *;" (point-max) t)
	(concat (buffer-substring-no-properties (match-beginning 2) (match-end 2))
		"."))))

(defun jde-db-get-class () "Lookups and return fully qualified class
name, e.g. A$B if point is in inner class B of A."
  (interactive)
  (let ((class-info (jde-parse-get-innermost-class-at-point)))
    (if class-info
      (save-excursion
	(goto-char (cdr class-info))
	(let ((parent (jde-db-get-class)))
	(if (not parent)
	    (car class-info)
	    (concat parent "$" (car class-info))))))))


(defun jde-db-src-dir-matches-file-p (file)
  "Return true if one of `jde-sourcepath'
matches FILE."
  (let* ((directory-sep-char ?/)
		 (filename (jde-normalize-path file)))
    (find-if
     (lambda (dir-x)
       (string-match
		(concat "^" dir-x)
		filename))
     (jde-expand-wildcards-and-normalize jde-sourcepath 'jde-sourcepath))))


(provide 'jde-db)

;;; End of jde-db.el
