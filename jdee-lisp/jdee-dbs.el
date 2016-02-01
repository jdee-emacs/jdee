;;; jdee-dbs.el -- JDEbug Session Interface Functions

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
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

;; This is one of a set of packages that make up the JDEE.
;; See the JDEE User's Guide for more information.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'jdee-db)
(require 'jdee-dbo)
(require 'tree-widget)

;; FIXME: refactor to eliminate these
(defvar jdee-bug-debug)
(defvar jdee-bug-debugger-command-timeout)
(defvar jdee-bug-debugger-host-address)
(declare-function jdee-bug-vm-includes-jpda-p "jdee-bug" ())
(declare-function jdee-build-classpath "jdee" (paths &optional symbol quote-path-p))
(declare-function jdee-normalize-path "jdee" (path &optional symbol))
(declare-function jdee-get-tools-jar "jdee" nil)

;; Need jdee-run only to get the definition for
;; save-w32-show-window macro. FIXME: refactor
(eval-when-compile
  (require 'jdee-run))

(defcustom jdee-bug-sio-connect-delay 1
  "Length of time in seconds that the JDE waits
before attempting to connect to the
debuggee application's standard I/O. This delay
is intended to give JDEbug time to create the
SIO socket. Try increasing this variable if JDEbug
hangs while launching an application. If your
system never hangs, you can reduce this setting
to 0 to eliminate the connection delay."
  :group 'jdee-bug
  :type 'integer)

(defvar jdee-dbs-comint-filter nil
  "Standard comint filter for debugger buffer.")

(defvar jdee-dbs-debugger-process-name "jdebug"
"Name of debugger process.")

(defun jdee-dbs-get-debugger-process ()
  (get-process jdee-dbs-debugger-process-name))


(defvar jdee-dbs-debugger-output-buffer-name "*JDEbug Messages*"
"Name of buffer used to display messages from the debugger.")

(defvar jdee-dbs-debugger-socket-process-name "jdebug-socket"
"Name of debugger socket process.")

(defvar jdee-dbs-debugger-hook nil
  "Hook to run when starting or stopping the debugger.
The hook is run with a single argument which is non-nil when the
debugger is starting and nil when it is quitting.")

(defun jdee-dbs-get-debugger-socket-process ()
  (get-process jdee-dbs-debugger-socket-process-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Set                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-set ()
  ((proc-alist     :initarg :proc-alist
		   :type list
		   :initform nil
		   :documentation
		   "List of active debugee processes"))
  "Class of debuggee process sets.")

(defmethod jdee-dbs-proc-set-add ((this jdee-dbs-proc-set) process)
  "Adds a process to this debuggee process set."
  (oset this :proc-alist
	(cons
	 (cons (oref process :id) process)
	 (oref this :proc-alist))))

(defmethod jdee-dbs-proc-set-remove ((this jdee-dbs-proc-set) process)
  (oset this :proc-alist
	(cl-remove-if
	 (lambda (assoc-x)
	   (let* ((xproc (cdr assoc-x))
		  (xid (oref xproc id))
		  (id (oref process id)))
	     (equal xid id)))
	 (oref this proc-alist))))

(defmethod jdee-dbs-proc-set-get-proc ((this jdee-dbs-proc-set) id)
  (cdr (assq id (oref this :proc-alist))))

(defmethod jdee-dbs-proc-set-find ((this jdee-dbs-proc-set) field value)
  "Finds the process in the set whose FIELD is equal to VALUE."
  (if (slot-boundp this :proc-alist)
      (cdr (cl-find-if
	(lambda (assoc-x)
	  (let ((process-x (cdr assoc-x)))
	    (equal (eieio-oref process-x field) value)))
	(oref this :proc-alist)))))

(defmethod jdee-dbs-proc-set-contains-p ((this jdee-dbs-proc-set) process)
  (assq (oref process :id) (oref this :proc-alist)))

(defmethod jdee-dbs-proc-set-get-size ((this jdee-dbs-proc-set))
  "Gets the number of processes in this set."
  (if (slot-boundp this 'proc-alist)
      (length (oref this proc-alist))
    0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Registry                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-registry (jdee-dbs-proc-set)
  ((target-process :initarg :target-process
		   :type jdee-dbs-proc
		   :documentation
		   "Process that currently has the debugger command focus."))
  "Class of process registries.")


(defmethod jdee-dbs-proc-registry-set-target-proc ((this jdee-dbs-proc-registry) &optional id)
  "Sets process specified by ID to be the target process. If ID is not specified, the first
registered process becomes the target process"
  (let ((target-process
	  (if id
	      (let ((process (jdee-dbs-proc-set-get-proc this id)))
		(if process
		    (if (jdee-dbs-proc-set-contains-p this process)
			process
		      (message "Error: process %s is dead." id)
		      nil)
		  (message "Error: process %s does not exist." id)
		  nil))
	    (let ((existing-processes
		   (oref jdee-dbs-the-process-registry :proc-alist)))
	      (if existing-processes (cdr (nth 0 existing-processes)))))))
    (when target-process
      (oset this target-process target-process)
      (set-window-configuration (oref target-process win-cfg)))
    target-process))


(defvar jdee-dbs-the-process-registry
  (jdee-dbs-proc-registry "Process Registry")
  "The debuggee process registry.")

(defun jdee-dbs-get-target-process ()
  (and jdee-dbs-the-process-registry
       (slot-boundp jdee-dbs-the-process-registry :target-process)
       (oref jdee-dbs-the-process-registry :target-process)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process Morgue                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-morgue (jdee-dbs-proc-set) ()
  "Class of process morgues. A process morgue contains dead or dying processes.
Their carcasses must be kept around until the debugger stops sending messages
concerning them." )

(defmethod jdee-dbs-proc-morgue-bury-the-dead ((this jdee-dbs-proc-morgue))
  (mapc
   (lambda (dead-proc-assoc)
     (let* ((dead-proc (cdr dead-proc-assoc))
	    (cli-buffer (if (slot-boundp dead-proc 'cli-buf) (oref dead-proc cli-buf)))
	    (msg-buffer (if (slot-boundp dead-proc 'msg-buf) (oref dead-proc msg-buf)))
	    (locals-buffer (if (slot-boundp dead-proc 'locals-buf) (oref dead-proc locals-buf)))
	    (threads-buffer (if (slot-boundp dead-proc 'threads-buf) (oref dead-proc threads-buf))))
       (if cli-buffer (kill-buffer cli-buffer))
       (if msg-buffer (kill-buffer msg-buffer))
       (if locals-buffer (kill-buffer locals-buffer))
       (if threads-buffer (kill-buffer threads-buffer))))
   (oref this proc-alist))
  (oset this proc-alist nil))


(defvar jdee-dbs-the-process-morgue
  (jdee-dbs-proc-morgue "Process Morgue")
  "The JDE process morgue. This morgue contains processes that are dead or
dying, for example, because they have been terminated by the user or the
debugger. Their corpses must be kept around until it is clear they are dead and
the debugger ceases sending messages concerning them.")


(defun jdee-dbs-get-process (id)
"Get the process whose id is ID. This function looks first in the process registry
and then in the process morgue for the process."
  (let ((process
	 (jdee-dbs-proc-set-get-proc jdee-dbs-the-process-registry id)))
    (if (not process)
	(setq process (jdee-dbs-proc-set-get-proc jdee-dbs-the-process-morgue id)))
    process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Process State Info                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-state-info ()
  ((state       :initarg :state)
   (reason      :initarg :reason)
   (thread-id   :initarg :thread-id)
   (thread-name :initarg :thread-name))
  "Class of process state information objects.")


(defmethod jdee-dbs-proc-state-info-set ((this jdee-dbs-proc-state-info)
					state reason thread-id thread-name)
  (oset this reason reason)
  (oset this state state)
  (oset this thread-id thread-id)
  (oset this thread-name thread-name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Specification                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-bpspec ()
  ((id         :initarg :id
	       :type integer
	       :documentation
	       "Id assigned to this breakpoint by the debugger.")
   (breakpoint :initarg :breakpoint
	       :type jdee-db-breakpoint
	       :documentation
	       "Instance of `jdee-db-breakpoint'.")
   (resolved   :initarg :resolved))
  (:allow-nil-initform t)
  "Class of breakpoint specifications. A breakpoint specification contains
process-specific information about a breakpoint")


;; Defines a class of containers for breakpoint specs.
;; Each container lists the process specs for breakpoints set in a
;; particular process.

(defun jdee-dbs-proc-bpspecs-add (bpspecs bpspec)
  "Adds BPSPEC to BPSPECS, a process's breakpoint spec list."
  (cons
   (cons (oref bpspec id) bpspec)
   bpspecs))

(defun jdee-dbs-proc-bpspecs-remove (bpspecs bpspec)
  "Removes BPSPEC from BPSPECS"
  (cl-remove-if (lambda (x)
	       (equal (car x) (oref bpspec id) ))
	     bpspecs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Request Class                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-request ()
  ((id                  :initarg :id
			:type integer
			:documentation
			"Trace request id")
   (suspend-policy      :initarg :suspend-policy
			:type string
			:initform "none"
			:documentation
			"Valid values are all (all threads), thread (current thread), or none")
   (inclusion-filters   :initarg :inclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to include in trace.")
   (exclusion-filters   :initarg :exclusion-filters
			:type list
			:documentation
			"List of regular expressions specifying classes to exclude from trace.")
   (cancel-command      :initarg :cancel-command
			:type string
			:documentation
			"Name of command used to cancel this request.")
   )
"Super class of trace requests."
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Method Request Class                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-methods-request (jdee-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "entry"
			:documentation
			"Entry or exit.")
   (thread-restriction  :initarg :thread-restriction
			:type string
			:documentation
			"Thread to trace."))
   "Trace methods request."
)

(defmethod initialize-instance ((this jdee-dbs-trace-methods-request) &rest fields)
  "Constructor for objects of `jdee-dbs-trace-methods-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_methods"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Request Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-classes-request (jdee-dbs-trace-request)
   ((trace-type         :initarg :trace-type
			:type string
			:initform "preparation"
			:documentation
			"Valid values are preparation or unloading."))
   "Trace classes request."
)

(defmethod initialize-instance ((this jdee-dbs-trace-classes-request) &rest fields)
  "Constructor for objects of `jdee-dbs-trace-classes-request' class."
  (call-next-method)
  (oset this cancel-command "cancel_trace_classes"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Request Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-exceptions-request (jdee-dbs-trace-request)
  ((exception-class    :initarg :exception-class
		       :type string
		       :documentation
		       "Class of exceptions to trace. Can be a wild card pattern.")
   (trace-type         :initarg :trace-type
		       :type string
		       :initform "both"
		       :documentation
			"Valid values are caught, uncaught, or both."))
   "Trace exceptions request."
)

(defmethod initialize-instance ((this jdee-dbs-trace-exceptions-request) &rest fields)
  "Constructor for objects of `jdee-dbs-trace-exceptions-request' class."
  (call-next-method)
  (oset this cancel-command "clear"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Request Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-watch-field-request (jdee-dbs-trace-request)
  ((watch-type         :initarg :watch-type
		       :type string
		       :documentation
		       "Valid values are \"access\" and \"modification\".")
   (object-class       :initarg :object-class
		       :type string
		       :documentation
		       "Class of object to watch. Can be a wild card pattern.")
   (field-name         :initarg :field-name
		       :type string
		       :documentation
			"Name of field to watch.")
   (expression         :initarg :expression
		       :type string
		       :documentation
		       "Boolean expression that must be satisfied to suspend execution.")
   (object-id          :initarg :object-id
		       :type string
		       :documentation
		       "Id of object to watch."))
   "Watch field request."
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Status                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc-status (jdee-db-debuggee-status)
   ((startup-p     :initarg :startupp
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if this process is in the startup state.")
    (steppable-p  :initarg :steppablep
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if this process can be single-stepped."))
  "Status of process being debugged with JDEbug.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debuggee Process Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-proc (jdee-db-debuggee-app)
  ((id            :initarg :id
		  :type integer
		  :documentation
		  "Id assigned by the JDE.")
   (cli-socket    :initarg :cli-socket
		  :type integer
		  :documentation
		  "Number of socket used by the process's command line interface.")
   (cli-buf       :initarg :cli-buf
		  :type buffer
		  :documentation
		  "Buffer for the process's command-line interface.")
   (msg-buf       :initarg :msf-buf
		  :type buffer
		  :documentation
		  "Buffer used to display debugger output for this process")
   (threads-buf   :initarg :threads-buf
		  :type buffer
		  :documentation
		  "Buffer used to display threads.")
   (locals-buf    :initarg :locals-buf
		  :type buffer
		  :documentation
		  "Buffer used to display local variables.")
   (startupp       :initarg :startupp
		  :type boolean
		  :initform nil
		  :documentation
		  "non-nil if this process is in the startup state.")
   (suspendedp    :initarg :suspendedp
		  :type boolean
		  :initform nil
		  :documentation
		  "non-nil if this process has been suspended by the debugger.")
   (steppablep    :initarg :steppablep
		  :type boolean
		  :initform nil
		  :documentation
		  "non-nil if this process can be single-stepped.")
   (state-info    :initarg :state-info
		  :type jdee-dbs-proc-state-info
		  :documentation
		  "Process state information.")
   (stack         :initarg :stack
		  :type list
		  :documentation
		  "Lists stack frames for thread of current step or breakpoint.")
   (stack-ptr     :initarg :stack-ptr
		  :type integer
		  :initform 0
		  :documentation
		  "Points to the current frame on the stack.")
   (trace-req     :initarg :trace-req
		  :type list
		  :documentation
		  "List of outstanding trace requests.")
   (watch-req     :initarg :watch-req
		  :type list
		  :documentation
		  "List of outstanding watch field requests.")
   (object-refs   :initarg :object-refs
		  :type list
		  :initform nil
		  :documentation
		  "IDs of debuggee objects currently referenced by the debugger.")
   (bpspecs       :initarg :bpspecs
		  :type list
		  :documentation
		  "Breakpoints set in this process.")
   (last-cmd      :initarg :last-cmd
		  :type jdee-dbs-cmd
		  :documentation
		  "Most recent command targeting this process.")
   (win-cfg       :initarg :win-cfg
		  :type window-configuration
		  :documentation
		  "Desired window configuration for this process.")
   (attachedp     :initarg :attachedp
		  :type boolean
		  :initform nil
		  :documentation
		  "Non-nil if the debugger was attached to this process."))
  (:allow-nil-initform t)
  "Class of debuggee processes.")

(defmethod initialize-instance ((this jdee-dbs-proc) &rest fields)
  "Constructor for objects of `jdee-dbs-proc' class."
  (call-next-method)

  (if (not (slot-boundp this 'state-info))
      (oset this state-info
	    (jdee-dbs-proc-state-info
	     (format "State Info %d" (oref this id)))))

  (assert (slot-boundp this 'main-class))
  (assert (slot-boundp this 'id))

  (oset this msg-buf (get-buffer-create
		      (format "*Process %s(%d)*"
			      (oref this main-class)
			      (oref this id))))
  (with-current-buffer (oref this msg-buf)
    (erase-buffer)
    (goto-char (point-min))
    (insert
       (format "*** Debugger Output for Process %s(%d) ***\n\n"
	       (oref this main-class)
	       (oref this id))))

  (oset this locals-buf (get-buffer-create
			 (format "*%s(%d) Local Variables*"
				 (oref this main-class)
				 (oref this id))))

  (oset this threads-buf (get-buffer-create
			  (format "*%s(%d) Threads*"
				  (oref this main-class)
				  (oref this id)))))


(defmethod jdee-dbs-proc-set-state ((this jdee-dbs-proc) state)
  (let ((state-info (oref this state-info)))
    (oset state-info state state)))

(defmethod jdee-dbs-proc-set-state-reason ((this jdee-dbs-proc) reason)
  (let ((state-info (oref this state-info)))
    (oset state-info reason reason)))

(defmethod jdee-dbs-proc-get-state ((this jdee-dbs-proc))
  (oref (oref this state-info) state))

(defmethod jdee-dbs-proc-get-state-reason ((this jdee-dbs-proc))
  (oref (oref this state-info) reason))

(defmethod jdee-dbs-proc-display-debug-message ((this jdee-dbs-proc)
					       message
					       &optional pop-buffer)
  (let ((buffer
	 (oref this msg-buf)))
    (if buffer
	(save-excursion
	  (let ((source-window (selected-window))
		(currbuffp (equal buffer (current-buffer)))
		win)
	    (if (not currbuffp) (other-window -1))
	    (set-buffer buffer)
	    (goto-char (point-max))
	    (insert (concat message "\n"))
	    (goto-char (point-max))
	    (if (not currbuffp) (other-window 1))
	    (if (and pop-buffer (one-window-p))
		(progn
		  (setq win (split-window source-window))
		  (set-window-buffer win buffer)))
	    (if pop-buffer
		(progn
		  (set-window-buffer (next-window source-window) buffer)
		  (select-window source-window))
	      (if (not currbuffp)
		  (message message))))))))

(defmethod jdee-dbs-proc-move-to-morgue ((this jdee-dbs-proc))
  "Moves this process from the process registry to the process morgue."
  (jdee-dbs-proc-set-remove jdee-dbs-the-process-registry this)
  (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue this))

(defmethod jdee-dbs-proc-move-to-registry ((this jdee-dbs-proc))
  "Moves this process from the registry to the morgue."
  (jdee-dbs-proc-set-remove jdee-dbs-the-process-morgue this)
  (jdee-dbs-proc-set-add jdee-dbs-the-process-registry this))


(defmethod jdee-dbs-proc-get-bpspec ((this jdee-dbs-proc) bp)
  "Gets the process specification for a breakpoint. BP may be either
an instance of `jdee-db-breakpoint' or the debugger-assigned id
for the breakpoint."
  (if (slot-boundp this 'bpspecs)
      (let ((bpspecs (oref this bpspecs)))
	(if (and (object-p bp) (jdee-db-breakpoint-p bp))
	    (let* ((jdee-id (oref bp id)))
	      (cdr
	       (cl-find-if
		(lambda (assoc-x)
		  (let ((spec (cdr assoc-x)))
		    (equal (oref (oref spec breakpoint) id) jdee-id)))
		bpspecs)))
	  (cdr (assoc bp bpspecs))))))

(defmethod jdee-dbs-proc-runnable-p ((this jdee-dbs-proc))
  (or
   (oref this startupp)
   (oref this suspendedp)
   (oref this steppablep)))

(defun jdee-dbs-target-process-runnable-p ()
  (interactive)
  (let ((target (jdee-dbs-get-target-process)))
    (and target (jdee-dbs-proc-runnable-p target))))

(defun jdee-dbs-target-process-steppable-p ()
  (interactive)
  (let ((target (jdee-dbs-get-target-process)))
    (and target (oref target steppablep))))

(defun jdee-dbs-display-debug-message (proc-id message)
  (let ((process (jdee-dbs-get-process proc-id)))
    (if process
	(jdee-dbs-proc-display-debug-message process message)
      (message message))))

(defvar jdee-dbs-proc-counter 0
  "Process counter. Used to generate process IDs.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java Object                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-java-obj ()
  ((jtype  :initarg :jtype
	   :type string
	   :documentation
	   "Type of this object."))
  "Superclass of Java objects.")

(defmethod jdee-dbs-java-obj-to-string ((this jdee-dbs-java-obj))
  "")


(defclass jdee-dbs-java-primitive (jdee-dbs-java-obj)
  ((value :initarg :value
	  :type (or string number)
	  :documentation
	  "Value of this primitive object."))
  "Class of Java primitives.")

(defmethod jdee-dbs-java-obj-to-string ((this jdee-dbs-java-primitive))
  (format "%s" (oref this value)))

(defclass jdee-dbs-java-null (jdee-dbs-java-obj) ()
  "Java null object.")

(defmethod initialize-instance ((this jdee-dbs-java-null) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (oset this jtype "null"))


(defmethod jdee-dbs-java-obj-to-string ((this jdee-dbs-java-null))
  "null")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java Variable                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-java-variable ()
  ((name         :initarg :name
		 :type string
		 :documentation
		 "Name of this variable")
   (jtype        :initarg :jtype
		 :type string
		 :documentation
		 "Type of this variable.")
   (value        :initarg :value
		 :type jdee-dbs-java-obj
		 :documentation
		 "Value of this variable."))
  "Class that defines the JDE's representation of a Java variable.")

(defmethod jdee-dbs-java-variable-to-string ((this jdee-dbs-java-variable))
  (format "%s %s = %s"
	  (oref this jtype)
	  (oref this name)
	  (jdee-dbs-java-obj-to-string (oref this value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java Class Instance                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-java-class-instance (jdee-dbs-java-obj)
  ((id           :initarg :id
		 :type integer
		 :documentation
		 "Id assigned to this object by the debugger.")
   (gc-flag      :initarg :gc-flag
		 :type boolean
		 :documentation
		 "t if this object has been garbage collected."))
  "Instance of a Java class accessed via the debugger.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java Array                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-java-array (jdee-dbs-java-class-instance)
  ((length     :initarg :length
	       :type integer
	       :documentation
	       "Length of this array.")
   (elements   :initarg :elements
	       :type list
	       :initform nil
	       :documentation
	       "Elements of this array."))
  "Class of Lisp objects representing instances of Java arrays.")



(defmethod jdee-dbs-java-obj-to-string ((this jdee-dbs-java-array))
  (let ((str (format "<%s:%d%s> %d"
		     (if (slot-boundp this :jtype)
			 (oref this jtype))
		     (if (slot-boundp this :id)
			 (oref this id))
		     (if (slot-boundp this :gc-flag)
			 (if (oref this gc-flag) ":gc" ""))
		     (if (slot-boundp this :length)
			 (oref this length)
		       0)))
	(elements (if (slot-boundp this :elements)
		      (oref this elements))))
    (if elements
	(let ((sep "\n |- "))
	  (concat
	   str
	   sep
	   (mapconcat
	    (lambda (element)
	      (jdee-dbs-java-obj-to-string element))
	    elements sep)))
      str)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java User-Defined Class Instance                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-java-udci (jdee-dbs-java-class-instance)
  ((fields       :initarg :fields
		 :type list
		 :initform nil
		 :documentation
		 "Fields of this object."))
  "Class of Lisp objects representing instances of user-defined Java classes.")


(defmethod jdee-dbs-java-udci-add-field ((this jdee-dbs-java-udci) field)
  (oset this fields
	(nconc (oref this fields) (list (cons (oref field name) field)))))


(defmethod jdee-dbs-java-obj-to-string ((this jdee-dbs-java-udci))
  (let ((str (format "<%s:%d%s>"
		     (oref this jtype)
		     (oref this id)
		     (if (oref this gc-flag) ":gc" "")))
	(fields (oref this fields)))
    (if fields
	(let ((sep "\n |- "))
	  (concat
	   str
	   sep
	   (mapconcat
	    (lambda (assoc-x)
	      (jdee-dbs-java-variable-to-string (cdr assoc-x)))
	    fields sep)))
      str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Debugger Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-debugger (jdee-db-debugger)
  ((comint-filter :initarg :comint-filter)
   (started-p     :initarg :started-p
		  :initform nil
		  :type boolean
		  :documentation
		  "True if debugger started successfully."))
  "Class of JDEbug debuggers.")

(defmethod initialize-instance ((this jdee-dbs-debugger) &rest fields)
  "Constructor for JDEbug."
  (oset this :name "JDEbug")
  (oset this :buffer-name "*JDEbug*"))


(defmethod jdee-dbs-debugger-register-process-filter ((debugger jdee-dbs-debugger) filter)
  "Set the process filter for the debugger to FILTER."
  (set-process-filter  (oref debugger process) filter))


(defmethod jdee-dbs-debugger-display-message ((debugger jdee-dbs-debugger) message)
  "Displays message in the debugger process buffer."
 (let ((buffer
	 (oref debugger buffer)))
    (if buffer
	(with-current-buffer buffer
	  (goto-char (process-mark (get-buffer-process buffer)))
	  (insert-before-markers (concat message "\n"))))))

(defmethod jdee-dbs-debugger-start ((this jdee-dbs-debugger))
  "Starts the debugger."
  (if (jdee-dbs-debugger-running-p)
      (progn
	(message "An instance of the debugger is running.")
	(pop-to-buffer (jdee-dbs-get-app-buffer-name))
	nil)
    (let* ((debugger-buffer-name
	      (oref this buffer-name))
	     (debugger-buffer
	      (let ((old-buf (get-buffer debugger-buffer-name)))
		    (if old-buf (kill-buffer old-buf))
		    (get-buffer-create debugger-buffer-name)))
	     (win32-p (eq system-type 'windows-nt))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (source-directory default-directory)
	     (working-directory
	      (if (and
		   jdee-run-working-directory
		   (not (string= jdee-run-working-directory "")))
		  (jdee-normalize-path 'jdee-run-working-directory)
		source-directory))
	     (vm (oref (jdee-run-get-vm) :path))
	     (vm-args
		(let (args)
		  (setq args
			(append
			 args
			 (list
			  "-classpath"
			  (jdee-build-classpath
			       (list
                                 jdee-server-dir
				 (if (jdee-bug-vm-includes-jpda-p)
				   (jdee-get-tools-jar)
				   (expand-file-name
				    "lib/jpda.jar" (jdee-normalize-path
						    'jdee-bug-jpda-directory))))))))
		  (if jdee-bug-debug
		      (setq args
			    (append args
			     (list "-Xdebug"
				   "-Xnoagent"
				   "-Xrunjdwp:transport=dt_socket,address=2112,server=y,suspend=n"))))
		  (setq args (append args (list "jde.debugger.Main")))
		  args))
	     (command-string
	      (concat
	       vm " "
	       (jdee-run-make-arg-string
		vm-args)
	       "\n\n"))
	     debugger-process)
	(run-hook-with-args 'jdee-dbs-debugger-hook t)
	(oset this started-p nil)
	(setq jdee-dbs-debugger-output nil)


	(with-current-buffer debugger-buffer
	  (erase-buffer)
	  ;; Set working directory
	  (if (and
	       (file-exists-p working-directory)
	       (file-directory-p working-directory))
	      (cd working-directory)
	    (error "Invalid working directory: %s" working-directory))
	  (insert (concat "cd " working-directory "\n"))
	  (insert command-string)
	  (jdee-run-mode))

	(save-w32-show-window
	 (comint-exec debugger-buffer debugger-buffer-name vm nil vm-args)
	 (setq debugger-process (get-process debugger-buffer-name))
	 (oset this process debugger-process)
	 (oset this buffer debugger-buffer)
	 (oset this comint-filter (process-filter debugger-process))
	 (jdee-dbs-debugger-register-process-filter this 'jdee-dbs-asynch-output-listener)
	 )

	(cd source-directory)

	(bury-buffer debugger-buffer)

	(setq jdee-dbs-proc-counter 0)

	(setq jdee-dbs-cmd-counter 0)

	;; Wait for response from debugger
	(if (not (accept-process-output debugger-process jdee-bug-debugger-command-timeout 0))
	    (progn
	      (message "Error: debugger failed to start.")
	      nil)
	  (oref this started-p))

	;; Create a process registry for registering debuggee processes
	;; started by the debugger.
	(setq jdee-dbs-the-process-registry
	      (jdee-dbs-proc-registry "Process Registry"))

	;; Create a registry for debuggee processes that have died but
	;; still may be getting messages from the debugger.
	(setq jdee-dbs-the-process-morgue
	      (jdee-dbs-proc-morgue "Process Morgue")))))




(defmethod jdee-dbs-debugger-quit ((debugger jdee-dbs-debugger))
  (jdee-dbs-do-command -1 "quit")
  (run-hook-with-args 'jdee-dbs-debugger-hook nil)
  (slot-makeunbound debugger :process)
  (slot-makeunbound debugger :buffer)
  (slot-makeunbound debugger :comint-filter))

(defun jdee-dbs-debugger-running-p ()
  "*Returns t if the debugger is running."
  (and (slot-boundp jdee-dbs-the-debugger 'buffer)
       (oref jdee-dbs-the-debugger started-p)
       (comint-check-proc (oref jdee-dbs-the-debugger buffer))))

(defmethod jdee-db-debugger-launch ((this jdee-dbs-debugger) main-class)
  "Launch the application whose main class is MAIN-CLASS in debug mode."
  )

(defvar jdee-dbs-the-debugger (jdee-dbs-debugger "JDEbug")
  "The debugger.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; JDEbug Command Line Commands                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-cmd (jdee-db-cmd)
  ((process    :initarg :process
	       :type jdee-dbs-proc
	       :documentation
	       "Process that this command targets.")
   (id         :initarg :id
	       :type integer
	       :documentation
	       "Command id.")
   (name       :initarg :name
	       :type string
	       :documentation
	       "Name of command.")
   (result     :initarg :result
	       :documentation
	       "Result of executing command.")
   (data       :initarg :data
	       :documentation
	       "Data returned by command.")
   (msg        :initarg :msg
	       :type string
	       :documentation
	       "Message to display to user in debug buffer.")
   )
  "Super class of debugger commands.")


(defmethod initialize-instance ((this jdee-dbs-cmd) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."
  (call-next-method)
  (setq jdee-dbs-cmd-counter (+ jdee-dbs-cmd-counter 1))
  (oset this id jdee-dbs-cmd-counter))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-cmd))
  "Creates the command line for this command by concatentating
the process id, command id, and command name. If there is no
process, specifies the process id as -1. Derived classes can
extend this method to specify command arguments."
  (let* ((process (oref this process))
	 (process-id (if process (oref process id) -1))
	 (command-id (oref this id))
	 (command-name (oref this name)))
    (format "%s %s %s" process-id command-id command-name)))

(defvar jdee-dbs-debugger-output nil
  "Contains output from the debugger.")

(defvar jdee-dbs-command-reply nil
  "Contains reply to a debugger command.")

(defvar jdee-dbs-pending-command 0
"Number of the current command.")

(defun jdee-dbs-eval-debugger-output (lisp-form)
  (condition-case error-desc
      (eval (read lisp-form))
    (error
     (let* ((process (jdee-dbs-get-target-process)))
       (if process
	   (jdee-dbs-proc-display-debug-message
	    process
	    (concat
	     "Error: evaluating debugger output caused a Lisp error.\n"
	     "  See *messages* buffer for details.")))
       (message "Error: evaluating output from the debugger caused a Lisp error.")
       (message "Debugger output: %s." lisp-form)
       (message "Lisp error: %s" error-desc)))))

(defun jdee-dbs-extract-exception (debugger-output)
  (let ((lisp-form "")
	(remainder "")
	(output-length (length debugger-output))
	(re "\\(.*Exception:.*[\n]\\)+\\(.*at[^\n]*[\n]\\)+"))
    (if (string-match re debugger-output)
	(let ((start (match-beginning 0))
	      (end (match-end 0)))
	  (setq lisp-form (format "(jdee-dbo-unknown-exception \"%s\")"
				  (substring debugger-output 0 end)))
	  (if (< end output-length)
	      (setq remainder (substring debugger-output end output-length))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun jdee-dbs-extract-lisp-form (debugger-output)
"Extract first complete Lisp form from debugger output.
Returns (FORM . REMAINDER) where FORM is the Lisp form
or the null string and REMAINDER is the remainder of the
debugger output following the Lisp form."
  (let ((lisp-form "")
	(remainder "")
	(level 0)
	in-string-p
	in-escape-p
	(curr-pos 1)
	(output-length (length debugger-output))
	command-end
	lisp-form-end)
    (setq
     lisp-form-end
     (catch 'found-lisp-form
       ;; skip over any inital white space.
       (string-match "^[\n\t ]*(" debugger-output)
       (setq curr-pos (match-end 0))

       (while (< curr-pos output-length)

	 (cond

	  ;; Current character = left slash (escape)
	  ((equal (aref debugger-output curr-pos) ?\\)
	   (if in-string-p
	       (setq in-escape-p (not in-escape-p))))

	  ;; Current character = quotation mark
	  ((equal (aref debugger-output curr-pos) ?\")
	   (if in-string-p
	       (if in-escape-p
		   (progn
		     (setq in-escape-p nil)
		     (setq in-string-p nil))
		 (setq in-string-p nil))
	     (setq in-string-p t)))

	  ;; Current character = right paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\)))
	   (if (= level 0)
	       (throw 'found-lisp-form curr-pos)
	     (setq level (1- level))
	     (if (< level 0)
		 (error "Error parsing debugger output.")))
	   ;; (prin1 (format ") lev = %d pos = %d" level curr-pos) (current-buffer))
	   )

	  ;; Current character = left paren
	  ((and
	    (not in-string-p)
	    (equal (aref debugger-output curr-pos) ?\()
	       (setq level (1+ level)))
	   ;; (prin1 (format "( lev = %d pos = %d" level curr-pos) (current-buffer))
	   )
	  (t
	   (if in-escape-p
	       (setq in-escape-p nil))))

	 (setq curr-pos (1+ curr-pos)))

       -1))
    (if (> lisp-form-end 1)
	(progn
	  (setq lisp-form (substring debugger-output 0 (1+ lisp-form-end)))
	  (when (< lisp-form-end (1- output-length))
	    (setq remainder (substring debugger-output (1+ lisp-form-end) output-length))
	    (if (string-match "(" remainder)
		(setq remainder (substring remainder (string-match "(" remainder)))
	      (setq remainder ""))))
      (setq remainder debugger-output))
    (cons lisp-form remainder)))

(defun jdee-dbs-reply-p (form)
  "Returns t if FORM is a command response form."
  (or
   (string-match "jdee-dbo-command-result" form)
   (string-match "jdee-dbo-command-error" form)))

(defvar jdee-dbs-reply-received nil
"Value to let us know a reply to a command has been received")

(defvar jdee-dbs-pending-event-queue nil
"Queue of events that occurred before receiving a reply to the last command.")

(defun jdee-dbs-command-reply-listener (process output)
  "Listens for a reply to the command specified by
`jdee-dbs-pending-command'."
  ;; (message "entering command reply listener")
  (let* ((combined-output (concat jdee-dbs-debugger-output output))
	 (parsed-output
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (jdee-dbs-extract-lisp-form combined-output)
	    (jdee-dbs-extract-exception combined-output)))
	 (form (car parsed-output))
	 (remainder (cdr parsed-output)))

    ;; (message "form: %s" form)
    ;; (message "remainder: %s" remainder)

    ;; Insert debugger output into the *JDEbug* buffer.
    (funcall (oref jdee-dbs-the-debugger  comint-filter)
	 process output)

    ;; Process the Lisp forms extracted from the debugger output.
    (while (not (string= form ""))

      (if (jdee-dbs-reply-p form)

	  ;; The current form is a reply to a debugger command.
	  (progn
	    (setq jdee-dbs-command-reply form)
	    (setq jdee-dbs-reply-received t))

	;; The form is an event. Postpone processing the event
	;; until we receive a reply to the last command.
	;; (message "   appending %s to pending event queue" form)
	(setq jdee-dbs-pending-event-queue
	      (append jdee-dbs-pending-event-queue (list form))))

      ;; Extract the next Lisp form from the debugger output.
      ;; The car of parsed-output is the next form. The cdr
      ;; is the remaining unprocessed debugger output.
      (setq parsed-output
	    (jdee-dbs-extract-lisp-form remainder))

      (setq form (car parsed-output))
      (setq remainder (cdr parsed-output))) ;; End of form processing loop.

    (setq jdee-dbs-debugger-output remainder)

    (if (not jdee-dbs-reply-received)
	(when (not (accept-process-output process jdee-bug-debugger-command-timeout 0))
	    (message "No response to command %d. (process = %s; timeout = %s sec.)"
		     jdee-dbs-pending-command
		     (if (jdee-dbs-get-target-process)
			 (oref (jdee-dbs-get-target-process) id)
		       "?")
		     jdee-bug-debugger-command-timeout)
		    (setq jdee-dbs-command-reply nil)))))

(defun jdee-dbs-asynch-output-listener (process output)
  "Listens for asynchronous debugger output."
  (let* ((combined-output (concat jdee-dbs-debugger-output output))
	 (parsed-output
	  (if (string-match "^[\n\t ]*(" combined-output)
	      (jdee-dbs-extract-lisp-form combined-output)
	    (jdee-dbs-extract-exception combined-output)))
	 (lisp-form (car parsed-output))
	 (remainder (cdr parsed-output))
	 events)

    ;; (message "asynch form: %s" lisp-form)
    ;; (message "asynch remainder: %s" remainder)

    (funcall (oref  jdee-dbs-the-debugger comint-filter)
	     process output)
    ;; Extract events from debugger output.
    (while (not (string= lisp-form ""))
      ;; (message "   evaluating %s" lisp-form)
      ;; (jdee-dbs-eval-debugger-output lisp-form)
      (setq events (append events (list lisp-form)))
      (setq parsed-output
	    (jdee-dbs-extract-lisp-form remainder))
      (setq lisp-form (car parsed-output))
      (setq remainder (cdr parsed-output)))
    (setq jdee-dbs-debugger-output remainder)
    (if events
	(mapc (lambda (event) (jdee-dbs-eval-debugger-output event))
	      events))))

(defun jdee-dbs-do-command (vm command)
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process
	  (oref jdee-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 cmd)
    (setq jdee-dbs-debugger-output "")
    (setq jdee-dbs-command-reply "")
    (setq jdee-dbs-reply-received nil)
    (setq jdee-dbs-pending-event-queue nil)
    (setq jdee-dbs-cmd-counter (+ jdee-dbs-cmd-counter 1))
    (setq jdee-dbs-pending-command (number-to-string jdee-dbs-cmd-counter))
    (setq cmd (concat (number-to-string vm) " " jdee-dbs-pending-command " " command "\n\n"))
    (jdee-dbs-debugger-display-message jdee-dbs-the-debugger (concat "JDE> " cmd))
    (set-process-filter debugger-process 'jdee-dbs-command-reply-listener)
    (process-send-string debugger-process cmd)
    (when (not (accept-process-output debugger-process jdee-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" cmd)
		(setq jdee-dbs-command-reply nil))
    (set-process-filter debugger-process previous-listener)
    (if jdee-dbs-command-reply
	(let ((result (jdee-dbs-eval-debugger-output jdee-dbs-command-reply)))
	  ;; evaluate any events that occurred between issuance and
	  ;; acknowledgement of this command
	  (mapc (lambda (event) (jdee-dbs-eval-debugger-output event))
		jdee-dbs-pending-event-queue)
	  (setq jdee-dbs-pending-event-queue nil)
	  result))))



(defvar jdee-dbs-debugger-socket-number nil
"Number of socket used to communicate with debugger.")


(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-cmd)))


(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-cmd)))


(defmethod jdee-dbs-cmd-display-response ((this jdee-dbs-cmd))
  (if (slot-boundp this 'msg)
      (jdee-dbs-proc-display-debug-message
       (oref this process)
       (oref this msg))))

(defmethod jdee-dbs-cmd-execute-pending-events ((this jdee-dbs-cmd))
  "Evaluate any events that occurred between issuance and
   acknowledgement of this command"
  (let ((events jdee-dbs-pending-event-queue))
    ;; Empty queue to avoid recursion if commands are executed
    ;; as a result of processing these events.
    (setq jdee-dbs-pending-event-queue nil)
    (mapc (lambda (event) (jdee-dbs-eval-debugger-output event))
		events)))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-cmd))
  "Posts the specified command to the debugger and returns its response."
  (let* ((debugger-process
	  (oref jdee-dbs-the-debugger process))
	 (previous-listener (process-filter debugger-process))
	 (target-process (oref this process))
	 (command-line (format "%s\n" (jdee-dbs-cmd-make-command-line this))))

    (setq jdee-dbs-debugger-output "")
    (setq jdee-dbs-command-reply "")
    (setq jdee-dbs-reply-received nil)
    (setq jdee-dbs-pending-event-queue nil)
    (setq jdee-dbs-pending-command (oref this id))

    (if target-process (oset target-process last-cmd this))
    (jdee-dbs-debugger-display-message jdee-dbs-the-debugger (concat "JDE> " command-line))
    (set-process-filter debugger-process 'jdee-dbs-command-reply-listener)
    (process-send-string debugger-process command-line)
    (process-send-string debugger-process "\n")

    (when (not (accept-process-output debugger-process jdee-bug-debugger-command-timeout 0))
		(message "Error: debugger didn't respond to command:\n%s" command-line)
		(setq jdee-dbs-command-reply nil))

    (process-send-string debugger-process "\n")

    (set-process-filter debugger-process previous-listener)

    (if jdee-dbs-command-reply
	(let ((result (jdee-dbs-eval-debugger-output jdee-dbs-command-reply)))

	  (oset this :result result)

	  (oset this :data (car (jdee-dbo-command-result-data (oref this result))))

	  (if (jdee-dbo-command-succeeded-p result)
	      (jdee-dbs-cmd-success-action this)
	    (jdee-dbs-cmd-failure-action this))

	  (jdee-dbs-cmd-display-response this)

	  (jdee-dbs-cmd-execute-pending-events this)
	  (oref this :result)))))

(defvar jdee-dbs-cmd-counter 0
 "Count of the number of commands issued in this session.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-launch-process (jdee-dbs-cmd)
  ((main-class  :initarg :main-class
		:type string
		:documentation
		"Class containing this process's main method.")
   (jre-home    :initarg :jre-home
		:type string
		:documentation
		"Home directory of JRE used to launch this process.")
   (vmexec     :initarg :vmexec
		:type string
		:initform "java"
		:documentation
		"Name of vm executable used to run process.")
   (vm-args     :initarg :args
		:type string
		:initform ""
		:documentation
		"Command line arguments to be passed to vm's main method.")
   (app-args    :initarg :app-args
		:type string
		:initform ""
		:documentation
		"Command line arguments to be passed to app's main method."))
  "Command to launch a debuggee process.")

(defun jdee-dbs-get-app-buffer-name ()
  (concat "*" (jdee-run-get-main-class) "*"))

(defmethod initialize-instance ((this jdee-dbs-launch-process) &rest fields)
  "Constructor for debugger commands. Generates a unique id for this command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "launch")

  ;; You must specify a process to launch when constructing a launch command."
  (assert (slot-boundp this :process))

  ;; Set main class.
  (if (not (slot-boundp this :main-class))
    (oset this :main-class
	  (oref (oref this :process) :main-class)))

  ;; Set vm.
  ;; (oset this vm (jdee-dbs-choose-vm))

  ;; Set vm args
  (oset this vm-args
	(concat (mapconcat (lambda (s) s) (jdee-db-get-vm-args jdee-dbs-the-debugger) " ")
		" "
		(mapconcat (lambda (s) s) (jdee-db-get-vm-args-from-user) " ")))


  ;; Set application arguments.
  (oset this app-args
	(concat
	 (if jdee-db-option-application-args
	     (mapconcat (lambda (s) s) jdee-db-option-application-args " ")
	   "")
	 " "
	 (mapconcat (lambda (s) s) (jdee-db-get-app-args-from-user) " "))))



(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-launch-process))
  "Creates the command line for the launch command."
  (let ((cmd (format "-1 %s %s %s -vmexec %s"
		     (oref this id)                    ;; cid
		     (oref this name)                  ;; launch
		     (oref (oref this process) id)     ;; pid
		     (oref this vmexec))))

    (if (slot-boundp this 'jre-home)
	(setq cmd (concat cmd " -home " (oref this jre-home))))

    (setq cmd
	  (format "%s %s %s %s"
		  cmd
		  (oref this vm-args)            ;; vm args
		  (oref this main-class)         ;; main class
		  (oref this app-args)))         ;; command line args

    (oset this msg
	  (format "Launch command line:\n  %s %s %s %s\n"
		  (oref this vmexec)
		  (oref this vm-args)            ;; vm args
		  (oref this main-class)         ;; main class
		  (oref this app-args)))         ;; command line args
    cmd))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-launch-process))
  (call-next-method)
  (delete-other-windows)
  (let* ((process (oref this process))
	 (main-class (oref this main-class))
	 (source-buffer (current-buffer))
	 (cli-socket
	  (car (jdee-dbo-command-result-data (oref this result))))
	 (cli-buffer-name
	  (format "%s(%d) CLI" main-class (oref process id))))

    (oset process cli-socket cli-socket)

    ;; Connect to socket used by debugger to transport the
    ;; standard I/O of the debuggee process.
    (sleep-for jdee-bug-sio-connect-delay)
    (oset
     process
     cli-buf
     (make-comint
      cli-buffer-name
      (cons jdee-bug-debugger-host-address cli-socket)))

    (oset this msg
	  (format "%s\nEmacs connected to standard IO port %d for process %s."
		  (oref this msg)
		  cli-socket
		  (oref this main-class)))

    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-launch-process))
  (delete-other-windows)
  (let* ((process (oref this process))
	 (source-buffer (current-buffer)))
    (oset this  msg
	  (format "%s\nError: debugger unable to launch %s.\n  Reason: %s"
		  (oref this msg)
		  (oref this main-class)
		  (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Shared Memory                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-attach-shmem (jdee-dbs-cmd)
  ((process-name  :initarg :process-name
		  :type string
		  :documentation
		  "Name of process to attach."))
  "Attach debugger to a running process via shared memory.")

(defmethod initialize-instance ((this jdee-dbs-attach-shmem) &rest fields)
  "Constructor for attach_shmem command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'process-name))

  ;; Set command name.
  (oset this name "attach_shmem"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-attach-shmem))
  "Creates the command line for the attach_shmem command."
  (format "-1 %s %s %s %s"
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this process-name)))       ;; process name

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-attach-shmem))
  (call-next-method)
  (delete-other-windows)
  (let* ((process (oref this process))
	 (source-buffer (current-buffer)))
    (oset process :attachedp t)
    (oset process :startupp t)
    (oset this msg  (format "Attached to process %s."
			    (oref this process-name)))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-attach-shmem))
  (delete-other-windows)
  (let* ((process (oref this process))
	 (source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach process %s.\n Reason: %s."
		    (oref this process-name)
		    (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process via Socket                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-attach-socket (jdee-dbs-cmd)
  ((port  :initarg :port
	  :type string
	  :documentation
	  "Name of port on which existing process is listening.")
   (host  :initarg :host
	  :type string
	  :documentation
	  "Name of host on which existing process is listening."))
  "Attach debugger to a running process via a socket connection.")

(defmethod initialize-instance ((this jdee-dbs-attach-socket) &rest fields)
  "Constructor for attach_socket command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'port))

  ;; Set command name.
  (oset this name "attach_socket"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-attach-socket))
  "Creates the command line for the attach_socket command."
  (let ((cmd
	 (format "-1 %s %s %s -port %s"
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this port))))              ;; process name
    (if (slot-boundp this 'host)
	(setq cmd (format "%s -host %s" cmd (oref this host))))
    cmd))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-attach-socket))
  (call-next-method)
  (delete-other-windows)
  (let* ((process (oref this process))
	(source-buffer (current-buffer)))
    (oset process attachedp t)
    (oset process startupp t)
    (oset this msg  (format "Attached to process on port %s of %s."
			    (oref this port)
			    (if (slot-boundp this 'host)
				(oref this host)
			      "local host")))
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-attach-socket))
  (delete-other-windows)
  (let* ((process (oref this process))
	 (source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot attach to process on port %s of %s.\n Reason: %s."
	     (oref this port)
	     (if (slot-boundp this 'host)
		 (oref this host)
	       "local host")
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-listen-for-process (jdee-dbs-cmd)
  ((address   :initarg :address
	      :type string
	      :documentation
	      "Address at which to listen for a debuggee process.")
   (transport :initarg :transport
	      :type string
	      :initform "shmem"
	      :documentation
	      "Transport mechanism used to interact with debuggee process."))
  "Listen for a process requesting debugger services.")

(defmethod initialize-instance ((this jdee-dbs-listen-for-process) &rest fields)
  "Constructor for listen command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  (assert (slot-boundp this 'address))

  (assert (not
	   (and
	    (not (eq system-type 'windows-nt))
	    (string= (oref this transport) "shmem"))))

  ;; Set command name.
  (oset this name
	(concat "listen_"
		(oref this transport))))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-listen-for-process))
  "Creates the command line for the listen command."
  (format "-1 %s %s %s %s"
	  (oref this id)
	  (oref this name)                 ;; command name
	  (oref (oref this process) id)    ;; process id
	  (oref this address)))            ;; process address

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-listen-for-process))
  (call-next-method)
  (delete-other-windows)
  (let* ((process (oref this process))
	(source-buffer (current-buffer)))
    (oset this msg  (format "Listening for process at %s address: %s."
			    (if (string= (oref this transport) "shmem")
				"shared memory" "socket")
			    (oref this address)))
    (oset process startupp t)
    (split-window-vertically)
    (pop-to-buffer (oref process msg-buf))
    (pop-to-buffer source-buffer)
    (oset process win-cfg (current-window-configuration))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-listen-for-process))
  (delete-other-windows)
  (let* ((process (oref this process))
	 (source-buffer (current-buffer)))
    (oset this  msg
     (format "Error: cannot listen for process at %s address: %s.\n Reason: %s."
	     (if (string= (oref this transport) "shmem")
		 "shared memory" "socket")
	     (oref this address)
	     (oref this data)))
      (split-window-vertically)
      (pop-to-buffer (oref process msg-buf))
      (pop-to-buffer source-buffer)
      (oset process win-cfg (current-window-configuration))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Run Process Command Class                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-run-process (jdee-dbs-cmd) ()
  "Run process command.")

(defmethod initialize-instance ((this jdee-dbs-run-process) &rest fields)
  "Constructor for run process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "run"))


(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-run-process))
  (call-next-method)
  (oset this msg (format "Running %s."
			 (oref (oref this process)  main-class))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-run-process))
  (oset this msg
	(format "Error: unable to run %s..\n  Reason: %s."
		(oref (oref this process) main-class)
		(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Finish Process Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-finish-process (jdee-dbs-cmd) ()
  "Finish process command.")

(defmethod initialize-instance ((this jdee-dbs-finish-process) &rest fields)
  "Constructor for finish process command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))

  ;; Set command name.
  (oset this name "finish"))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-finish-process))
  "Executes the finish process command."
  (let* ((process (oref this :process))
	 (main-class (oref process :main-class))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(progn
	  (jdee-dbs-proc-display-debug-message process
	   (concat "Terminating " main-class)))
      (jdee-dbs-proc-display-debug-message process
       (concat "Error: debugger unable to terminate: "
	       main-class
	       ".\n  Reason: "
	       (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Set Breakpoint Command Class                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-set-breakpoint (jdee-dbs-cmd)
  ((breakpoint    :initarg :breakpoint
		  :type jdee-db-breakpoint
		  :documentation
		  "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this jdee-dbs-set-breakpoint) &rest fields)
  "Constructor for set breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "break absolute"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-set-breakpoint))
  "Creates the command line for the set breakpoint command."
  (let* ((bp-spec (oref this breakpoint))
	 (file (file-name-nondirectory (oref bp-spec file)))
	 (line (jdee-db-breakpoint-get-line bp-spec)))
    (format "%s %s %s"
	    (call-next-method)
	    file     ;; File
	    line)))  ;; Line number

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-set-breakpoint))
  (call-next-method)
  (let*  ((process (oref this process))
	  (bp-procid (oref this data))
	  (bp-spec (oref this breakpoint))
	  (file (oref bp-spec file))
	  (line (jdee-db-breakpoint-get-line bp-spec))
	  (bpspec (jdee-dbs-proc-bpspec "spec" :id bp-procid :breakpoint bp-spec))
	  (bpspecs (if (slot-boundp process :bpspecs) (oref process :bpspecs))))
    (if bpspecs
	(oset process bpspecs (jdee-dbs-proc-bpspecs-add bpspecs bpspec))
      (oset process bpspecs (jdee-dbs-proc-bpspecs-add nil bpspec)))
    (oset this msg (format "Setting breakpoint at line %s in %s." line file))))


(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-set-breakpoint))
  (let* ((bp-spec (oref this breakpoint))
	 (file (oref bp-spec file))
	 (line (jdee-db-breakpoint-get-line bp-spec)))
    (oset this msg  (format "Error: cannot set breakpoint at line %s in file %s.\n  Reason: %s"
			    file line (oref this data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command Class                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-clear-breakpoint (jdee-dbs-cmd)
  ((breakpoint    :initarg :breakpoint
		  :type jdee-db-breakpoint
		  :documentation
		  "Breakpoint specification."))
  "Set breakpoint command.")

(defmethod initialize-instance ((this jdee-dbs-clear-breakpoint) &rest fields)
  "Constructor for clear breakpoint command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this breakpoint))

  ;; Set command name.
  (oset this name "clear"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-clear-breakpoint))
  "Creates the command line for the clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))
	 (bpspec (jdee-dbs-proc-get-bpspec process breakpoint))
	 (bp-procid (oref bpspec id)))
    (format "%s %s"              ;; PID CID clear BPID
	    (call-next-method)
	    bp-procid)))         ;; Id assigned by debugger to this breakpoint


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-clear-breakpoint))
  "Execute clear breakpoint command."
  (let* ((process (oref this process))
	 (breakpoint (oref this breakpoint))
	 (file (oref breakpoint file))
	 (line (jdee-db-breakpoint-get-line breakpoint))
	 (proc-id (oref process id))
	 (bpspec (jdee-dbs-proc-get-bpspec process breakpoint)))
    (if bpspec
	(let ((bp-procid (oref bpspec id))
	      (result (call-next-method)))
	  (if (jdee-dbo-command-succeeded-p result)
	      (let ((bpspecs (oref process bpspecs)))
		(oset process bpspecs
		      (jdee-dbs-proc-bpspecs-remove bpspecs bpspec))
		(jdee-dbs-proc-display-debug-message
		 process
		 (format "Cleared breakpoint at line %s in file %s" line file)))
	    (jdee-dbs-proc-display-debug-message
	     process
	     (format "Error: cannot clear breakpoint at line %s in file %s.\n Reason: %s."
		     line file (car (jdee-dbo-command-result-data result))))
	    nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Over/Into/Out Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-dbs-step (jdee-dbs-cmd)
  ((step-type :initarg :step-type
	      :type string
	      :initform "over"
	      :documentation
	      "Type of step operation: over, into, into-all, out"))
  "Step command.")

(defmethod initialize-instance ((this jdee-dbs-step) &rest fields)
  "Constructor for step command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))


  ;; Set command name.
  (oset this name (concat "step " (oref this step-type))))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-step))
  "Creates the command line for the step command."
  (format "%s %d" (call-next-method)
	  (oref (oref (oref this process) state-info) thread-id)))


(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-step))
  (oset this msg
	(format "Error: unable to step %s.\n Reason: %s"
		(oref this step-type) (oref this data))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Into Command Class                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod jdee-dbs-proc-step-into ((this jdee-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id
	  (oref (oref this state-info) thread-id))
	 (result (jdee-dbs-do-command proc-id  (format "step into %s" thread-id))))
    (when (not (jdee-dbo-command-succeeded-p result))
      (jdee-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (jdee-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Step Out Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod jdee-dbs-proc-step-out ((this jdee-dbs-proc))
  (let* ((proc-id (oref this id))
	 (thread-id
	  (oref (oref this state-info) thread-id))
	 (result (jdee-dbs-do-command proc-id  (format "step out %s" thread-id))))
    (when (not (jdee-dbo-command-succeeded-p result))
      (jdee-dbs-proc-display-debug-message this
       (format "Error: unable to step into... .\n  Reason: %s"
	       (car (jdee-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Command Class                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-evaluate (jdee-dbs-cmd)
  ((expression    :initarg :expression
		  ;; :type string
		  :documentation
		  "Expression to be evaluate. Required.")
   (thread-id     :initarg :thread-id
		  ;; :type integer
		  :documentation
		  "Id of thread that scopes this expression. Required."))
  "Evaluate expression command.")

(defmethod initialize-instance ((this jdee-dbs-evaluate) &rest fields)
  "Constructor for evaluate command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))
  (assert (oref this expression))
  (assert (oref this thread-id))

  ;; Set command name.
  (oset this name "evaluate"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-evaluate))
  "Creates the command line for the clear breakpoint command."
    (format "%s %s 0 \"%s\""         ;; PID CID evaluate THREAD-ID 0 "EXPRESSION"
	    (call-next-method)       ;; PID CID evaluate
	    (oref this thread-id)    ;; thread id
	    (oref this expression))) ;; expression to be evaluated.


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-evaluate))
  "Execute evaluate expression command. Returns
(TYPE VALUE GCFLAG) where TYPE is the type of the result,
VALUE is the value, and GCFLAG is t if the result has been
garbage collected."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(car (jdee-dbo-command-result-data result))
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot evaluate \"%s\".\n Reason: %s."
	       (oref this expression)
	       (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Array                                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-array (jdee-dbs-cmd)
  ((array    :initarg :array
	     :type jdee-dbs-java-array
	     :documentation
	     "Object to represent the array. Required.")
   (index    :initarg :index
	     :type integer
	     :documentation
	     "Index of array slice to be returned.")
   (length   :initarg :length
	     :type integer
	     :documentation "Length of slice to be returned."))
  "Get a slice of the array object specified by ARRAY. INDEX and LENGTH are
the index and length of the slice to be returned. If omitted, this command returns
the length of the first slice of the array. Note that each element of this array
can be another array or some other object.")


(defmethod initialize-instance ((this jdee-dbs-get-array) &rest fields)
  "Constructor for get array command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :array))

  (if (slot-boundp this :index)
      (assert (slot-boundp this :length)))

  ;; Set command name.
  (oset this name "get_array"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-get-array))
  "Creates the command line for the get-object command."
  (let ((cl
	 (format "%s %d" (call-next-method) (oref (oref this array) id)))
	(index (if (slot-boundp this :index) (oref this :index))))
    (if index
	(setq cl
	      (format "%s %d %d"                ;; PID CID get_array OBJ-ID INDEX LENGTH
		      cl
		      index                     ;; index of slice to be returned.
		      (oref this length))))    ;; length of slice to be returned.
    cl))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-array))
  "Executes the get-array command. If a slice is specified,
returns the slice as a list of elements. Otherwise, return
the length of the array."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((array (oref this array))
	       (data   (nth 0 (jdee-dbo-command-result-data result)))
	       (type (nth 0 data))
	       (id (nth 1 data))
	       (gc-flag (nth 2 data))
	       (length (nth 3 data))
	       (elements (if (> (length data) 4)
			     (cdr (cdr (cdr (cdr data)))))))
	  (or elements length)
	  (oset array jtype type)
	  (oset array id id)
	  (oset array gc-flag gc-flag)
	  (oset array length length)
	  (oset array elements
		(mapcar
		 (lambda (element)
		   (jdee-dbs-objectify-value element))
		 elements))
	  array)
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot get array %d.\n Reason: %s."
	       (oref this object-id)
	       (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Abstract Get Object                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-abstract-get-object (jdee-dbs-cmd)
  ((object-id     :initarg :object-id
		  :type integer
		  :documentation
		  "Id of object. Required."))
  "Parent class of get object commands.")


(defmethod initialize-instance ((this jdee-dbs-abstract-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :process))
  (assert (slot-boundp this :object-id)))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-abstract-get-object))
  "Creates the command line for the get-object command."

  (format "%s %d" (call-next-method) (oref this object-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-object (jdee-dbs-abstract-get-object) ()
  "Class of generic get-object commands. These commands return the fields of
the object.")


(defmethod initialize-instance ((this jdee-dbs-get-object) &rest fields)
  "Constructor for get-object command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_object"))

(defun jdee-dbs-objectify-value (value-form)
  (let ((lvf        (length value-form))
	(value-type (car value-form)))
    (cond
     ((and (= lvf 1) (string= value-type "null"))
      (jdee-dbs-java-null "null"))
     ((= lvf 2)
      (jdee-dbs-java-primitive
       "primitive"
       :jtype  value-type
       :value  (nth 1 value-form)))
     ((= lvf 3)
      (if (string-match "\\[\\]" value-type)
	  (jdee-dbs-java-array
	   (format "array %d" (nth 1 value-form))
	   :jtype value-type
	   :id (nth 1 value-form)
	   :gc-flag (nth 2 value-form))
	(jdee-dbs-java-udci
	 (format "obj %d" (nth 1 value-form))
	 :jtype    value-type
	 :id       (nth 1 value-form)
	 :gc-flag  (nth 2 value-form)))))))

(defun jdee-dbs-objectify-variable (variable-form)
  (let* ((var-name   (car (car variable-form)))
	 (var-type   (cdr (car variable-form)))
	 (value-form (cdr variable-form))
	 (value      (jdee-dbs-objectify-value
		      value-form)))
    (jdee-dbs-java-variable
     (format "variable %s" var-name)
     :name var-name
     :jtype (mapconcat (lambda (x) x) (nreverse var-type) " ")
     :value value)))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-object))
  "Executes the get-object command. Returns a Lisp object of type
`jdee-dbs-java-class-instance' that represents the Java object."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((obj     (car (jdee-dbo-command-result-data result)))
	       (type    (nth 0 obj))
	       (id      (nth 1 obj))
	       (gc-flag (nth 2 obj))
	       (fields  (if (> (length obj) 3)
			    (nth 3 obj)))
	       (object  (jdee-dbs-java-udci
			 (format "obj %d" id)
			 :jtype type
			 :id id
			 :gc-flag gc-flag)))
	  (if fields
	      (mapc
	       (lambda (variable-form)
		 (let ((field
			(jdee-dbs-objectify-variable variable-form)))
		   (jdee-dbs-java-udci-add-field object field)))
	       fields))
	  object)
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot get object %d.\n Reason: %s."
	       (oref this object-id)
	       (car (jdee-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get String                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-string (jdee-dbs-abstract-get-object) ()
  "Get the value of a string object.")


(defmethod initialize-instance ((this jdee-dbs-get-string) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_string"))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-string))
  "Executes the get_string command. Returns the string."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(nth 3 (car (jdee-dbo-command-result-data result)))
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot get string %d.\n Reason: %s."
	       (oref this object-id)
	       (car (jdee-dbo-command-result-data result))))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Locals                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-locals (jdee-dbs-cmd)
  ((thread-id         :initarg :thread-id
		      :type integer
		      :documentation
		      "ID of thread whose local variables are being queried.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame containing requested local variables."))
  "Get variables local to a specified thread and stack frame.")


(defmethod initialize-instance ((this jdee-dbs-get-locals) &rest fields)
  "Constructor for get-string command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_locals"))


(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-get-locals))
  "Creates the command line for the get-locals command."
  (format "%s %d %d"
	  (call-next-method)
	  (oref this thread-id)
	  (oref this stack-frame-index)))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-locals))
  "Executes the get-locals command. Returns a list of Lisp objects of type
`jdee-dbs-java-variable' that represents the local variables."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((variable-forms (car (jdee-dbo-command-result-data result)))
	       (variables      (if variable-forms
				   (mapcar
				    (lambda (variable-form)
					(jdee-dbs-objectify-variable variable-form))
				    variable-forms))))
	  variables)
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot get local variables.\n Reason: %s."
	       (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get This                                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-this (jdee-dbs-cmd)
  ((thread-id         :initarg :thread-id
		      :type integer
		      :documentation
		      "ID of thread of stack frame whose this object is required.")
   (stack-frame-index :initarg :stack-frame-index
		      :type integer
		      :initform 0
		      :documentation
		      "Index of stack frame whose this object is required."))
  "Get this object of a specified stack frame.")


(defmethod initialize-instance ((this jdee-dbs-get-this) &rest fields)
  "Constructor for get_this command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))
  (assert (slot-boundp this 'stack-frame-index))

  ;; Set command name.
  (oset this name "get_this"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-get-this))
  "Creates the command line for the get_this command."
  (format "%s %d %d"
	  (call-next-method)
	  (oref this thread-id)
	  (oref this stack-frame-index)))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-get-this))
  (call-next-method)
  (let ((this-obj (oref this :data)))
    (oset
     this
     :result
     (if (string= (nth 0 this-obj) "null")
	 (jdee-dbs-java-null "null")
       (jdee-dbs-java-udci
	  "this object"
	  :jtype (nth 0 this-obj)
	  :id (nth 1 this-obj))))))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-get-this))
 (oset
  this
  msg
  (format
   "Error: unable to get this object for stack frame %s on thread %d.\n Reason: %s."
   (oref this stack-frame-index)
   (oref this thread-id)
   (oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Loaded Classes Command Class                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-loaded-classes (jdee-dbs-cmd) ()
  "Gets the classes loaded by a specified process.")

(defmethod initialize-instance ((this jdee-dbs-get-loaded-classes) &rest fields)
  "Constructor for get_loaded_classes command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_loaded_classes"))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-loaded-classes))
  "Executes the get_loaded_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let ((classes (car (jdee-dbo-command-result-data result))))
	  (jdee-dbs-proc-display-debug-message
	   process
	   (format "Loaded classes:\n  %s."
		   (mapconcat (lambda (x) x) classes "\n  ")) t)
	  t)
      (jdee-dbs-proc-display-debug-message process
	     (format "Error: unable to list loaded classes.\n  Reason: %s."
		     (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Path Info Command Class                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-path-info (jdee-dbs-cmd) ()
  "Gets the base directory, boot classpath, and classpath of the specified process.")

(defmethod initialize-instance ((this jdee-dbs-get-path-info) &rest fields)
  "Constructor for get_path_information command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (oref this process))

  ;; Set command name.
  (oset this name "get_path_information"))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-path-info))
  "Executes the get_path_info command."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((data (jdee-dbo-command-result-data result))
	       (base-dir (nth 0 data))
	       (boot-classpath (nth 1 data))
	       (classpath (nth 2 data)))
	  (jdee-dbs-proc-display-debug-message
	   process
	   (format (concat
		    "\nPath information\n\n  Base directory:\n    %s\n\n  "
		    "Boot classpath:\n    %s\n\n  Application Classpath:\n    %s\n")
		   base-dir
		   (mapconcat (lambda (x) x) boot-classpath "\n    ")
		   (mapconcat (lambda (x) x) classpath "\n    ")))
	  t)
      (jdee-dbs-proc-display-debug-message process
	     (format "Error: unable to display path information.\n  Reason: %s."
		     (car (jdee-dbo-command-result-data result))))
      nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Threads                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-threads (jdee-dbs-cmd) ()
  "Get all the threads for this process.")


(defmethod initialize-instance ((this jdee-dbs-get-threads) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "get_threads"))

(defun jdee-dbs-map-thread-to-tree (thread)
  (list (quote tree-widget) :tag (concat (nth 2 thread) " thread")
	:value nil
	(list (quote tree-widget) :tag (concat "id: " (number-to-string (nth 1 thread))))
	(list (quote tree-widget) :tag (concat "status: " (nth 3 thread)))
	(list (quote tree-widget) :tag (concat "state: " (nth 4 thread)))
	(jdee-dbs-map-stack-to-tree (nth 5 thread))))


(defun jdee-dbs-map-threadgroup-to-tree (threadgroup)
  (nconc
   (list (quote tree-widget) :tag (concat (nth 2 threadgroup) " thread group")
	:value nil)
   (mapcar
    (lambda (x)
      (jdee-dbs-map-thread-to-tree x))
    (nth 3 threadgroup))
   (mapcar
    (lambda (x)
      (jdee-dbs-map-threadgroup-to-tree x))
    (nth 4 threadgroup))))

(defun jdee-dbs-map-stack-to-tree (stack)
  (nconc
   (list (quote tree-widget) :tag "Stack")
   (if (listp stack)
       (mapcar
	(lambda (x)
	  (list (quote tree-widget) :tag
		(format "%s.%s(%s:%s)" (nth 1 x) (nth 4 x) (nth 2 x)
			(nth 3 x))))
	stack))))

(defun jdee-dbs-map-threads-to-tree (threads)
  (nconc
   (list (quote tree-widget) :tag "Threads")
	(mapcar
	 (lambda (x)
	   (if (string= (nth 0 x) "Thread")
	       (jdee-dbs-map-thread-to-tree x)
	     (if (string= (nth 0 x) "ThreadGroup")
		 (jdee-dbs-map-threadgroup-to-tree x))))
	 threads)))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-threads))
  "Executes the get-threads command. Returns a list of thread information."
  (let* ((process (oref this process))
	 (result (call-next-method)))
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((thread-list (car (jdee-dbo-command-result-data result)))
	       (buf (oref process threads-buf)))
	  (set-window-configuration (oref process win-cfg))
	  (set-window-buffer
	   (next-window
	    (frame-first-window))
	   buf)
	  (set-buffer buf)
	  (kill-all-local-variables)
	  (let ((inhibit-read-only t))
	    (erase-buffer))
	  (let ((all (overlay-lists)))
            (mapc 'delete-overlay (car all))
            (mapc 'delete-overlay (cdr all)))
	  (apply 'widget-create (jdee-dbs-map-threads-to-tree thread-list))
	  (use-local-map widget-keymap)
	  (widget-setup))
      (jdee-dbs-proc-display-debug-message
       process
       (format "Error: cannot get local variables.\n Reason: %s."
	       (car (jdee-dbo-command-result-data result))))
      nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-thread (jdee-dbs-cmd)
  ((thread-id     :initarg :thread-id
		  :type integer
		  :documentation
		  "Id of thread to be queried."))
  "Gets information about a thread, including the method call stack.")


(defmethod initialize-instance ((this jdee-dbs-get-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this 'process))
  (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "get_thread"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-get-thread))
  "Creates the command line for the get_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-get-thread))
  (call-next-method)
  (oset this :result (oref this :data)))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-get-thread))
 (oset this msg (format "Error: unable to get info for thread %d.\n Reason: %s."
			(oref this thread-id)
			(oref this result))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Get Object Monitors                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-get-object-monitors (jdee-dbs-cmd)
  ((object-id     :initarg :object-id
		  :type integer
		  :documentation
		  "Id of object. Required."))
  "Get threads that are monitoring the specified object.")


(defmethod initialize-instance ((this jdee-dbs-get-object-monitors) &rest fields)
  "Constructor for get_object_monitors command."

  ;; Call parent initializer.
  (call-next-method)

  (assert (slot-boundp this :object-id))

  ;; Set command name.
  (oset this name "get_object_monitors"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-get-object-monitors))
  "Creates the command line for the get_object_monitors command."

  (format "%s %d" (call-next-method) (oref this object-id)))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-get-object-monitors))
  "Executes the get_object_monitors command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 msg)
    (if (jdee-dbo-command-succeeded-p result)
	(let* ((data (car (jdee-dbo-command-result-data result)))
	       (obj-id (nth 0 data))
	       (obj-type (nth 1 data))
	       (obj-gc (nth 2 data))
	       (owner (nth 3 data))
	       (waiting (nth 4 data)))

	  (setq msg (format "\nThe following threads are monitoring <%s:%s>:\n"
			    obj-type obj-id))

	  (setq
	   msg
	   (concat
	    msg
	    "  Current owner:"
	    (if (listp owner)
		(concat
		 "\n"
		 "    Name:   " (nth 1 owner) "\n"
		 "    Id:     " (nth 2 owner) "\n"
		 "    Status: " (nth 3 owner) "\n"
		 "    State:  " (nth 4 owner) "\n")
	      (if (stringp owner)
		  (concat " " owner)))))

	  (if waiting
	      (setq
	       msg
	       (concat
		msg
		"\n  Waiting threads:"
		(if (listp waiting)
		    (progn
		      "\n"
		      (mapconcat
		      (lambda (thread)
			(concat
			 "    Name:   " (nth 1 thread) "\n"
			 "    Id:     " (nth 2 thread) "\n"
			 "    Status: " (nth 3 thread) "\n"
			 "    State:  " (nth 4 thread) "\n"))
		      waiting "\n"))
		  (if (stringp waiting) (concat " " waiting "\n")))))))
      (setq msg
	    (format "Error: cannot get object monitors for  %d.\n Reason: %s."
		    (oref this object-id)
		    (car (jdee-dbo-command-result-data result)))))
    (jdee-dbs-proc-display-debug-message process msg)
    nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-suspend-thread (jdee-dbs-cmd)
  ((thread-id     :initarg :thread-id
		  :type integer
		  :documentation
		  "Id of thread or thread-group to be suspended. If omitted, all threads are suspended."))
  "Suspend a thread of this process.")


(defmethod initialize-instance ((this jdee-dbs-suspend-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "suspend"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-suspend-thread))
  "Creates the command line for the suspend_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-suspend-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d suspended." (oref this thread-id)))
    (oset this msg "All threads suspended.")
    (oset (oref this process) suspendedp t)))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-suspend-thread))
 (oset this msg (format "Error: unable to suspend thread.\n Reason: %s."
	       (oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Resume Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-resume-thread (jdee-dbs-cmd)
  ((thread-id     :initarg :thread-id
		  :type integer
		  :documentation
		  "Id of thread or thread-group to be resumed. If omitted, all threads are resumed."))
  "Resume a thread of this process.")


(defmethod initialize-instance ((this jdee-dbs-resume-thread) &rest fields)
  "Constructor for resume-thread command."

  ;; Call parent initializer.
  (call-next-method)

  ;; Set command name.
  (oset this name "resume"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-resume-thread))
  "Creates the command line for the resume_thread command."
    (if (slot-boundp this 'thread-id)
	(format "%s %d" (call-next-method) (oref this thread-id))
      (call-next-method)))

(defmethod jdee-dbs-cmd-success-action ((this jdee-dbs-resume-thread))
  (call-next-method)
  (if (slot-boundp this 'thread-id)
	(oset this msg (format "Thread %d resumed." (oref this thread-id)))
    (oset this msg "All threads resumed.")
    (oset (oref this process) suspendedp nil)))

(defmethod jdee-dbs-cmd-failure-action ((this jdee-dbs-resume-thread))
  (oset this msg
	(format "Error: unable to resume thread.\n Reason: %s."
		(oref this result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Stop Thread                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-stop-thread (jdee-dbs-cmd)
  ((thread-id     :initarg :thread-id
		  :type integer
		  :documentation
		  "Id of thread to be stopped.")
   (exception-id  :initarg :exception-id
		  :type integer
		  :documentation
		  "Id of thread to be stopped."))
  "Stops the specified thread in the target process and throw the specified
exception. You can use the evaluate expression command to create the exception
object.")


(defmethod initialize-instance ((this jdee-dbs-stop-thread) &rest fields)
  "Constructor for stop-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))
 (assert (slot-boundp this 'exception-id))

  ;; Set command name.
  (oset this name "stop"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-stop-thread))
  "Creates the command line for the resume_thread command."

  (format "%s %d %d" (call-next-method) (oref this thread-id)
	  (oref this exception-id)))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-stop-thread))
  "Executes the stop_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result)))
    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d stopped." (oref this thread-id))
	   (format "Error: unable to stop thread %d.\n Reason: %s."
		   (oref this thread-id)
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-interrupt-thread (jdee-dbs-cmd)
  ((thread-id     :initarg :thread-id
		  :type integer
		  :documentation
		  "Id of thread to be interrupted."))
  "Interrupt a thread of this process. An interrupted thread cannot be resumed.")


(defmethod initialize-instance ((this jdee-dbs-interrupt-thread) &rest fields)
  "Constructor for suspend-thread command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'thread-id))

  ;; Set command name.
  (oset this name "interrupt"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-interrupt-thread))
  "Creates the command line for the interrupt_thread command."
  (format "%s %d" (call-next-method) (oref this thread-id)))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-interrupt-thread))
  "Executes the interrupt_thread command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result)))
    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Thread %d interrupted." (oref this thread-id))
	   (format "Error: unable to interrupt thread %d.\n Reason: %s."
		   (oref this thread-id)
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-methods (jdee-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jdee-dbs-trace-methods-request
		   :documentation
		   "Trace method request."))
  "Trace method entries or exits.")


(defmethod initialize-instance ((this jdee-dbs-trace-methods) &rest fields)
  "Constructor for trace_methods command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "entry")
	  (string= (oref (oref this trace-request) trace-type) "exit")))

  ;; Set command name.
  (oset this name "trace_methods"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-trace-methods))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'thread-restriction)
	(setq cmd (format "%s -tname %s" cmd (oref request thread-restriction))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd
	      (format
	       "%s -cf \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd
	      (format
	       "%s -cef \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-trace-methods))
  "Executes the trace_methods command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jdee-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset
	   process
	   trace-req
	   (nconc (oref process trace-req)
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace method %s enabled. Use request id %s to cancel."
		     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s."
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-classes (jdee-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jdee-dbs-trace-classes-request
		   :documentation
		   "Trace classes request."))
  "Trace class preparations or unloadings.")


(defmethod initialize-instance ((this jdee-dbs-trace-classes) &rest fields)
  "Constructor for trace_classes command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "preparation")
	  (string= (oref (oref this trace-request) trace-type) "unloading")))

  ;; Set command name.
  (oset this name "trace_classes"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-trace-classes))
  "Creates the command line for the trace_methods command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s" (call-next-method) (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd
	      (format
	       "%s -cf \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd
	      (format
	       "%s -cef \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-trace-classes))
  "Executes the trace_classes command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jdee-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset
	   process
	   trace-req
	   (nconc (oref process trace-req)
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace class %s enabled. Use request id %s to cancel."
		     (oref request trace-type) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s."
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-trace-exceptions (jdee-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jdee-dbs-trace-exceptions-request
		   :documentation
		   "Trace exceptions request."))
  "Trace exceptions.")


(defmethod initialize-instance ((this jdee-dbs-trace-exceptions) &rest fields)
  "Constructor for trace_exceptions command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (or
	  (string= (oref (oref this trace-request) trace-type) "both")
	  (string= (oref (oref this trace-request) trace-type) "caught")
	  (string= (oref (oref this trace-request) trace-type) "uncaught")))

  ;; Set command name.
  (oset this name "trace_exceptions"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-trace-exceptions))
  "Creates the command line for the trace_exceptions command."
  (let* ((request (oref this trace-request))
	 (cmd (format "%s %s %s"
		      (call-next-method)
		      (oref request exception-class)
		      (oref request trace-type))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd
	      (format
	       "%s -cf \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd
	      (format
	       "%s -cef \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-trace-exceptions))
  "Executes the trace_exceptions command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result))
	 (request (oref this trace-request))
	 (request-id (car (jdee-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'trace-req)
	  (oset
	   process
	   trace-req
	   (nconc (oref process trace-req)
		  (list (cons request-id request))))
	(oset process trace-req (list (cons request-id request)))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Trace exception %s enabled. Use request id %s to cancel."
		     (oref request exception-class) request-id)
	   (format "Error: unable to enable trace.\n Reason: %s."
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-cancel-trace (jdee-dbs-cmd)
  ((trace-request  :initarg :trace-request
		   :type jdee-dbs-trace-request
		   :documentation
		   "Trace request."))
  "Cancel a trace request.")


(defmethod initialize-instance ((this jdee-dbs-cancel-trace) &rest fields)
  "Constructor for cancel_trace command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'trace-request))

  ;; Set command name.
 (oset this name (oref (oref this trace-request) cancel-command)))


(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-cancel-trace))
  "Creates the command line for the cancel_trace command."
  (format "%s %s" (call-next-method) (oref (oref this trace-request) id)))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-cancel-trace))
  "Executes the cancel_trace command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this trace-request) id))
	       (requests
		(cl-remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process trace-req))))
	  (if requests
	      (oset process trace-req requests)
	    (slot-makeunbound process 'trace-req))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled trace request %s."
		     (oref (oref this trace-request) id))
	   (format "Error: unable to cancel trace %s.\n Reason: %s."
		   (oref (oref this trace-request) id)
		   (car (jdee-dbo-command-result-data result)))))

    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field                                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-watch-field (jdee-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type jdee-dbs-watch-field-request
		   :documentation
		   "Watch field request."))
  "Watch a field of an object or a specified class of objects.")


(defmethod initialize-instance ((this jdee-dbs-watch-field) &rest fields)
  "Constructor for watch field command."

  ;; Call parent initializer.
  (call-next-method)

  (let ((request (oref this watch-request)))

    (assert (or
	     (string= (oref request watch-type) "access")
	     (string= (oref request watch-type) "modification")))

    (assert (slot-boundp request 'object-class))
    (assert (slot-boundp request 'field-name)))

  ;; Set command name.
  (oset this name "watch"))

(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-watch-field))
  "Creates the command line for the watch-field command."
  (let* ((request (oref this watch-request))
	 (cmd (format
	       "%s %s %s %s"
	       (call-next-method)
	       (oref request object-class)
	       (oref request field-name)
	       (concat "for_" (oref request watch-type)))))

    (if (slot-boundp request 'object-id)
	(setq cmd (format "%s -oid %s" cmd (oref request object-id))))

    (if (slot-boundp request 'expression)
	(setq cmd (format "%s -if %s" cmd (oref request expression))))

    (if (slot-boundp request 'suspend-policy)
	(setq cmd (format "%s -sp %s" cmd (oref request suspend-policy))))

    (if (slot-boundp request 'inclusion-filters)
	(setq cmd
	      (format
	       "%s -cf \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request inclusion-filters) " "))))

    (if (slot-boundp request 'exclusion-filters)
	(setq cmd
	      (format
	       "%s -cef \"%s\""
	       cmd
	       (mapconcat (lambda (x) x) (oref request exclusion-filters) " "))))

    cmd))

(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-watch-field))
  "Executes the watch-field command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result))
	 (request (oref this watch-request))
	 (request-id (car (jdee-dbo-command-result-data result))))

    (when command-succeeded-p
      (oset request id request-id)
      (if (slot-boundp process 'watch-req)
	  (oset
	   process
	   watch-req
	   (nconc (oref process watch-req)
		  (list (cons request-id request))))
	(oset process watch-req (list (cons request-id request)))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Watch request field for field %s of %s instance of class %s is enabled. Use request id %s to cancel."
		     (oref request field-name)
		     (if (slot-boundp request 'object-id)
			 (oref request object-id)
		       "any")
		     (oref request object-class)
		     request-id)
	   (format "Error: unable to enable watch request.\n Reason: %s."
		   (car (jdee-dbo-command-result-data result)))))
    command-succeeded-p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Requests                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass jdee-dbs-cancel-watch (jdee-dbs-cmd)
  ((watch-request  :initarg :watch-request
		   :type jdee-dbs-watch-field-request
		   :documentation
		   "Watch request."))
  "Cancel a watch request.")


(defmethod initialize-instance ((this jdee-dbs-cancel-watch) &rest fields)
  "Constructor for cancel_watch command."

  ;; Call parent initializer.
  (call-next-method)

 (assert (slot-boundp this 'watch-request))

  ;; Set command name.
 (oset this name "clear"))


(defmethod jdee-dbs-cmd-make-command-line ((this jdee-dbs-cancel-watch))
  "Creates the command line for the clear command."
  (format "%s %s" (call-next-method) (oref (oref this watch-request) id)))


(defmethod jdee-dbs-cmd-exec ((this jdee-dbs-cancel-watch))
  "Executes the cancel watch command."
  (let* ((process (oref this process))
	 (result (call-next-method))
	 (command-succeeded-p (jdee-dbo-command-succeeded-p result)))

    (if command-succeeded-p
	(let* ((canceled-request-id (oref (oref this watch-request) id))
	       (requests
		(cl-remove-if
		 (lambda (r)
		   (= (car r) canceled-request-id))
		 (oref process watch-req))))
	  (if requests
	      (oset process watch-req requests)
	    (slot-makeunbound process 'watch-req))))

    (jdee-dbs-proc-display-debug-message
	 process
	 (if command-succeeded-p
	     (format "Canceled watch request %s."
		     (oref (oref this watch-request) id))
	   (format "Error: unable to cancel watch request %s.\n Reason: %s."
		   (oref (oref this watch-request) id)
		   (car (jdee-dbo-command-result-data result)))))

    command-succeeded-p))

(provide 'jdee-dbs)

;;; jdee-dbs.el ends here
