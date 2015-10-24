;;; jdee-dbo.el -- JDEbug output functions
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 1999, 2001, 2002, 2004 Paul Kinnucan.
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

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jdee.sourceforge.net/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'eieio)
(require 'jdee-widgets)
(require 'tree-widget)

;; FIXME: refactor to eliminate these
(defvar jdee-bug-local-variables)
(defvar jdee-bug-stack-info)
(defvar jdee-bug-raise-frame-p)
(defvar jdee-dbs-the-debugger)
(defvar jdee-dbs-the-process-morgue)
(defvar jdee-dbs-the-process-registry)

;; FIXME: these are all used here; merge this file with jdee-dbs.el!
;; jdee-dbs-get-process, jdee-dbs-proc-get-bpspec, jdee-db-breakpoint-get-line,
;; jdee-db-mark-breakpoint-active, jdee-dbs-proc-display-debug-message,
;; jdee-dbs-display-debug-message, jdee-dbs-proc-state-info-set,
;; jdee-dbs-proc-set-contains-p, jdee-dbs-proc-move-to-registry,
;; jdee-db-set-debug-cursor, jdee-dbs-get-locals, jdee-dbs-cmd-exec,
;; jdee-dbs-get-this, jdee-dbs-java-null-p, jdee-dbs-get-thread,
;; jdee-db-set-all-breakpoints-specified, jdee-dbs-proc-set-state,
;; jdee-dbs-proc-move-to-morgue, jdee-dbs-java-primitive-p,
;; jdee-dbs-java-udci-p, jdee-dbs-get-string, jdee-dbs-java-array-p

(defclass jdee-dbo-thread ()
  ((id      :initarg :id)
   (name    :initarg :name)
   (state   :initarg :state)
   (status  :initarg :status))
  "Process thread.")

(defun jdee-dbo-make-thread-obj (thread-spec)
  (jdee-dbo-thread "thread"
		  :id     (nth 1 thread-spec)
		  :name   (nth 2 thread-spec)
		  :state  (nth 3 thread-spec)
		  :status (nth 4 thread-spec)))


(defun jdee-dbo-command-result (id &rest args)
"Returns the result of normally executing command specified by ID.
The result consists of a list whose first element is the command ID,
whose second element is the symbol `normal' to indicate a normal
result and whose remaining element is a list of optional result data."
 (list id 'normal args))

(defun jdee-dbo-command-error (id &rest args)
"Returns a command error result. The result consists of list whose first
element is the command's id, whose second element is the symbol `error'
 to indicate that an error occured and whose third element is a list
of optional error data."
 (list id 'error args))

(defun jdee-dbo-command-result-id (result)
  (nth 0 result))

(defun jdee-dbo-command-succeeded-p (result)
  (equal (nth 1 result) 'normal))

(defun jdee-dbo-command-result-data (result)
  (nth 2 result))

(defun jdee-dbo-report-ids-in-use (id-count)
  (message "%d object ids in use." id-count))

(defun jdee-dbo-init-debug-session ()
  (oset jdee-dbs-the-debugger started-p t))

(defun jdee-dbo-debug (debug-info)
  (message "Debug message: %s" debug-info))

(defun jdee-dbo-spec-resolved (proc-id spec-id)
  "Notifies resolution of breakpoint, watchpoint, or
exception spec."
  (let* ((proc (jdee-dbs-get-process proc-id))
	 (bpspec (if proc (jdee-dbs-proc-get-bpspec proc spec-id)))
	 (bp (if bpspec (oref bpspec breakpoint)))
	 (file (if bp (oref bp file)))
	 (line (if bp (jdee-db-breakpoint-get-line bp))))
    (and proc file line
	 (progn
	   (oset bp status 'active)
	   (jdee-db-mark-breakpoint-active file line)
	   (jdee-dbs-proc-display-debug-message
	    proc
	    (format "Resolved breakpoint set in %s at line %s." file line))))))

(defun jdee-dbo-error (proc-id message)
  (jdee-dbs-display-debug-message proc-id message))

(defun jdee-dbo-message (proc-id message)
  (jdee-dbs-display-debug-message proc-id message))


(defun jdee-dbo-unknown-exception (exception)
  (jdee-dbs-proc-display-debug-message
   (jdee-dbs-get-target-process) exception))

(defun jdee-dbo-vm-start-event (process-id process-status process-state)
  (let* ((process (jdee-dbs-get-process process-id))
	 (thread-id (nth 1 process-state))
	 (thread-name (nth 2 process-state))
	 (state (nth 3 process-state))
	 (reason (nth 4 process-state)))
    (if process
	(let ((state-info (oref process state-info)))
	  (jdee-dbs-proc-state-info-set state-info state reason thread-id thread-name)
	  (oset process startupp t)
	  (jdee-dbs-proc-display-debug-message process "vm started...")
	  (cond
	   ((string= process-status "all")
	    (jdee-dbs-proc-display-debug-message process "All threads suspended...")))
	  ;; Sometimes the debugger is tardy responding to a launch command and thus the JDE thinks the
	  ;; process is dead. In this case, move the process back to the registry and
	  ;; make it the target process.
	  (when (jdee-dbs-proc-set-contains-p jdee-dbs-the-process-morgue process)
	    (jdee-dbs-proc-move-to-registry process)
	    (oset jdee-dbs-the-process-registry :target-process process)))
      (message "Start Event Error: can't find process object for process id %d" process-id))))

(defvar jdee-dbo-current-process nil "Used to keep track of the process
used in the last breakpoint hit event, and watch point hit event.")

(defvar jdee-dbo-current-thread-id nil "Used to keep track of the thread id
used in the last breakpoint hit event, and watch point hit event.")

(defun jdee-dbo-break (process state-info state reason thread-id thread-name
			      message proc-id class file line-no)
  (jdee-dbs-proc-state-info-set state-info state reason
			       thread-id thread-name)
  (setq jdee-dbo-current-process process)
  (setq jdee-dbo-current-thread-id thread-id)
  (if jdee-bug-local-variables
    (jdee-dbo-update-locals-buf process
			       thread-id 0))
  (if jdee-bug-stack-info (jdee-dbo-update-stack process thread-id))
  (oset process steppablep t)
  (jdee-dbs-display-debug-message proc-id message)
  (jdee-db-set-debug-cursor class file line-no)
  (when jdee-bug-raise-frame-p (raise-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Breakpoint Event Handler                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jdee-dbo-locals-open-nodes nil
  "List of name of tree nodes opened.")

(defun jdee-dbo-locals-open-p (node-name)
  "Return non-nil if NODE-NAME is the name of an open tree node."
  (member node-name jdee-dbo-locals-open-nodes))

(defun jdee-dbo-locals-update-open-nodes (tree)
  "Update the list of open nodes `jdee-dbo-locals-open-nodes'.
Called after each folding/unfolding of the `tree-widget' TREE.
See also the hook `tree-widget-after-toggle-fucntions'."
  (let ((node-name (widget-get tree :node-name))
	(open      (widget-get tree :open)))
    (if open
	(add-to-list 'jdee-dbo-locals-open-nodes node-name)
      (setq jdee-dbo-locals-open-nodes
	    (delete node-name jdee-dbo-locals-open-nodes)))))


(defun jdee-dbo-update-locals-buf (process thread frame)
  (let* ((cmd (jdee-dbs-get-locals
               "get locals"
               :process process
               :thread-id thread
               :stack-frame-index frame))
	 (locals (jdee-dbs-cmd-exec cmd))
	 var)

    (with-current-buffer (oref process locals-buf)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
	(erase-buffer))

      (let ((all (overlay-lists)))
        (mapc 'delete-overlay (car all))
        (mapc 'delete-overlay (cdr all)))

      (add-hook 'tree-widget-after-toggle-functions
		'jdee-dbo-locals-update-open-nodes nil t)

      (goto-char (point-min))

      ;; Insert the this object for this stack frame.
      (let* ((cmd (jdee-dbs-get-this
		   "get this"
		   :process process
		   :thread-id thread
		   :stack-frame-index frame))
	     (this-obj (jdee-dbs-cmd-exec cmd)))
	(if (not (typep this-obj 'jdee-dbs-java-null))
	    (progn
	      (let* ((id (oref this-obj :id))
		     (open (concat "this" (number-to-string id))))
                (widget-create 'jdee-widget-java-obj
                               :tag "this"
                               :node-name open
                               :open (jdee-dbo-locals-open-p open)
                               :process process
                               :object-id (oref this-obj :id))))))

      ;; Insert the local variables for this stack frame.
      (dolist (local-var locals)
	(jdee-dbo-view-var-in-buf (oref local-var value)
                                  (oref local-var name) process
                                  'jdee-dbo-locals-open-p (current-buffer))))))

(defun jdee-dbo-update-stack (process thread-id)
  (let* ((cmd  (jdee-dbs-get-thread "get_thread"
				   :process process
				   :thread-id thread-id))
	 (thread-info (jdee-dbs-cmd-exec cmd))
	 (stack (nth 5 thread-info)))
    (oset process :stack stack)
    (oset process :stack-ptr 0)))

(defun jdee-dbo-breakpoint-hit-event (process-id process-status process-state spec-id location a2 a3)
  (let ((process (jdee-dbs-get-process process-id)))
    (if process
	(let ((class (nth 0 location))
	      (file (nth 1 location))
	      (line-no (nth 2 location))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (state-info (oref process state-info)))
	  (if state-info
	      (jdee-dbo-break process state-info state reason thread-id
			     thread-name
			     (format "Breakpoint hit at line %d in %s (%s) on thread %s. All threads suspended." line-no class file thread-name)
			     process-id class file line-no)
	    (message "Breakpoint hit event error: state info object missing for process %d." process-id)))
      (message "Breakpoint hit event error: process object for process %d is missing." process-id))))

(defun jdee-dbo-step-event (proc-id status process-state location)
  "Handler for step events."
  (let ((process (jdee-dbs-get-process proc-id)))
    (if process
	(let ((class (nth 0 location))
	      (file (nth 1 location))
	      (line-no (nth 2 location))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (state-info (oref process state-info)))
	  (if state-info
	    (jdee-dbo-break process state-info state reason thread-id
			   thread-name
			   (format "Stepped to line %d in %s (%s) on thread %s. All threads suspended."
				   line-no class file thread-name)
			   proc-id class file line-no)
	    (message "Step event error: state info missing for process %d" proc-id)))
      (message "Step event error: could not find process %d." proc-id))))


(defun jdee-dbo-exception-event (proc-id status process-state spec-id exception-spec a3)
  (let ((process (jdee-dbs-get-process proc-id)))
    (if process
	(let ((exception-class (nth 0 exception-spec))
	      (exception-object (nth 1 exception-spec))
	      (thread-id (nth 1 process-state))
	      (thread-name (nth 2 process-state))
	      (state (nth 3 process-state))
	      (reason (nth 4 process-state))
	      (location (nth 5 process-state))
	      (state-info (oref process state-info)))
	  (if (not (equal status "none"))
	    ;; Then it's a break, not a trace
	    (let ((class (nth 1 (car location)))
		  (file (nth 2 (car location)))
		  (line-no (nth 3 (car location))))
		  (if state-info
		    (jdee-dbo-break process state-info state reason thread-id
				   thread-name
				   (format "Exception encountered at line %d in %s (%s) on thread %s. All threads suspended."
					   line-no class file thread-name)
				   proc-id class file line-no)
		    (message "Exception event error: state info missing for process %d" proc-id)))
	  (jdee-dbs-display-debug-message
	   proc-id
	   (format "Exception of class %s occurred on thread %s"
			 exception-class thread-name)))))))


(defun jdee-dbo-vm-disconnected-event (process-id process-status thread)
  (let ((process (jdee-dbs-get-process process-id)))
    (when process
	(jdee-dbs-proc-display-debug-message process "vm disconnected...")
	(setq overlay-arrow-position nil)
	(jdee-db-set-all-breakpoints-specified)
	(jdee-dbs-proc-set-state process "vm disconnected")
	(when (jdee-dbs-proc-set-contains-p jdee-dbs-the-process-registry process)
	  (jdee-dbs-proc-move-to-morgue process)
	  (slot-makeunbound jdee-dbs-the-process-registry :target-process)))))

(defun jdee-dbo-invalid-break (process-id arg2 reason)
  (jdee-dbs-proc-display-debug-message
   (jdee-dbs-get-process process-id)
   (concat "Invalid break error.\n  Reason: " reason)))

(defun jdee-dbo-vm-death-event (process-id process-status thread)
  (let* ((process (jdee-dbs-get-process process-id))
	 (main-class (oref process main-class)))
    (jdee-dbs-proc-display-debug-message
     process
     (format "%s process ended." main-class))
    (when (jdee-dbs-proc-set-contains-p jdee-dbs-the-process-registry process)
      (jdee-dbs-proc-move-to-morgue process)
      (slot-makeunbound jdee-dbs-the-process-registry :target-process))
    (setq overlay-arrow-position nil)))

(defclass jdee-dbo-method ()
  ((class    :initarg :class
	     :type string)
   (name     :initarg :name
	     :type string)
   (returns  :initarg :returns
	     :type string)
   (args     :initarg :args
	     :type list)
   (kind     :initarg :kind
	     :type string))
  "Method")

(defmethod jdee-dbo-to-string ((this jdee-dbo-method))
  (format "<%s %s.%s(%s)>"
	  (oref this :returns)
	  (oref this :class)
	  (oref this :name)
	  (mapconcat (lambda (x) x) (oref this :args) ",")))

(defun jdee-dbo-make-method (spec)
  (let ((m
	 (jdee-dbo-method "method"
			 :class   (nth 0 spec)
			 :name    (nth 1 spec)
			 :returns (nth 2 spec)
			 :args    (nth 3 spec))))
    (if (nth 4 spec)
	(oset m :kind (nth 4 spec)))
    m))

(defun jdee-dbo-class-prepare-event (process-id process-status thread-spec class-name)
  (let* ((thread (jdee-dbo-make-thread-obj thread-spec))
	 (process (jdee-dbs-get-process process-id)))
    (jdee-dbs-proc-display-debug-message
     process
     (format "Preparing class %s.\n  Thread: %s. Status: %s.\n"
	     class-name
	     (oref thread name)
	     (oref thread status)))))

(defun jdee-dbo-class-unload-event (process-id process-status thread-spec class-name)
  (let* ((thread (jdee-dbo-make-thread-obj thread-spec))
	 (process (jdee-dbs-get-process process-id)))
    (jdee-dbs-proc-display-debug-message
     process
     (format "Unloading class %s.\n  Thread: %s. Status: %s.\n"
	     class-name
	     (oref thread name)
	     (oref thread status)))))

(defun jdee-dbo-method-entry-event (process-id process-status thread-spec method-spec)
  (let* ((thread (jdee-dbo-make-thread-obj thread-spec))
	 (method (jdee-dbo-make-method method-spec))
	 (method-sig (jdee-dbo-to-string method))
	 (process (jdee-dbs-get-process process-id)))
    (jdee-dbs-proc-display-debug-message
     process
     (format "Entering %s.%s\n  Thread: %s\n  Signature: %s\n"
	     (oref method class)
	     (oref method name)
	     (oref thread name)
	     method-sig))))

(defun jdee-dbo-method-exit-event (process-id process-status thread-spec method-spec)
  (let* ((thread (jdee-dbo-make-thread-obj thread-spec))
	 (method (jdee-dbo-make-method method-spec))
	 (method-sig (jdee-dbo-to-string method))
	 (process (jdee-dbs-get-process process-id)))
    (jdee-dbs-proc-display-debug-message
     process
     (format "Exiting %s.%s\n  Thread: %s\n  Signature: %s\n"
	     (oref method class)
	     (oref method name)
	     (oref thread name)
	     method-sig))))

(defun jdee-dbo-watchpoint-hit-event (process-id process-status thread-spec request-id &rest data)
  (let ((process (jdee-dbs-get-process process-id)))
    (if process
	(let* ((thread (jdee-dbo-make-thread-obj thread-spec))
	       (thread-id (oref thread id))
	       (thread-name (oref thread name))
	       (thread-state (oref thread state))
	       (thread-status (oref thread status))

	       ;; Object whose field was accessed or modified.
	       (obj-spec (nth 0 data))
	       (obj-class (nth 0 obj-spec))
	       (obj-id (nth 1 obj-spec))
	       (obj-gc-flag  (nth 2 obj-spec))
	       ;; Field that was accessed or modified.
	       (field-spec (nth 1 data))
	       (field-decl (nth 0 field-spec))
	       (field-name (nth 0 field-decl))
	       (field-type (nth 1 field-decl))
	       (field-qual (if (> (length field-decl) 3) (nth 2 field-decl)))
	       (field-value-type (nth 1 field-spec))
	       (field-value (nth 2 field-spec))
	       ;; Breakpoint data
	       (breakpoint-spec (nth 2 data))
	       (breakpoint-class (nth 0 breakpoint-spec))
	       (breakpoint-file (nth 1 breakpoint-spec))
	       (breakpoint-line (nth 2 breakpoint-spec))
	       ;; Object match data
	       (obj-match (nth 3 data))
	       ;; Thread match data
	       (thread-match (nth 4 data))
	       ;; Expression true data
	       (expression-true (nth 5 data)))

	  (jdee-dbs-proc-display-debug-message
	   process
	   (format "<%s:%s> accessed or modified at line %s in %s.\n  Watched field: %s %s %s = %s\n"
		   obj-class obj-id breakpoint-line breakpoint-file
		   (if field-qual field-qual "") field-type field-name field-value))

	  (if (string= thread-status "suspended by debugger")
	      (let ((state-info (oref process state-info)))
		(if state-info
		    (progn
		      (jdee-dbs-proc-state-info-set
		       state-info thread-state
		       thread-status thread-id thread-name)
		      (setq jdee-dbo-current-process process)
		      (setq jdee-dbo-current-thread-id thread-id)
		      (if jdee-bug-local-variables
			  (jdee-dbo-update-locals-buf process
						     thread-id 0))
		      (if jdee-bug-stack-info (jdee-dbo-update-stack process
								   thread-id))
		      (oset process steppablep t)
		      (jdee-db-set-debug-cursor breakpoint-class breakpoint-file
					 breakpoint-line)
		      (when jdee-bug-raise-frame-p (raise-frame))

		      (jdee-dbs-display-debug-message
		       process-id
		       (format "Stopping at line %d in %s (%s) on thread %s."
			       breakpoint-line breakpoint-class breakpoint-file thread-name)))
		  (message "Watchpoint event error: state info object missing for process %d."
			   process-id)))))
      (message "Watchpoint event error: process object for process %d is missing." process-id))))


(defun jdee-dbo-event-set (process-id process-status thread &rest events)
  "Invoked when a set of debugger events occurs. EVENTS is a list of
lists. The first element is the name of a function that handles the event.
The remaining elements are arguments to pass to the handler."
   (mapc
    (lambda (event)
      (let ((handler (car event))
	   (args (cdr event)))
	(apply handler
	       (append (list process-id process-status thread) args))))
    events))

(defun jdee-dbo-view-var-in-buf (var-value name process open buf
					  &optional clear)
  "Create a tree-widget representing variable VAR-VALUE (a
  jdee-dbs-java-null/primitive/udci type), whose name is NAME and in
  process PROCESS, and place that tree-widget in buffer BUF.  OPEN is
  either a variable or a function.  If it is a variable, then the
  tree-widget is open if true.  If it is a function, then the function
  is called with name to return a true or false value (this is used
  for caching the state). If CLEAR is true then the buffer is cleared
  before creating the tree-widget."
  (with-current-buffer buf
    (when clear
      (erase-buffer))
    (let* ((var-tag (format "%s %s [id: %s]" (oref var-value jtype) name
			   (if (or (typep var-value 'jdee-dbs-java-primitive)
				    (typep var-value 'jdee-dbs-java-null))
			     "-"
			     (oref var-value id))))
	  (openp (if (functionp open)
		   (funcall open var-tag)
		   open)))
      (cond
	((typep var-value 'jdee-dbs-java-udci)
	  (if (string= (oref var-value :jtype) "java.lang.String")
	    (let* ((cmd (jdee-dbs-get-string
			 "get string"
			 :process process
			 :object-id (oref var-value id)))
		   (str-val (jdee-dbs-cmd-exec cmd)))
	      (widget-create 'tree-widget
			     :tag var-tag
			     :node-name var-tag
			     :open openp
			     :value t
			     (list 'tree-widget :tag str-val)))
	    (widget-create 'jdee-widget-java-obj
			   :tag var-tag
			   :node-name var-tag
			   :open openp
			   :process process
			   :object-id (oref var-value :id))))
	   ((typep var-value 'jdee-dbs-java-array)
	    (widget-create 'jdee-widget-java-array
			   :tag var-tag
			   :node-name var-tag
			   :open openp
			   :process process
			   :object var-value))
	   ((typep var-value 'jdee-dbs-java-primitive)
	    (widget-create 'tree-widget
			   :tag var-tag
			   :node-name var-tag
			   :open openp
			   :value t
			   (list 'tree-widget
				 :tag (format "%s" (oref var-value value)))))
	   ((typep var-value 'jdee-dbs-java-null)
	    (widget-create 'tree-widget :tag var-tag :value t
			   (list 'tree-widget :tag "null")))
	   (t
	    (error "Unidentified type of variable: %s" var-tag))))
    (use-local-map widget-keymap)
    (widget-setup)))

(provide 'jdee-dbo)

;; End of jdee-dbo.el
