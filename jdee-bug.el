;;; jdee-bug.el -- JDEbug Interface
;; $Id$

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

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;;; Code:

(require 'cl-lib)
(require 'jdee-parse)
(require 'jdee-dbs)
(require 'jdee-dbo)
(require 'jdee-db)
(require 'jdee-run)
(require 'efc)
(require 'semantic/util-modes);; semantic-add-minor-mode

;; FIXME: refactor
(declare-function jdee-convert-cygwin-path "jdee" (path &optional separator))
(declare-function jdee-describe-map "jdee" (map))
(declare-function jdee-find-jdee-doc-directory "jdee" ())
(declare-function jdee-java-major-version "jdee" ())
(declare-function jdee-java-minor-version "jdee" ())
(declare-function jdee-normalize-path "jdee" (path &optional symbol))

(defgroup jdee-bug nil
  "JDEbug General Options"
  :group 'jdee
  :prefix "jdee-bug")


(defcustom jdee-bug-debug nil
"*IMPORTANT!!!! Leave this switch in its default state (off) unless
you need to modify the *JDEbug* Java source code. Setting this switch
on causes the JDE to load *JDEbug* from its java/classes directory
instead of from jde.jar. It also causes the JDE to run the debugger in
debug server mode. This allows you to use *JDEbug* to debug itself."
  :group 'jdee-bug
  :type 'boolean)

(defcustom jdee-bug-jpda-directory ""
  "*Pathname of the directory containing Sun's Java Platform Debug Architecture
distribution. You need to set this variable only if this project uses a JDK 1.2 vm."
  :group 'jdee-bug
  :type 'file)


(defun jdee-bug-vm-includes-jpda-p ()
  "Returns t if the current JDK provides built-in support for JPDA."
  (or
   (> (jdee-java-major-version) 1)
   (> (jdee-java-minor-version) 2)))


(defcustom jdee-bug-jre-home ""
"*Home directory of the JRE containing the executable used to
run debuggee processes.
This variable specifies the home directory of the Java runtime
environment containing the executable, e.g., java, to be used to
launch processes (see `jdee-bug-vm-executable'). If you do not specify
a home directory, the home directory is the same as that of the
executable used to run the debugger itself."
  :group 'jdee-bug :type 'string)


(defcustom jdee-bug-vm-executable (list (if (eq system-type 'windows-nt) "javaw" "java"))
  "*Name of the executable used to launch target processes.
This defaults to java on Unix platforms and javaw on Windows platforms"
  :group 'jdee-bug
  :type '(list
	  (radio-button-choice
	  (const "java")
	  (const "javaw")
	  (const "java_g"))))

(defcustom jdee-bug-raise-frame-p t
  "*Raise frame when a breakpoint is hit."
  :group 'jdee-bug
  :type 'boolean)

(defcustom jdee-bug-server-socket (cons t "2112")
  "*Socket where debugger listens for apps needing debugger services.
You can arrange for a vm to connect to JDEbug via a socket by starting it with the
options -Xdebug and -Xrunjdwp:transport=dt_socket,address=MYHOST:NNNN,
where MYHOST is the name of the machine running the debugger and
NNNN is the socket specified by this variable. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE."
  :group 'jdee-bug
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Address")))

(defcustom jdee-bug-server-shmem-name (cons t "JDEbug")
  "*Shared memory name under which the debugger listens for apps
needing debugger services. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE."
  :group 'jdee-bug
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Name")))


(defcustom jdee-bug-debugger-host-address system-name
  "*Address of system on which JDEbug is running.
The default value is the value of the standard Emacs variable `system-name'.
The JDE uses the host address to connect to JDEBug during startup. On some Windows
systems, the JDE is unable to connect to the debugger socket under the system name.
If this happens, you can try setting this variable to the absolute address of
a local host: 127.0.0.1 ."
  :group 'jdee-bug
  :type 'string)


(defcustom jdee-bug-debugger-command-timeout 30
  "*Length of time in seconds the JDE waits for a response from the debugger to a command."
  :group 'jdee-bug
  :type 'integer)

(defcustom jdee-bug-saved-breakpoints nil
"*Breakpoints to be set for the current project."
  :group 'jdee-bug
  :type '(repeat
	  (cons :tag "Break at"
	   (string :tag "File Name")
	   (integer :tag "Line Number"))))


(defcustom jdee-bug-breakpoint-cursor-colors (cons "cyan" "brown")
"*Specifies the foreground and background colors of the debugger's
breakpoint cursor."
  :group 'jdee-bug
  :type '(cons
	  (string :tag "Foreground Color")
	  (string :tag "Background Color"))
  :set '(lambda (sym val)
	  (make-face 'jdee-bug-breakpoint-cursor)
	  (set-face-foreground 'jdee-bug-breakpoint-cursor (car val))
	  (set-face-background 'jdee-bug-breakpoint-cursor (cdr val))
	  (set-default sym val)))


(defgroup jdee-bug-window nil
  "JDEbug Window Preferences"
  :group 'jdee-bug
  :prefix "jdee-bug-window")

(defcustom jdee-bug-window-message nil
  "Message buffer window preferences."
  :group 'jdee-bug-window
  :type 'list)

(defcustom jdee-bug-local-variables nil "A non nil values makes the JDEBug
retrieve the local variables after every step command. It is recommended to
enable this feature just when is needed since retrieving the local variables
is time consuming and slow down stepping through the code."
  :group 'jdee-bug
  :type 'boolean)

(defcustom jdee-bug-stack-info nil "A non nil values makes the JDEBug retrieve
the stack info every step command. It is recommended to enable
this feature just when is needed since retrieving the stack info is time
consuming and slows down stepping through the code."
  :group 'jdee-bug
  :type 'boolean)

(defvar jdee-bug-minor-mode-hook nil
  "Hook to run when entering or leaving jdee-bug-minor-mode.")

(defvar jdee-bug-menu-spec
  (list "JDEbug"

	["Step Over"
	 jdee-bug-step-over
	 (jdee-dbs-target-process-steppable-p)]

	["Step Into"
	 jdee-bug-step-into
	 (jdee-dbs-target-process-steppable-p)]

	["Step Into All"
	 jdee-bug-step-into-all
	 (jdee-dbs-target-process-steppable-p)]

	["Step Out"
	 jdee-bug-step-out
	 (jdee-dbs-target-process-steppable-p)]

	["Run"
	 jdee-debug
	 :active                    t
	 :included                  (null (jdee-dbs-debugger-running-p)) ]

	["Continue"
	 jdee-bug-continue
	 :active                    (jdee-dbs-target-process-runnable-p)
	 :included                  (jdee-dbs-debugger-running-p)        ]

	["Exit Debugger"
	 jdee-bug-exit
	 (jdee-dbs-debugger-running-p)]

	"-"
	;; Added by lea
	["Toggle Breakpoint"
	 jdee-bug-toggle-breakpoint t]
	["Set Conditional Breakpoint"
	 jdee-bug-set-conditional-breakpoint nil]
	["Break on exception"
	 jdee-bug-break-on-exception
	 :active (and
		  (jdee-dbs-debugger-running-p)
		  (jdee-dbs-get-target-process))]
	["Save Breakpoints"
	 jdee-bug-save-breakpoints nil]
	(list
	 "Watch for Field"

	 ["Access"
	  jdee-bug-watch-field-access
	  :style    nil
	  :active   (and
		     (jdee-dbs-debugger-running-p)
		     (jdee-dbs-get-target-process))]


	 ["Modification"
	  jdee-bug-watch-field-modification
	  :style   nil
	  :active  (and
		    (jdee-dbs-debugger-running-p)
		    (jdee-dbs-get-target-process))]

	 ["Cancel"
	  jdee-bug-cancel-watch
	  :style     nil
	  :active    (and
		      (jdee-dbs-debugger-running-p)
		      (jdee-dbs-get-target-process)
		      (slot-boundp
		       (jdee-dbs-get-target-process)
		       'watch-req))]

	 )

	(list
	 "Trace"

	 ["Class Prep..."
	  jdee-bug-trace-class-prep
	  :style    nil
	  :active	 (and
			  (jdee-dbs-debugger-running-p)
			  (jdee-dbs-get-target-process))]

	 ["Class Unload..."
	  jdee-bug-trace-class-unload
	  :style    nil
	  :active	 (and
			  (jdee-dbs-debugger-running-p)
			  (jdee-dbs-get-target-process))]


	 ["Method Entry..."
	  jdee-bug-trace-method-entry
	  :style    nil
	  :active	 (and
			  (jdee-dbs-debugger-running-p)
			  (jdee-dbs-get-target-process))]

	 ["Method Exit..."
	  jdee-bug-trace-method-exit
	  :style    nil
	  :active	 (and
			  (jdee-dbs-debugger-running-p)
			  (jdee-dbs-get-target-process))]

	 ["Exceptions..."
	  jdee-bug-trace-exceptions
	  :style    nil
	  :active	 (and
			  (jdee-dbs-debugger-running-p)
			  (jdee-dbs-get-target-process))]


	 ["Cancel..."
	  jdee-bug-cancel-trace
	  :style     nil
	  :active    (and
		      (jdee-dbs-debugger-running-p)
		      (jdee-dbs-get-target-process)
		      (slot-boundp
		       (jdee-dbs-get-target-process)
		       'trace-req))]

	 )

	"-"

	(list
	 "Display"

	 ["Loaded Classes"
	  jdee-bug-display-loaded-classes
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["Object Monitors"
	  jdee-bug-show-object-monitors
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["Path Info"
	  jdee-bug-display-path-info
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]
	 )

	["Display Variable At Point"
	 jdee-bug-display-variable
	 (and
          (jdee-dbs-debugger-running-p)
          (jdee-dbs-get-target-process))]

	["Evaluate Expression"
	 jdee-bug-evaluate-expression
	 (and
	  (jdee-dbs-debugger-running-p)
	  (jdee-dbs-get-target-process))]

	(list
	 "Stack"
	 `["Enable"
	   jdee-bug-toggle-stack-info
	   :enable t
	   :style radio
	   :selected jdee-bug-stack-info]
	 ["Up"
	  jdee-bug-up-stack
	  (and
	   (jdee-dbs-target-process-steppable-p)
	   (let* ((process (jdee-dbs-get-target-process))
		  (stack-max
		   (if (slot-boundp process 'stack)
		       (1- (length (oref process stack)))
		     0))
		  (stack-ptr (oref process stack-ptr)))
	     (< stack-ptr stack-max)))]

	 ["Down"
	  jdee-bug-down-stack
	  (and
	   (jdee-dbs-target-process-steppable-p)
	   (let* ((process (jdee-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))]

	 )
	(list
	 "Thread"

	 ["Suspend"
	  jdee-bug-suspend-thread
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]


	 ["Resume"
	  jdee-bug-resume-thread
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["Interrupt"
	  jdee-bug-interrupt-thread
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["Stop"
	  jdee-bug-stop-thread
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

                                        ; ["Show Thread Info"
                                        ;   jdee-bug-thread-show-thread-info nil]
	 )

	(list
	 "Processes"
	 ["Start Debugger"
	  jdee-bug-start-debugger
	  (not (jdee-dbs-debugger-running-p))]

	 ["Launch Process"
	  jdee-bug-launch-process
	  (jdee-dbs-debugger-running-p)]

	 ["Suspend Process"
	  jdee-bug-suspend-process
	  (let ((process (jdee-dbs-get-target-process)))
	    (and
	     (jdee-dbs-debugger-running-p)
	     process
	     (not (oref process suspendedp))))]

	 ["Resume Process"
	  jdee-bug-resume-process
	  (let ((process (jdee-dbs-get-target-process)))
	    (and
	     (jdee-dbs-debugger-running-p)
	     process
	     (oref process suspendedp)))]

	 ["Finish Process"
	  jdee-bug-finish-process
	  (let ((process (jdee-dbs-get-target-process)))
	    (and
	     (jdee-dbs-debugger-running-p)
	     process
	     (not (oref process attachedp))))]

	 "-"

	 (list
	  "Attach Process"
	  ["Via Shared Memory"
	   jdee-bug-attach-via-shared-memory
	   (and
	    (eq system-type 'windows-nt)
	    (jdee-dbs-debugger-running-p))]

	  ["On Local Host"
	   jdee-bug-attach-local-host
	   (jdee-dbs-debugger-running-p)]

	  ["On Remote Host"
	   jdee-bug-attach-remote-host
	   (jdee-dbs-debugger-running-p)]
	  )

	 (list
	  "Listen on"
	  ["Shared Memory"
	   jdee-bug-listen-shmem
	   (and
	    (eq system-type 'windows-nt)
	    (jdee-dbs-debugger-running-p))]

	  ["Socket"
	   jdee-bug-listen-socket
	   (jdee-dbs-debugger-running-p)]
	  )

	 ["Detach Process"
	  jdee-bug-detach-process
	  (let ((process (jdee-dbs-get-target-process)))
	    (and
	     (jdee-dbs-debugger-running-p)
	     process
	     (oref process attachedp)))]


	 "-"

	 ["Set Target Process"
	  jdee-bug-set-target-process
	  (> (jdee-dbs-proc-set-get-size
	      jdee-dbs-the-process-registry)
	     0)]

	 ["Show Processes"
	  jdee-bug-set-show-processes nil]

	 ["Remove Dead Processes"
	  jdee-bug-remove-dead-processes
	  (oref jdee-dbs-the-process-morgue proc-alist)]

	 )
	(list
	 "Show Buffer"

	 ["Locals"
	  jdee-bug-show-locals-buf
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["CLI"
	  jdee-bug-show-cli-buf
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]

	 ["Threads"
	  jdee-bug-show-threads-buf
	  (and
	   (jdee-dbs-debugger-running-p)
	   (jdee-dbs-get-target-process))]
	 )
	["Show Debug Frame"
	 jdee-bug-show-debug-frame
	 (and (jdee-dbs-debugger-running-p)
	      (jdee-dbs-get-target-process))]
	"-"
	["Preferences"
	 jdee-bug-show-preferences t]
	"-"
	["Help"
	 jdee-bug-help t]
	)
  "JDEbug menu specification")

(defvar jdee-bug-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define jdee-bug-menu km "JDEbug Minor Mode Menu"
		      jdee-bug-menu-spec)
    km)
  "Keymap for JDEbug minor mode.")

;;;###autoload
(define-minor-mode jdee-bug-minor-mode nil
  :keymap jdee-bug-mode-map)

(semantic-add-minor-mode 'jdee-bug-minor-mode " JDEbug")

;; (fmakunbound 'jdee-bug-key-bindings)
(defcustom jdee-bug-key-bindings
  (list (cons "[?\C-c ?\C-z ?\C-s]" 'jdee-bug-step-over)
	(cons "[?\C-c ?\C-z ?\C-x]" 'jdee-bug-step-into)
	(cons "[?\C-c ?\C-z ?\C-a]" 'jdee-bug-step-into-all)
	(cons "[?\C-c ?\C-z ?\C-w]" 'jdee-bug-step-out)
	(cons "[?\C-c ?\C-z ?\C-c]" 'jdee-bug-continue)
	(cons "[?\C-c ?\C-z ?\C-b]" 'jdee-bug-toggle-breakpoint))
  "*Specifies key bindings for JDEbug.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jdee-bug
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'jdee-bug-key-bindings)
	       jdee-bug-key-bindings)
	      (mapc
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]"key)
		       (setq key (car (read-from-string key))))
		   (define-key jdee-bug-mode-map key nil)))
	       jdee-bug-key-bindings))
	  ;; Map new key bindings.
	  (mapc
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]"key)
		   (setq key (car (read-from-string key))))
	       (define-key jdee-bug-mode-map key fcn)))
	   val)
	  (set-default sym val)))


(defun jdee-bug-step-over ()
  "Advances the process to the next line in the current method."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-step "step over" :process process)))
    (jdee-dbs-cmd-exec cmd)))

(defun jdee-bug-step-into ()
  "Advances to the next step in the method at point except if the method
   belongs to the java, javax, or sun packages."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-step "step into" :process process :step-type "into")))
    (jdee-dbs-cmd-exec cmd)))

(defun jdee-bug-step-into-all ()
  "Advances the process into the function invoked at point."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-step "step into" :process process :step-type "into-all")))
    (jdee-dbs-cmd-exec cmd)))

(defun jdee-bug-step-out ()
  "Advances the process to the next line in the invoking method."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-step "step into" :process process :step-type "out")))
    (jdee-dbs-cmd-exec cmd)))


(defun jdee-bug-continue ()
  "Runs the target process. Execution continues from the current breakpoint."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (run (jdee-dbs-run-process
	       (format "run %d" (oref process id))
		  :process process)))
    (oset process startupp nil)
    (oset process suspendedp nil)
    (oset process steppablep nil)
    (setq overlay-arrow-position nil)
    (jdee-dbs-cmd-exec run)))



(defun jdee-bug-exit ()
  (interactive)
  (if (jdee-dbs-debugger-running-p)
      (progn
	(mapc
	 (lambda (assoc-x)
	   (let* ((process (cdr assoc-x))
		  (finish-cmd (jdee-dbs-finish-process
			       (format "finish %d" (oref process id))
			       :process process))
		  (result (jdee-dbs-cmd-exec finish-cmd)))
	     (jdee-dbs-proc-move-to-morgue process)))
	 (oref jdee-dbs-the-process-registry proc-alist))
	(slot-makeunbound jdee-dbs-the-process-registry :target-process)
	(jdee-dbs-debugger-quit jdee-dbs-the-debugger))
    (error "Debugger is not running.")))


(add-hook
 'jdee-mode-hook
 (lambda ()
   (if (buffer-file-name)
       (let ((this-file (file-name-nondirectory (buffer-file-name))))
	 (mapc
	  (lambda (spec)
	    (let* ((file (car spec))
		   (line (cdr spec))
		   (bp (jdee-db-find-breakpoint file line)))
	      (when (not bp)
		(setq jdee-db-breakpoint-id-counter (1+ jdee-db-breakpoint-id-counter))
		(setq bp
		      (jdee-db-breakpoint
		       (format "breakpoint%d" jdee-db-breakpoint-id-counter)
		       :id jdee-db-breakpoint-id-counter
		       :file file
		       :line line))
		(jdee-db-breakpoints-add bp))
	      (if (string-match file this-file)
		  (jdee-db-mark-breakpoint-specified file line))))
	  jdee-bug-saved-breakpoints)))))


(defun jdee-bug-set-breakpoint()
  "Sets a breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jdee-get-line-at-point))
	 (bp (jdee-db-find-breakpoint file line))
	 (proc (jdee-dbs-get-target-process)))
    (unless bp
      (setq bp (jdee-db-spec-breakpoint))
      (oset bp line line)
      (jdee-db-breakpoints-add bp))
    (if (and bp proc)
	(let* ((set-breakpoint (jdee-dbs-set-breakpoint
				"set breakpoint"
				:process proc
				:breakpoint bp))
	       (result (jdee-dbs-cmd-exec set-breakpoint)))
	  (message "Breakpoint set at line %d in class %s." line file)))))


(defun jdee-bug-set-conditional-breakpoint ()
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jdee-bug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jdee-get-line-at-point))
	 (bp (jdee-db-find-breakpoint file line))
	 (proc (jdee-dbs-get-target-process)))
    (if (and bp proc)
	(let* ((clear-breakpoint
		(jdee-dbs-clear-breakpoint
		 "clear breakpoint"
		 :process proc
		 :breakpoint bp))
	       (result (jdee-dbs-cmd-exec clear-breakpoint)))))
    (if bp
	(jdee-db-delete-breakpoint bp))))

;; test by lea

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Toggle Breakpoint Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-toggle-breakpoint ()
  "Toggles the breakpoint on the current line."
  (interactive)

  (assert (equal major-mode 'jdee-mode)
	  nil "You can only toggle a breakpoint within jdee-mode")

  (assert (jdee-db-src-dir-matches-file-p (buffer-file-name))
	  nil "The current buffer is not in the source path.  See `jdee-sourcepath' for more information.")

  (let*  ((file (buffer-file-name))
	  (line (jdee-get-line-at-point))
	  (bp (jdee-db-find-breakpoint file line)))
    (if bp
	(jdee-bug-clear-breakpoint)
      (jdee-bug-set-breakpoint))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Save Breakpoints Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-save-breakpoints ()
  "Save breakpoints in project file."
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-bug-trace-methods-dialog (efc-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "entry"
			     :documentation
			     "Values may be entry or exit.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.")
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace methods dialogs."
)

(defmethod initialize-instance ((this jdee-bug-trace-methods-dialog) &rest fields)
  "Constructor for trace methods dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "entry")
	      (string= (oref this trace-type) "exit")))
)


(defmethod efc-dialog-create ((this jdee-bug-trace-methods-dialog))

  (widget-insert (concat "Trace method "
			 (oref this trace-type)
			 "\n\n"))

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on method entry or exit."
	   (const "all")
	   (const "thread")
	   (const "none"))))


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose methods should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose methods should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))
  )

(defmethod efc-dialog-ok ((this jdee-bug-trace-methods-dialog))
  (let* ((thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-exclusion-field)))
	 (process (jdee-dbs-get-target-process))
	 (request (jdee-dbs-trace-methods-request "trace methods request"
						 :trace-type (oref this trace-type)))
	 (cmd  (jdee-dbs-trace-methods
		"trace methods command"
		:process process :trace-request request)))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))

    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jdee-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jdee-bug-trace-method-entry ()
  "Displays the trace method entry dialog."
  (interactive)
  (let ((dialog (jdee-bug-trace-methods-dialog "trace method entry dialog")))
    (efc-dialog-show dialog)))

(defun jdee-bug-trace-method-exit ()
  "Displays the trace method exit dialog."
  (interactive)
  (let ((dialog (jdee-bug-trace-methods-dialog
		 "trace method exit dialog" :trace-type "exit")))
    (efc-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jdee-bug-trace-classes-dialog (efc-dialog)
  ((trace-type               :initarg :trace-type
			     :type string
			     :initform "preparation"
			     :documentation
			     "Values may be preparation or unloading.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.")
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Class of trace classes dialogs."
)

(defmethod initialize-instance ((this jdee-bug-trace-classes-dialog) &rest fields)
  "Constructor for trace classes dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "preparation")
	      (string= (oref this trace-type) "unloading")))
)


(defmethod efc-dialog-create ((this jdee-bug-trace-classes-dialog))

  (widget-insert (concat "Trace class "
			 (oref this trace-type)
			 "\n\n"))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))
  )

(defmethod efc-dialog-ok ((this jdee-bug-trace-classes-dialog))
  (let* ((thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jdee-dbs-get-target-process))
	 (request (jdee-dbs-trace-classes-request "trace classes request"
						 :trace-type (oref this trace-type)))
	 (cmd  (jdee-dbs-trace-classes
		"trace classes command"
		:process process :trace-request request)))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))

    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jdee-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jdee-bug-trace-class-prep ()
  "Displays the trace class preparation dialog."
  (interactive)
  (let ((dialog (jdee-bug-trace-classes-dialog "trace class prep dialog")))
    (efc-dialog-show dialog)))

(defun jdee-bug-trace-class-unload ()
  "Displays the trace class unloading dialog."
  (interactive)
  (let ((dialog (jdee-bug-trace-classes-dialog
		 "trace class unloading dialog" :trace-type "unloading")))
    (efc-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jdee-bug-trace-exceptions-dialog (efc-dialog)
  ((exception-class-field    :initarg :exception-class
			     :documentation
			     "Class of exception to trace.")
   (trace-type-field         :initarg :trace-type
			     :documentation
			     "Values may be caught, uncaught, or both.")
   (thread-restriction-field :initarg :thread-restriction-field
			     :documentation
			     "Text field that contains thread restriction.")
   (suspend-policy-field     :initarg :suspend-policy-field
			     :documentation
			     "Text field that specifies the thread suspension policy.")
   (class-inclusion-field    :initarg :class-inclusion-field
			     :documentation
			     "Specifies class inclusion filters.")
   (class-exclusion-field    :initarg :class-exclusion-field
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a trace exception dialog."
)

(defmethod initialize-instance ((this jdee-bug-trace-exceptions-dialog) &rest fields)
  "Constructor for trace exceptions dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod efc-dialog-create ((this jdee-bug-trace-exceptions-dialog))

  (widget-insert (concat "Trace exception\n\n"))

  (oset this exception-class-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Exception class"
	 :doc "Name of class of exception to trace. May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to trace occurences of java.io.IOException, specify *.IOException."))

  (oset this trace-type-field
	(widget-create
	 '(choice
	   :tag "Exception type"
	   :value "both"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify the type of exception to trace."
	   (const "caught")
	   (const "uncaught")
	   (const "both"))))

  (oset this thread-restriction-field
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict trace to the specified thread."))

  (oset this suspend-policy-field
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "none"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which thread to suspend on class preparation or unloading."
	   (const "all")
	   (const "thread")
	   (const "none"))))


  (oset this class-inclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))

    (oset this class-exclusion-field
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose preparation or unloading should not be traced."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))
  )

(defmethod efc-dialog-ok ((this jdee-bug-trace-exceptions-dialog))
  (let* ((exception-class (widget-value (oref this exception-class-field)))
	 (trace-type (widget-value (oref this trace-type-field)))
	 (thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jdee-dbs-get-target-process))
	 (request (jdee-dbs-trace-exceptions-request
		   "trace exceptions request"
		   :exception-class exception-class
		   :trace-type trace-type))
	 (cmd  (jdee-dbs-trace-exceptions
		"trace exceptions command"
		:process process :trace-request request)))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))

    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jdee-dbs-cmd-exec cmd)
    (call-next-method)))

(defun jdee-bug-break-on-exception (exception-class)
  (interactive "sFully qualified exception: ")
  (let* ((process (jdee-dbs-get-target-process))
	 (request (jdee-dbs-trace-exceptions-request
		   "break on exceptions request"
		   :exception-class exception-class
		   :trace-type "both"
		   :suspend-policy "all"))
	 (cmd (jdee-dbs-trace-exceptions
	       "break on exceptions command"
	       :process process :trace-request request)))
    (jdee-dbs-cmd-exec cmd)
    (jdee-dbs-proc-display-debug-message process "Use JDEbug->Trace->Cancel to remove this breakpoint")))


(defun jdee-bug-trace-exceptions ()
  "Displays the trace exceptions dialog."
  (interactive)
  (let ((dialog (jdee-bug-trace-exceptions-dialog "trace exceptions dialog")))
    (efc-dialog-show dialog)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-cancel-trace-request (process request)
  "Cancels a specified trace request on a specified process."
  (let ((cmd (jdee-dbs-cancel-trace "cancel trace" :process process
				  :trace-request request)))
    (jdee-dbs-cmd-exec cmd)))

(defclass jdee-bug-cancel-trace-dialog (efc-dialog)
  ((process          :initarg :process
		     :type jdee-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod efc-dialog-create ((this jdee-bug-cancel-trace-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc
	       (concat
		(if (typep request 'jdee-dbs-trace-methods-request)
		    (progn
		      (concat
		      (format "Trace method %s." (oref request trace-type))
		      (if (slot-boundp request 'thread-restriction)
			  (format " Thread restriction: %s."
				  (oref request thread-restriction)))))
		  (format "Trace class %s." (oref request trace-type)))
		(if (slot-boundp request 'suspend-policy)
		    (format " Suspend policy: %s." (oref request suspend-policy)))
		(if (slot-boundp request 'inclusion-filters)
		    (format " Inclusion filters: %s." (oref request inclusion-filters)))
		(if (slot-boundp request 'exclusion-filters)
		    (format " Exclusion filters: %s." (oref request exclusion-filters)))
		)
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the trace requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod efc-dialog-ok ((this jdee-bug-cancel-trace-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr
	     (cl-find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jdee-bug-cancel-trace-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jdee-bug-cancel-trace ()
  "Cancels method and class trace requests for the target process.
If only one trace request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive)
 (let* ((process (jdee-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'trace-req)
	   (let ((trace-requests (oref process :trace-req)))
	     (if (= (length trace-requests) 1)
		 (jdee-bug-cancel-trace-request process (cdr (car trace-requests)))
	       (let ((dialog
		      (jdee-bug-cancel-trace-dialog "cancel trace dialog"
						   :process process
						   :requests trace-requests)))
		 (efc-dialog-show dialog))))
	 (error "The target process has no outstanding trace requests"))
     (error "There is no active process."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Watch Field Command                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jdee-bug-watch-field-dialog (efc-dialog)
  ((watch-type                :initarg :watch-type
			      :type string
			      :initform "access"
			      :documentation
			      "Watch type: field access or modification.")
   (object-class              :initarg :object-class
			      :type string
			      :initform ""
			      :documentation
			      "Default value of class to watch.")
   (field                     :initarg :field
			      :type string
			      :initform ""
			      :documentation
			      "Default value of the field to watch.")
   (object-class-widget       :initarg :object-class-widget
			      :documentation
			      "Widget specifying class of objects to watch.")
   (field-name-widget         :initarg :field-name-widget
			      :documentation
			      "Widget specify name of field to watch.")
   (expression-widget         :initarg :expression-widget
			      :documentation
			      "Widget specify watch restriction expression.")
   (object-id-widget          :initarg :object-id-widget
			      :documentation
			      "Widget specify id of object to watch.")
   (thread-restriction-widget :initarg :thread-restriction-widget
			      :documentation
			      "Text field that contains thread restriction.")
   (suspend-policy-widget     :initarg :suspend-policy-widget
			      :documentation
			      "Text field that specifies the thread suspension policy.")
   (class-inclusion-widget    :initarg :class-inclusion-widget
			      :documentation
			     "Specifies class inclusion filters.")
   (class-exclusion-widget    :initarg :class-exclusion-widget
			     :documentation
			     "Specifies class exclusion filters.")
   )
  "Defines a watch field dialog."
)

(defmethod initialize-instance ((this jdee-bug-watch-field-dialog) &rest fields)
  "Constructor for watch field dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod efc-dialog-create ((this jdee-bug-watch-field-dialog))

  (widget-insert (format "Watch for field %s\n\n" (oref this watch-type)))

  (oset this object-class-widget
	(widget-create
	 'editable-field
	 :value (oref this object-class)
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Class"
	 :doc "Class of object or objects to watch.
May be a wild card pattern of the form *.name. This allows you to omit a package qualifier from a class name. For example, to watch a field of java.io.IOException, specify *.IOException."))


  (oset this field-name-widget
	(widget-create
	 'editable-field
	 :value (oref this field)
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Field name"
	 :doc "Name of field to watch."))

  (oset this expression-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Watch expression"
	 :doc "A boolean expression.
Execution of the process is suspended only if the expression is true. The expression can contain any variable that is in scope when a field changes."))

  (oset this object-id-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Object ID"
	 :doc "ID of the object to watch."))

  (oset this thread-restriction-widget
	(widget-create
	 'editable-field
	 :format "  %t:  %v\n  %h \n\n"
	 :size 40
	 :tag "Thread restriction"
	 :doc "Restrict watch to the specified thread."))

  (oset this suspend-policy-widget
	(widget-create
	 '(choice
	   :tag "Thread Suspension Policy"
	   :value "all"
	   :format "  %t: %[Options%] %v  %h\n\n"
	   :doc "Specify which threads to suspend on field access or modification."
	   (const "all")
	   (const "thread")
	   (const "none"))))


  (oset this class-inclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class inclusion filters"
	   :doc "Regular expressions that specify classes whose field should be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter"))))

    (oset this class-exclusion-widget
	(widget-create
	 '(editable-list
	   :format "  %t:\n  %i\n%v  %h\n\n"
	   :entry-format "  %i %d %v\n"
	   :tag "Class exclusion filters"
	   :doc "Regular expressions that specify classes whose fields should not be watched."
	   (string
	    :format "%t: %v"
	    :size 40
	    :tag "Filter")))))

(defmethod efc-dialog-ok ((this jdee-bug-watch-field-dialog))
  (let* ((obj-class (widget-value (oref this object-class-widget)))
	 (field-name (widget-value (oref this field-name-widget)))
	 (expression (widget-value (oref this expression-widget)))
	 (object-id (widget-value (oref this object-id-widget)))
	 (thread-restriction (widget-value (oref this thread-restriction-widget)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-widget)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (process (jdee-dbs-get-target-process))
	 (request (jdee-dbs-watch-field-request
		   "watch field request"
		   :watch-type (oref this watch-type)
		   :object-class obj-class
		   :field-name field-name))
	 (cmd  (jdee-dbs-watch-field
		"watch field command"
		:process process :watch-request request)))

    (if (and expression (not (string= expression "")))
	(oset request :expression expression))

    (if (and object-id (not (string= object-id "")))
	(oset request :object-id object-id))

    (if (and thread-restriction (not (string= thread-restriction "")))
	(oset request :thread-restriction thread-restriction))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))

    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jdee-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jdee-bug-watch-field-access ()
  "Request that the debugger watch for access of a
field of an object or class of objects."
  (interactive)
  (let ((dialog
	 (jdee-bug-watch-field-dialog
	  "watch field dialog"
	  :object-class (concat
			 "*."
			 (car (jdee-parse-get-innermost-class-at-point)))
	  :field (thing-at-point 'symbol))))
    (efc-dialog-show dialog)))

(defun jdee-bug-watch-field-modification ()
  "Request that the debugger watch for modifiction of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (jdee-bug-watch-field-dialog
		 "watch field dialog"
		 :watch-type "modification"
		 :object-class (concat
				"*."
				(car (jdee-parse-get-innermost-class-at-point)))
		 :field (thing-at-point 'symbol))))
    (efc-dialog-show dialog)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-cancel-watch-request (process request)
  "Cancels a specified watch field request on a specified process."
  (let ((cmd (jdee-dbs-cancel-watch "cancel watch" :process process
				  :watch-request request)))
    (jdee-dbs-cmd-exec cmd)))

(defclass jdee-bug-cancel-watch-dialog (efc-dialog)
  ((process          :initarg :process
		     :type jdee-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod efc-dialog-create ((this jdee-bug-cancel-watch-dialog))
  (let ((items
	 (mapcar
	  (lambda (x)
	    (let ((request (cdr x)))
	      (list
	       'const
	       :format "%t %v  %d"
	       :tag "Request"
	       :doc
	       (concat
		(format "Watch type: %s. Class: %s. Field: %s."
		       (oref request watch-type)
		       (oref request object-class)
		       (oref request field-name))
		(if (slot-boundp request 'object-id)
		    (concat " Object id: " (oref request object-id) "."))
		(if (slot-boundp request 'expression)
		    (concat " Expression: " (oref request expression) ".")))
	       (car x))))
	  (oref this requests))))

  (widget-insert "Check the watch requests you want to cancel.\n\n")

  (oset this check-boxes
	(widget-create
	 (list
	  'checklist
	  :entry-format "  %b %v\n"
	  :args items
	   )))
  ))

(defmethod efc-dialog-ok ((this jdee-bug-cancel-watch-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr
	     (cl-find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jdee-bug-cancel-watch-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jdee-bug-cancel-watch ()
  "Cancels watch requests for the target process.
If only one watch request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive)
 (let* ((process (jdee-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'watch-req)
	   (let ((watch-requests (oref process :watch-req)))
	     (if (= (length watch-requests) 1)
		 (jdee-bug-cancel-watch-request process (cdr (car watch-requests)))
	       (let ((dialog
		      (jdee-bug-cancel-watch-dialog "cancel watch dialog"
						   :process process
						   :requests watch-requests)))
		 (efc-dialog-show dialog))))
	 (error "The target process has no outstanding watch requests"))
     (error "There is no active process."))))

(defun jdee-make-frame-names-alist ()
  (let* ((current-frame (selected-frame))
	 (falist
	  (cons
	   (cons (frame-parameter current-frame 'name)
		 current-frame) nil))
	 (frame (next-frame nil t)))
    (while (not (eq frame current-frame))
      (progn
	(setq falist (cons (cons
			    (frame-parameter frame 'name)
			    frame) falist))
	(setq frame (next-frame frame t))))
    falist))

(defun jdee-bug-show-debug-frame ()
  "Show or open a new frame with the locals, thread, and CLI buffer."
  (interactive)
  (let* ((existing-frame (cdr (assoc "JDebug" (jdee-make-frame-names-alist)))))
    (if existing-frame
        (progn
          (make-frame-visible existing-frame)
          (raise-frame existing-frame)
          (select-frame existing-frame))
      (let* ((process (jdee-dbs-get-target-process))
	     (cli-buffer (when (slot-boundp process 'cli-buf)
                           (oref process cli-buf)))
	     (locals-buffer (when (slot-boundp process 'locals-buf)
                              (oref process locals-buf)))
	     (threads-buffer (when (slot-boundp process 'threads-buf)
                               (oref process threads-buf)))
	     (msg-buffer (when (slot-boundp process 'msg-buf)
                           (oref process msg-buf)))
	     (frame (make-frame '((name . "JDebug") (minibuffer . nil))))
	     (height (/ (frame-height frame)
			(cl-count-if 'identity
				     (list cli-buffer
					   locals-buffer
					   threads-buffer
					   msg-buffer))))
	     (init-frame (selected-frame))
	     (init-config (current-window-configuration))
	     (prev-window))
	(save-excursion
	  (select-frame frame)
	  ;; msg buffer should always be there, if not this could break
	  (switch-to-buffer msg-buffer)
	  (setq prev-window (get-buffer-window msg-buffer))
	  (when locals-buffer
	    (let ((locals-win (split-window prev-window height)))
	      (setq prev-window locals-win)
	      (when (not jdee-bug-local-variables)
		(jdee-bug-toggle-local-variables)
		(sleep-for 0 100))
	      (set-window-buffer locals-win locals-buffer)))
	  (when threads-buffer
	    (let ((threads-win (split-window prev-window height)))
	      (setq prev-window threads-win)
	      (jdee-bug-show-threads)
	      (set-window-buffer threads-win threads-buffer)))
	  (when cli-buffer
	    (let ((cli-win (split-window prev-window height)))
	      (set-window-buffer cli-win cli-buffer)))
	  (select-frame init-frame)
	  (set-window-configuration init-config))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Variable Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-display-variable ()
  (interactive)
  (if (not (jdee-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
  ;; Previously we used semantic. I recently have been having problems
  ;; with it, so I just replaced it with word-at-point
  (let ((qualified-expr (thing-at-point 'word)))
    (jdee-bug-evaluate-expression qualified-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Expression Command                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-evaluate-expression (expression)
"Evaluates a Java expression. The Java expression may include
any variables in scope in the program being debugged."
  (interactive
   "sExpression: ")

  (if (not (jdee-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (if  (string= expression "")
      (error "Empty expression."))

  (let* ((process (jdee-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (evaluate-command
	  (jdee-dbs-evaluate
	   (format "Evaluate %s" expression)
	   :process process
	   :expression expression
	   :thread-id thread-id))
	 (result
	  (jdee-dbs-cmd-exec evaluate-command)))
    (if result
	(let* ((type  (nth 0 result))
	       (value (nth 1 result))
	       (buf (get-buffer-create (concat "Expression: " expression))))
	    (jdee-dbo-view-var-in-buf (jdee-dbs-objectify-value result)
				     expression process t buf t)
	    (pop-to-buffer buf (split-window nil (- (window-height) 4))))
      (message "Error: could not evaluate \"%s\"." expression))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Loaded Classes Command                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-display-loaded-classes ()
  "Displays the classes currently loaded by the target process."
  (interactive)

  (if (not (jdee-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-get-loaded-classes
	   "get_loaded_classes"
	   :process process))
	 (result
	  (jdee-dbs-cmd-exec cmd)))
    (if (not result)
      (error "Could not get loaded classes."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Threads Command                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-show-threads ()
  "Shows all threads and thread-groups running in the target process.
This command displays the threads as a tree structure. To expand
a node of the tree, click the + sign next to the node, using mouse
button 2."
  (interactive)

  (if (not (jdee-dbs-get-target-process))
      (error "No target process."))

  (if (not jdee-bug-stack-info)
      (progn
	(jdee-bug-toggle-stack-info)
	(sleep-for 0 100)))

  (let* ((process (jdee-dbs-get-target-process))
	 (get-threads-command
	  (jdee-dbs-get-threads
	   "get_threads"
	   :process process))
	 (result
	  (jdee-dbs-cmd-exec get-threads-command)))
    (if (not result)
        (error "Could not get threads"))))

(defun jdee-bug-thread-show-thread-info ()
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Object Monitors                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-show-object-monitors (object-id)
"Shows the threads that are monitoring a specified object, including the thread
that currently owns the object and threads that are waiting to access the object."
  (interactive
   "nObject ID: ")

  (if (not (jdee-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jdee-dbs-get-target-process))
	 (get-monitors-command
	  (jdee-dbs-get-object-monitors
	   "get_object_monitors"
	   :process process :object-id object-id))
	 (result
	  (jdee-dbs-cmd-exec get-monitors-command)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Path Info Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-display-path-info ()
  "Displays the base directory, boot classpath, and classpath of the target process."
  (interactive)

  (if (not (jdee-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jdee-dbs-get-target-process))
	 (cmd
	  (jdee-dbs-get-path-info
	   "get_path_info"
	   :process process))
	 (result
	  (jdee-dbs-cmd-exec cmd)))
    (if (not result)
      (error "Could not get path info."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Watchpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-clear-watchpoint ()
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Up Stack Command                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-up-stack ()
  "Moves the source cursor up one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jdee-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not
       (let* ((process (jdee-dbs-get-target-process))
	      (stack-max (1- (length (oref process stack))))
	      (stack-ptr (oref process stack-ptr)))
	 (< stack-ptr stack-max)))
      (error "The debugger is displaying the top of the stack."))

  (let* ((process (jdee-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1+ (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))

    (oset process :stack-ptr stack-ptr)
    (jdee-db-set-debug-cursor class file line)
    (jdee-dbo-update-locals-buf process thread-id stack-ptr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Down Stack Command                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-down-stack ()
  "Moves the source cursor down one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jdee-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not (let* ((process (jdee-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))
      (error "The debugger is displaying the bottom of the stack."))

  (let* ((process (jdee-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1- (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))

    (oset process :stack-ptr stack-ptr)
    (jdee-db-set-debug-cursor class file line)
    (jdee-dbo-update-locals-buf process thread-id stack-ptr)))



(defun jdee-bug-suspend-thread (thread-id)
"Suspends the thread or group of threads specified by THREAD-ID.
If the thread or group is already suspended, this command increments
the thread's suspend count. Use JDEBug->Threads->Show Threads (`jdee-bug-thread-show-threads')
to display the IDs of all threads and thread groups running in the
target process. Use JDEBug->Processes->Suspend Process
(`jdee-bug-suspend-process') to suspend the entire process. Use
Threads->Resume Thread (`jdee-bug-resume-thread') to resume the thread."
  (interactive
   "nThread ID: ")
  (let* ((process (jdee-dbs-get-target-process))
	 (suspend-command
	  (jdee-dbs-suspend-thread
	       (format "suspend thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jdee-dbs-cmd-exec suspend-command)))


(defun jdee-bug-resume-thread (thread-id)
"Resumes the previously suspended thread or group of threads specified
by THREAD-ID.  This command has no effect if the specified thread or
thread-group is running or was not suspended by you, using the
JDEBug->Threads->Suspend Thread command (`jdee-bug-suspend-thread').
If you suspended the thread more than once, this command reduces the
suspend count by 1. The thread resumes only when the suspend count
reaches 0. Use JDEBug->Threads->Show Threads
(`jdee-bug-thread-show-threads') to display the IDs of all threads and
thread groups running in the target process. Use
JDEBug->Processes->Resume Process (`jdee-bug-resume-process') to resume
the entire process."
  (interactive
   "nThread ID: ")
  (let* ((process (jdee-dbs-get-target-process))
	 (resume-command
	  (jdee-dbs-resume-thread
	       (format "resume thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jdee-dbs-cmd-exec resume-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-interrupt-thread (thread-id)
"Interrupts the thread specified by THREAD-ID. The thread cannot be
resumed. Use JDEBug->Threads->Show Threads
(`jdee-bug-thread-show-threads') to display the IDs of all threads
running in the target process. Use Threads->Suspend Thread
(`jdee-bug-suspend-thread') to suspend a thread temporarily."
  (interactive
   "nThread ID: ")
  (let* ((process (jdee-dbs-get-target-process))
	 (interrupt-command
	  (jdee-dbs-interrupt-thread
	       (format "interrupt thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jdee-dbs-cmd-exec interrupt-command)))


(defun jdee-bug-stop-thread (thread-id exception-id)
"Stops a thread and throws an exception. THREAD-ID is the id of the thread you want
to stop. EXCEPTION-ID is the id of the exception object you want to throw. Use
JDEBug->Threads->Show Threads (`jdee-bug-thread-show-threads') to display the IDs of
all threads and thread groups running in the target process. Use JDEBug->Evaluate Expression
to creae the exception object."
 (interactive
   "nThread ID: \nnException Id: ")
  (let* ((process (jdee-dbs-get-target-process))
	 (stop-command
	  (jdee-dbs-stop-thread
	       (format "stop thread %d" thread-id)
	       :process process
	       :thread-id thread-id
	       :exception-id exception-id)))
    (jdee-dbs-cmd-exec stop-command)))

(defun jdee-bug-jpda-installed-p ()
  "Returns t if the jpda is installed."
  (interactive)
  (cond
   ((jdee-bug-vm-includes-jpda-p)
    t)
   ((string= jdee-bug-jpda-directory "")
    (error "jdee-bug-jpda-directory variable is not set.")
    nil)
   ((not (file-exists-p
	  (expand-file-name "lib/jpda.jar" (jdee-normalize-path
					    'jdee-bug-jpda-directory))))
    (error "Cannot find JPDA jar file at %s"
	     (expand-file-name "lib/jpda.jar" jdee-bug-jpda-directory))
    nil)
   (t
    t)))


(defun jdee-bug-start-debugger ()
  "Starts the debugger."
  (interactive)
  (if (not (jdee-dbs-debugger-running-p))
      (if (and (jdee-bug-jpda-installed-p)
	   (jdee-dbs-debugger-start jdee-dbs-the-debugger))
      (message "Debugger started successfully." )
      (message "Could not start debugger."))
    (message "Debugger is already started")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-launch-process ()
  "Starts a virtual machine to run the application
in the current source buffer. Halts at the beginning
of the process to let you set breakpoints. The started
process becomes the target process for debugger
commands. Select Processes->Set Target Process from the JDEBug
menu or run the `jdee-bug-set-target-process' command
to set another process as the target process."
  (interactive)
  (let* ((main-class (jdee-run-get-main-class)))
    (unless (and
	     (jdee-dbs-proc-set-find jdee-dbs-the-process-registry
				    :main-class main-class)
	     (not (yes-or-no-p
		   (format "An instance of %s is already running. Continue?" main-class))))
      (let* ((process
	      (jdee-dbs-proc (format "process%d"
				    (setq jdee-dbs-proc-counter
					  (1+ jdee-dbs-proc-counter)))
			    :id jdee-dbs-proc-counter :main-class main-class))
	     (old-target (jdee-dbs-get-target-process))
	     (launch (jdee-dbs-launch-process
		      (format "Launch %s" main-class)
		      :process process
		      :vmexec (car jdee-bug-vm-executable)
		      ;; :vmexec "xyz"
		      ))
	     (succeededp t))
	(jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
	(if (not (string= jdee-bug-jre-home ""))
	    (oset launch :jre-home (jdee-normalize-path 'jdee-bug-jre-home)))
	(oset jdee-dbs-the-process-registry :target-process process)
	(when (not (jdee-dbs-cmd-exec launch))
	  (jdee-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jdee-dbs-the-process-registry :target-process old-target))
	  (jdee-dbs-proc-set-state process "unknown")
	  (jdee-dbs-proc-set-state-reason process "Error launching process.")
	  (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
	  (setq succeededp nil))
	succeededp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Local Process Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-bug-attach-via-shared-memory (process-name)
  "Attaches the debugger to a process running on the same machine via shared
memory. This command works only on Windows."
  (interactive
   "sProcess name: ")
  (let* ((process
	  (jdee-dbs-proc (format "process%d"
				(setq jdee-dbs-proc-counter
				      (1+ jdee-dbs-proc-counter)))
			:id jdee-dbs-proc-counter :main-class process-name))
	 (old-target (jdee-dbs-get-target-process))
	 (attach (jdee-dbs-attach-shmem
		  (format "Attach %s" process-name)
		  :process process
		  :process-name process-name)))
    (jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
    (oset jdee-dbs-the-process-registry :target-process process)
    (if (not (jdee-dbs-cmd-exec attach))
	(progn
	  (jdee-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jdee-dbs-the-process-registry :target-process old-target))
	  (jdee-dbs-proc-set-state process "unknown")
	  (jdee-dbs-proc-set-state-reason process "Error launching process.")
	  (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
	  nil)
      (jdee-bug-set-breakpoints process jdee-db-breakpoints))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Local Host Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jdee-bug-attach-local-host (process-port)
  "Attaches the debugger to a process running on local host. This command connects
to the process via a socket."
  (interactive
   "sProcess Port: ")
  (let* ((process
	  (jdee-dbs-proc (format "process%d"
				(setq jdee-dbs-proc-counter
				      (1+ jdee-dbs-proc-counter)))
			:id jdee-dbs-proc-counter :main-class process-port))
	     (old-target (jdee-dbs-get-target-process))
	     (attach (jdee-dbs-attach-socket
		      (format "Attach %s" process-port)
		      :process process
		      :port process-port)))
    (jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
    (oset jdee-dbs-the-process-registry :target-process process)
    (if (not (jdee-dbs-cmd-exec attach))
	(progn
	  (jdee-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jdee-dbs-the-process-registry :target-process old-target))
	  (jdee-dbs-proc-set-state process "unknown")
	  (jdee-dbs-proc-set-state-reason process "Error launching process.")
	  (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
	  nil)
      (jdee-bug-set-breakpoints process jdee-db-breakpoints))
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Remote Host Command                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jdee-bug-attach-remote-host (process-host process-port)
  "Attaches the debugger to a process running on a remote host. This command connects
to the process via a socket."
 (interactive
   "sHost: \nsProcess Port: ")
  (let* ((process
	  (jdee-dbs-proc (format "process%d"
				(setq jdee-dbs-proc-counter
				      (1+ jdee-dbs-proc-counter)))
			:id jdee-dbs-proc-counter :main-class process-port))
	     (old-target (jdee-dbs-get-target-process))
	     (attach (jdee-dbs-attach-socket
		      (format "Attach %s" process-port)
		      :process process
		      :host process-host
		      :port process-port)))
    (jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
    (oset jdee-dbs-the-process-registry :target-process process)
    (if (not (jdee-dbs-cmd-exec attach))
	(progn
	  (jdee-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jdee-dbs-the-process-registry :target-process old-target))
	  (jdee-dbs-proc-set-state process "unknown")
	  (jdee-dbs-proc-set-state-reason process "Error launching process.")
	  (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
	  nil)
      (jdee-bug-set-breakpoints process jdee-db-breakpoints))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Shared Memory Command                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jdee-bug-server-shmem-name-history nil
  "History of shared memory names for debugger.")

(defun jdee-bug-listen-shmem (shmem-name)
  "Listens on shared memory for a vm requiring debugging services."
  (interactive
   (list
    (if (car jdee-bug-server-shmem-name)
	(read-from-minibuffer "Name: "
			      (car jdee-bug-server-shmem-name-history)
			      nil nil
			      'jdee-bug-server-shmem-name-history)
      (cdr jdee-bug-server-shmem-name))))
  (let* ((process
	  (jdee-dbs-proc (format "process%d"
				(setq jdee-dbs-proc-counter
				      (1+ jdee-dbs-proc-counter)))
			:id jdee-dbs-proc-counter :main-class shmem-name))
	     (old-target (jdee-dbs-get-target-process))
	     (listen (jdee-dbs-listen-for-process
		      (format "Listen %s" shmem-name)
		      :process process
		      :address shmem-name)))
    (jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
    (oset jdee-dbs-the-process-registry :target-process process)
    (when (not (jdee-dbs-cmd-exec listen))
      (jdee-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jdee-dbs-the-process-registry :target-process old-target))
      (jdee-dbs-proc-set-state process "unknown")
      (jdee-dbs-proc-set-state-reason process "Error listening for process.")
      (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Socket Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jdee-bug-server-socket-history nil
  "History of sockets used by debugger to listen for debuggee vms.")

(defun jdee-bug-listen-socket (socket)
  "Listens on socket for a vm requiring debugging services.
If `jdee-bug-server-socket' is set to \"prompt for,\" this command
prompts you to enter the socket on which to listen. Otherwise, it
listens on the socket specified by `jdee-bug-server-socket'."
  (interactive
   (list
    (if (car jdee-bug-server-socket)
	(read-from-minibuffer "Socket: "
			      (car jdee-bug-server-socket-history)
			      nil nil
			      'jdee-bug-server-socket-history)
      (cdr jdee-bug-server-socket))))
  (let* ((process
	  (jdee-dbs-proc (format "process%d"
				(setq jdee-dbs-proc-counter
				      (1+ jdee-dbs-proc-counter)))
			:id jdee-dbs-proc-counter :main-class socket))
	     (old-target (jdee-dbs-get-target-process))
	     (listen (jdee-dbs-listen-for-process
		      (format "Listen %s" socket)
		      :process process
		      :address socket
		      :transport "socket")))
    (jdee-dbs-proc-set-add jdee-dbs-the-process-registry process)
    (oset jdee-dbs-the-process-registry :target-process process)
    (when (not (jdee-dbs-cmd-exec listen))
      (jdee-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jdee-dbs-the-process-registry :target-process old-target))
      (jdee-dbs-proc-set-state process "unknown")
      (jdee-dbs-proc-set-state-reason process "Error listening for process.")
      (jdee-dbs-proc-set-add jdee-dbs-the-process-morgue process)
      nil)
    t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Detach Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jdee-bug-detach-process ()
  "Detaches the debugger from the target process. The target process continues
to run."
  (interactive)
  (jdee-bug-finish-process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Process Command                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jdee-bug-suspend-process ()
"Suspends the target process. To suspend a particular thread or thread group,
use JDEbug->Threads->Suspend Thread (`jdee-bug-suspend-thread')."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (suspend-command
	  (jdee-dbs-suspend-thread "suspend process" :process process)))
    (jdee-dbs-cmd-exec suspend-command)))


(defun jdee-bug-resume-process ()
"Resumes the target process. To resume a particular thread or thread group,
use JDEbug->Threads->Resume Thread (`jdee-bug-resume-thread')."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (resume-command
	  (jdee-dbs-resume-thread "resume process" :process process)))
    (jdee-dbs-cmd-exec resume-command)))


(defun jdee-bug-finish-process ()
  "Terminates the target process."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	 (finish (jdee-dbs-finish-process
		  (format "finish %d" (oref process id))
		  :process process))
	 (result (jdee-dbs-cmd-exec finish)))
    (jdee-dbs-proc-move-to-morgue process)
    (slot-makeunbound jdee-dbs-the-process-registry :target-process)
    (jdee-dbs-proc-set-state-reason process "finish")))

(defun jdee-bug-set-target-process (process-id)
  "Sets the process whose process-id is PROCESS-ID to be
the focus of debugger commands."
  (interactive
   "nEnter process id: ")
  (jdee-dbs-proc-registry-set-target-proc
   jdee-dbs-the-process-registry process-id))


(defun jdee-bug-show-processes ()
  (interactive)
  (message "not implemented"))


(defun jdee-bug-remove-dead-processes ()
  "Remove dead processes and their associated buffers from the Emacs environment."
  (interactive)
  (if (oref jdee-dbs-the-process-morgue proc-alist)
      (jdee-dbs-proc-morgue-bury-the-dead jdee-dbs-the-process-morgue)))


(defun jdee-bug-show-locals-buf ()
  "Show the local variables buffer of the target process.
This command splits the window and shows the locals buffer."
  (interactive)
  (if (not jdee-bug-local-variables)
      (progn
	(jdee-bug-toggle-local-variables)
	(sleep-for 0 100)))

  (let* ((process (jdee-dbs-get-target-process))
	 (locals-buf (oref process locals-buf))
	 (source-window (selected-window))
	 locals-window)
    (if (one-window-p)
	(progn
	  (setq locals-window (split-window source-window))
	  (set-window-buffer locals-window locals-buf)))
    (set-window-buffer (next-window source-window) locals-buf)
    (select-window source-window)))

(defun jdee-bug-show-cli-buf ()
  "Show the command-line interface (CLI) buffer of the target process."
  (interactive)
  (let* ((process (jdee-dbs-get-target-process))
	(cli-buf (oref process cli-buf))
	(source-window (selected-window))
	cli-window)
    (if (one-window-p)
	(progn
	  (setq cli-window (split-window source-window))
	  (set-window-buffer cli-window cli-buf)))
    (set-window-buffer (next-window source-window) cli-buf)
    (select-window source-window)))


(defun jdee-bug-show-threads-buf ()
  "Show the threads buffer of the target process."
  (interactive)

  ;;Updating the threads buffer
  (jdee-bug-show-threads)

  (let* ((process (jdee-dbs-get-target-process))
	 (threads-buf (oref process threads-buf))
	 (source-window (selected-window))
	 threads-window)
    (if (one-window-p)
	(progn
	  (setq threads-window (split-window source-window))
	  (set-window-buffer threads-window threads-buf)))
    (set-window-buffer (next-window source-window) threads-buf)
    (select-window source-window)))


(defun jdee-bug-show-preferences ()
  (interactive)
  (customize-apropos "jdee-bug" 'groups))


(defun jdee-bug-set-breakpoints (process breakpoints)
  "Sets BREAKPOINTS in PROCESS."
  (mapc
   (lambda (assoc-x)
     (let* ((breakpoint (cdr assoc-x))
	    (set-breakpoint (jdee-dbs-set-breakpoint
			     (format "set breakpoint%d"
				     (oref breakpoint id))
			     :process process
			     :breakpoint breakpoint))
	    (result (jdee-dbs-cmd-exec set-breakpoint)))))
   breakpoints))

;;;###autoload
(defun jdee-bug-debug-app ()
  "Runs the debugger on the application in the current source buffer."
  (interactive)
  (if (and
       (jdee-bug-jpda-installed-p)
       (not (jdee-dbs-debugger-running-p)))
      (jdee-dbs-debugger-start jdee-dbs-the-debugger))
  (if (jdee-dbs-debugger-running-p)
      (let ((result (jdee-bug-launch-process)))
	(if result
	    (let ((process (oref jdee-dbs-the-process-registry :target-process)))
	      (jdee-bug-set-breakpoints process jdee-db-breakpoints)
	      (setq result (jdee-bug-continue)))))))

(defun jdee-bug-help ()
  "Displays the JDEbug User's Guide."
  (interactive)
  (let* ((jdee-dir (jdee-find-jdee-doc-directory))
	 (jdebug-help
	  (if jdee-dir
	      (expand-file-name "doc/html/jdebug-ug/jdebug-ug.html" jdee-dir))))
    (if (and
	 jdebug-help
	 (file-exists-p jdebug-help))
	(browse-url (concat "file://" (jdee-convert-cygwin-path jdebug-help)))
      (signal 'error '("Cannot find JDEbug User's Guide.")))))

(defun jdee-bug-keys ()
  "Displays JDEbug keybindings. Use `jdee-keys' to display JDE keybindings."
  (interactive)
  (jdee-describe-map 'jdee-bug-keymap))

(defun jdee-bug-toggle-local-variables ()
  "Enables and disables the retrieval of the local variables. It toggles
the value of the variable `jdee-bug-local-variables'"
  (interactive)
  (setq jdee-bug-local-variables (not jdee-bug-local-variables))
  (if (and jdee-dbo-current-process
	   jdee-dbo-current-thread-id
	   jdee-bug-local-variables)
      (jdee-dbo-update-locals-buf jdee-dbo-current-process
				 jdee-dbo-current-thread-id 0)
    (save-excursion
      (if jdee-dbo-current-process
	  (progn
	    (set-buffer (oref jdee-dbo-current-process locals-buf))
	    (kill-all-local-variables))))))

(defun jdee-bug-toggle-stack-info ()
  "Enables and disables the retrieval of stack info. It toggles the value
of the variable `jdee-bug-stack-info'"
  (interactive)
  (setq jdee-bug-stack-info (not jdee-bug-stack-info))
  (if (and jdee-dbo-current-process
	   jdee-dbo-current-thread-id
	   jdee-bug-stack-info)
      (jdee-dbo-update-stack jdee-dbo-current-process
			    jdee-dbo-current-thread-id)
    (save-excursion
      (if jdee-dbo-current-process
	  (progn
	    (set-buffer (oref jdee-dbo-current-process threads-buf))
	    (kill-all-local-variables)
	    (erase-buffer))))))

(provide 'jdee-bug)

;;; jdee-bug.el ends here
