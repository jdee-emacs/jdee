;;; jde-bug.el -- JDEbug Interface
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
;; The latest version of the JDE is available at
;; <URL:http://jdee.sourceforge.net/>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

(require 'cl)
(require 'jde-parse)
(require 'jde-dbs)
(require 'jde-dbo)
(require 'jde-db)
(require 'efc)


(defgroup jde-bug nil
  "JDEbug General Options"
  :group 'jde
  :prefix "jde-bug")


(defcustom jde-bug-debug nil
"*IMPORTANT!!!! Leave this switch in its default state (off) unless
you need to modify the *JDEbug* Java source code. Setting this switch
on causes the JDE to load *JDEbug* from its java/classes directory
instead of from jde.jar. It also causes the JDE to run the debugger in
debug server mode. This allows you to use *JDEbug* to debug itself."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-jpda-directory ""
  "*Pathname of the directory containing Sun's Java Platform Debug Architecture
distribution. You need to set this variable only if this project uses a JDK 1.2 vm."
  :group 'jde-bug
  :type 'file)


(defun jde-bug-vm-includes-jpda-p ()
  "Returns t if the current JDK provides built-in support for JPDA."
  (or
   (> (jde-java-major-version) 1)
   (> (jde-java-minor-version) 2)))


(defcustom jde-bug-jre-home ""
"*Home directory of the JRE containing the executable used to
run debuggee processes.
This variable specifies the home directory of the Java runtime
environment containing the executable, e.g., java, to be used to
launch processes (see `jde-bug-vm-executable'). If you do not specify
a home directory, the home directory is the same as that of the
executable used to run the debugger itself."
  :group 'jde-bug :type 'string)


(defcustom jde-bug-vm-executable (list (if (eq system-type 'windows-nt) "javaw" "java"))
  "*Name of the executable used to launch target processes.
This defaults to java on Unix platforms and javaw on Windows platforms"
  :group 'jde-bug
  :type '(list
	  (radio-button-choice
	  (const "java")
	  (const "javaw")
	  (const "java_g"))))

(defcustom jde-bug-raise-frame-p t
  "*Raise frame when a breakpoint is hit."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-server-socket (cons t "2112")
  "*Socket where debugger listens for apps needing debugger services.
You can arrange for a vm to connect to JDEbug via a socket by starting it with the
options -Xdebug and -Xrunjdwp:transport=dt_socket,address=MYHOST:NNNN,
where MYHOST is the name of the machine running the debugger and
NNNN is the socket specified by this variable. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE."
  :group 'jde-bug
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Address")))

(defcustom jde-bug-server-shmem-name (cons t "JDEbug")
  "*Shared memory name under which the debugger listens for apps
needing debugger services. To connect via shared
memory, start the debuggee process with the options -Xdebug and
-Xrunjdwp:transport=dt_shmem,address=ANYSTRING, where ANYSTRING is
the value of this variable. If you are running JDK 1.2, you must also
specifiy the options  -Xnoagent and -Djava.compiler=NONE."
  :group 'jde-bug
  :type '(cons
	  (boolean :tag "Prompt for")
	  (string :tag "Name")))


(defcustom jde-bug-debugger-host-address (if jde-xemacsp (system-name) system-name)
  "*Address of system on which JDEbug is running.
The default value is the value of the standard Emacs variable `system-name'.
The JDE uses the host address to connect to JDEBug during startup. On some Windows
systems, the JDE is unable to connect to the debugger socket under the system name.
If this happens, you can try setting this variable to the absolute address of
a local host: 127.0.0.1 ."
  :group 'jde-bug
  :type 'string)


(defcustom jde-bug-debugger-command-timeout 30
  "*Length of time in seconds the JDE waits for a response from the debugger to a command."
  :group 'jde-bug
  :type 'integer)

(defcustom jde-bug-saved-breakpoints nil
"*Breakpoints to be set for the current project."
  :group 'jde-bug
  :type '(repeat
	  (cons :tag "Break at"
	   (string :tag "File Name")
	   (integer :tag "Line Number"))))


(defcustom jde-bug-breakpoint-cursor-colors (cons "cyan" "brown")
"*Specifies the foreground and background colors of the debugger's
breakpoint cursor."
  :group 'jde-bug
  :type '(cons
	  (string :tag "Foreground Color")
	  (string :tag "Background Color"))
  :set '(lambda (sym val)
	  (make-face 'jde-bug-breakpoint-cursor)
	  (set-face-foreground 'jde-bug-breakpoint-cursor (car val))
	  (set-face-background 'jde-bug-breakpoint-cursor (cdr val))
	  (set-default sym val)))


(defgroup jde-bug-window nil
  "JDEbug Window Preferences"
  :group 'jde-bug
  :prefix "jde-bug-window")

(defcustom jde-bug-window-message nil
  "Message buffer window preferences."
  :group 'jde-bug-window
  :type 'list)

(defcustom jde-bug-local-variables nil "A non nil values makes the JDEBug
retrieve the local variables after every step command. It is recommended to
enable this feature just when is needed since retrieving the local variables
is time consuming and slow down stepping through the code."
  :group 'jde-bug
  :type 'boolean)

(defcustom jde-bug-stack-info nil "A non nil values makes the JDEBug retrieve
the stack info every step command. It is recommended to enable
this feature just when is needed since retrieving the stack info is time
consuming and slows down stepping through the code."
  :group 'jde-bug
  :type 'boolean)

(defvar jde-bug-minor-mode-hook nil
  "Hook to run when entering or leaving jde-bug-minor-mode.")

(defvar jde-bug-menu-spec
  (list "JDEbug"

	["Step Over"
	 jde-bug-step-over
	 (jde-dbs-target-process-steppable-p)]

	["Step Into"
	 jde-bug-step-into
	 (jde-dbs-target-process-steppable-p)]

	["Step Into All"
	 jde-bug-step-into-all
	 (jde-dbs-target-process-steppable-p)]

	["Step Out"
	 jde-bug-step-out
	 (jde-dbs-target-process-steppable-p)]

	["Run"
	 jde-debug
	 :active                    t
	 :included                  (null (jde-dbs-debugger-running-p)) ]

	["Continue"
	 jde-bug-continue
	 :active                    (jde-dbs-target-process-runnable-p)
	 :included                  (jde-dbs-debugger-running-p)        ]

	["Exit Debugger"
	 jde-bug-exit
	 (jde-dbs-debugger-running-p)]

	"-"
	;; Added by lea
	["Toggle Breakpoint"
	 jde-bug-toggle-breakpoint t]
	["Set Conditional Breakpoint"
	 jde-bug-set-conditional-breakpoint nil]
	["Break on exception"
	 jde-bug-break-on-exception
	 :active (and
		  (jde-dbs-debugger-running-p)
		  (jde-dbs-get-target-process))]
	["Save Breakpoints"
	 jde-bug-save-breakpoints nil]
	(list
	 "Watch for Field"

	 ["Access"
	  jde-bug-watch-field-access
	  :style    nil
	  :active   (and
		     (jde-dbs-debugger-running-p)
		     (jde-dbs-get-target-process))]


	 ["Modification"
	  jde-bug-watch-field-modification
	  :style   nil
	  :active  (and
		    (jde-dbs-debugger-running-p)
		    (jde-dbs-get-target-process))]

	 ["Cancel"
	  jde-bug-cancel-watch
	  :style     nil
	  :active    (and
		      (jde-dbs-debugger-running-p)
		      (jde-dbs-get-target-process)
		      (slot-boundp
		       (jde-dbs-get-target-process)
		       'watch-req))]

	 )

	(list
	 "Trace"

	 ["Class Prep..."
	  jde-bug-trace-class-prep
	  :style    nil
	  :active	 (and
			  (jde-dbs-debugger-running-p)
			  (jde-dbs-get-target-process))]

	 ["Class Unload..."
	  jde-bug-trace-class-unload
	  :style    nil
	  :active	 (and
			  (jde-dbs-debugger-running-p)
			  (jde-dbs-get-target-process))]


	 ["Method Entry..."
	  jde-bug-trace-method-entry
	  :style    nil
	  :active	 (and
			  (jde-dbs-debugger-running-p)
			  (jde-dbs-get-target-process))]

	 ["Method Exit..."
	  jde-bug-trace-method-exit
	  :style    nil
	  :active	 (and
			  (jde-dbs-debugger-running-p)
			  (jde-dbs-get-target-process))]

	 ["Exceptions..."
	  jde-bug-trace-exceptions
	  :style    nil
	  :active	 (and
			  (jde-dbs-debugger-running-p)
			  (jde-dbs-get-target-process))]


	 ["Cancel..."
	  jde-bug-cancel-trace
	  :style     nil
	  :active    (and
		      (jde-dbs-debugger-running-p)
		      (jde-dbs-get-target-process)
		      (slot-boundp
		       (jde-dbs-get-target-process)
		       'trace-req))]

	 )

	"-"

	(list
	 "Display"

	 ["Loaded Classes"
	  jde-bug-display-loaded-classes
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["Object Monitors"
	  jde-bug-show-object-monitors
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["Path Info"
	  jde-bug-display-path-info
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]
	 )

	["Display Variable At Point"
	 jde-bug-display-variable
	 (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	["Evaluate Expression"
	 jde-bug-evaluate-expression
	 (and
	  (jde-dbs-debugger-running-p)
	  (jde-dbs-get-target-process))]

	(list
	 "Stack"
	 `["Enable"
	   jde-bug-toggle-stack-info
	   ,(if jde-xemacsp :active :enable) t
	   :style radio
	   :selected jde-bug-stack-info]
	 ["Up"
	  jde-bug-up-stack
	  (and
	   (jde-dbs-target-process-steppable-p)
	   (let* ((process (jde-dbs-get-target-process))
		  (stack-max
		   (if (slot-boundp process 'stack)
		       (1- (length (oref process stack)))
		     0))
		  (stack-ptr (oref process stack-ptr)))
	     (< stack-ptr stack-max)))]

	 ["Down"
	  jde-bug-down-stack
	  (and
	   (jde-dbs-target-process-steppable-p)
	   (let* ((process (jde-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))]

	 )
	(list
	 "Thread"

	 ["Suspend"
	  jde-bug-suspend-thread
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]


	 ["Resume"
	  jde-bug-resume-thread
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["Interrupt"
	  jde-bug-interrupt-thread
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["Stop"
	  jde-bug-stop-thread
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

				       ; ["Show Thread Info"
			      ;   jde-bug-thread-show-thread-info nil]
	 )

	(list
	 "Processes"
	 ["Start Debugger"
	  jde-bug-start-debugger
	  (not (jde-dbs-debugger-running-p))]

	 ["Launch Process"
	  jde-bug-launch-process
	  (jde-dbs-debugger-running-p)]

	 ["Suspend Process"
	  jde-bug-suspend-process
	  (let ((process (jde-dbs-get-target-process)))
	    (and
	     (jde-dbs-debugger-running-p)
	     process
	     (not (oref process suspendedp))))]

	 ["Resume Process"
	  jde-bug-resume-process
	  (let ((process (jde-dbs-get-target-process)))
	    (and
	     (jde-dbs-debugger-running-p)
	     process
	     (oref process suspendedp)))]

	 ["Finish Process"
	  jde-bug-finish-process
	  (let ((process (jde-dbs-get-target-process)))
	    (and
	     (jde-dbs-debugger-running-p)
	     process
	     (not (oref process attachedp))))]

	 "-"

	 (list
	  "Attach Process"
	  ["Via Shared Memory"
	   jde-bug-attach-via-shared-memory
	   (and
	    (eq system-type 'windows-nt)
	    (jde-dbs-debugger-running-p))]

	  ["On Local Host"
	   jde-bug-attach-local-host
	   (jde-dbs-debugger-running-p)]

	  ["On Remote Host"
	   jde-bug-attach-remote-host
	   (jde-dbs-debugger-running-p)]
	  )

	 (list
	  "Listen on"
	  ["Shared Memory"
	   jde-bug-listen-shmem
	   (and
	    (eq system-type 'windows-nt)
	    (jde-dbs-debugger-running-p))]

	  ["Socket"
	   jde-bug-listen-socket
	   (jde-dbs-debugger-running-p)]
	  )

	 ["Detach Process"
	  jde-bug-detach-process
	  (let ((process (jde-dbs-get-target-process)))
	    (and
	     (jde-dbs-debugger-running-p)
	     process
	     (oref process attachedp)))]


	 "-"

	 ["Set Target Process"
	  jde-bug-set-target-process
	  (> (jde-dbs-proc-set-get-size
	      jde-dbs-the-process-registry)
	     0)]

	 ["Show Processes"
	  jde-bug-set-show-processes nil]

	 ["Remove Dead Processes"
	  jde-bug-remove-dead-processes
	  (oref jde-dbs-the-process-morgue proc-alist)]

	 )
	(list
	 "Show Buffer"

	 ["Locals"
	  jde-bug-show-locals-buf
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["CLI"
	  jde-bug-show-cli-buf
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]

	 ["Threads"
	  jde-bug-show-threads-buf
	  (and
	   (jde-dbs-debugger-running-p)
	   (jde-dbs-get-target-process))]
	 )
	["Show Debug Frame"
	 jde-bug-show-debug-frame
	 (and (jde-dbs-debugger-running-p)
	      (jde-dbs-get-target-process))]
	"-"
	["Preferences"
	 jde-bug-show-preferences t]
	"-"
	["Help"
	 jde-bug-help t]
	)
  "JDEbug menu specification")


(defvar jde-bug-mode-map
  (let ((km (make-sparse-keymap)))
    (easy-menu-define jde-bug-menu km "JDEbug Minor Mode Menu"
		      jde-bug-menu-spec)
    km)
  "Keymap for JDEbug minor mode.")

(defvar jde-bug-minor-mode nil
  "Non-nil if JDEBug minor mode is enabled.")
(make-variable-buffer-local 'jde-bug-minor-mode)

(defun jde-bug-minor-mode (&optional arg)
  "Toggle JDEbug minor mode.
With prefix argument ARG, turn on if positive, otherwise off.

\\{jde-bug-mode-map}"
  (interactive
   (list (or current-prefix-arg
	     (if jde-bug-minor-mode 0 1))))

  (setq jde-bug-minor-mode
	(if arg
	    (> (prefix-numeric-value arg) 0)
	  (not jde-bug-minor-mode)))

  (run-hook-with-args 'jde-bug-minor-mode-hook jde-bug-minor-mode))

(semantic-add-minor-mode 'jde-bug-minor-mode " JDEbug" jde-bug-mode-map)

;; (fmakunbound 'jde-bug-key-bindings)
(defcustom jde-bug-key-bindings
  (list (cons "[?\C-c ?\C-z ?\C-s]" 'jde-bug-step-over)
	(cons "[?\C-c ?\C-z ?\C-x]" 'jde-bug-step-into)
	(cons "[?\C-c ?\C-z ?\C-a]" 'jde-bug-step-into-all)
	(cons "[?\C-c ?\C-z ?\C-w]" 'jde-bug-step-out)
	(cons "[?\C-c ?\C-z ?\C-c]" 'jde-bug-continue)
	(cons "[?\C-c ?\C-z ?\C-b]" 'jde-bug-toggle-breakpoint))
  "*Specifies key bindings for JDEbug.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-bug
  :type '(repeat
	  (cons :tag "Key binding"
	   (string :tag "Key")
	   (function :tag "Command")))
  :set '(lambda (sym val)
	  ;; Unmap existing key bindings
	  (if (and
	       (boundp 'jde-bug-key-bindings)
	       jde-bug-key-bindings)
	      (mapc
	       (lambda (binding)
		 (let ((key (car binding))
		       (fcn (cdr binding)))
		   (if (string-match "\\[.+]"key)
		       (setq key (car (read-from-string key))))
		   (define-key jde-bug-mode-map key nil)))
	       jde-bug-key-bindings))
	  ;; Map new key bindings.
	  (mapc
	   (lambda (binding)
	     (let ((key (car binding))
		   (fcn (cdr binding)))
	       (if (string-match "\\[.+]"key)
		   (setq key (car (read-from-string key))))
	       (define-key jde-bug-mode-map key fcn)))
	   val)
	  (set-default sym val)))


(defun jde-bug-step-over ()
  "Advances the process to the next line in the current method."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step over" :process process)))
    (jde-dbs-cmd-exec cmd)))

(defun jde-bug-step-into ()
  "Advances to the next step in the method at point except if the method
   belongs to the java, javax, or sun packages."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step into" :process process :step-type "into")))
    (jde-dbs-cmd-exec cmd)))

(defun jde-bug-step-into-all ()
  "Advances the process into the function invoked at point."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step into" :process process :step-type "into-all")))
    (jde-dbs-cmd-exec cmd)))

(defun jde-bug-step-out ()
  "Advances the process to the next line in the invoking method."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-step "step into" :process process :step-type "out")))
    (jde-dbs-cmd-exec cmd)))


(defun jde-bug-continue ()
  "Runs the target process. Execution continues from the current breakpoint."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (run (jde-dbs-run-process
	       (format "run %d" (oref process id))
		  :process process)))
    (oset process startupp nil)
    (oset process suspendedp nil)
    (oset process steppablep nil)
    (setq overlay-arrow-position nil)
    (jde-dbs-cmd-exec run)))



(defun jde-bug-exit ()
  (interactive)
  (if (jde-dbs-debugger-running-p)
      (progn
	(mapc
	 (lambda (assoc-x)
	   (let* ((process (cdr assoc-x))
		  (finish-cmd (jde-dbs-finish-process
			       (format "finish %d" (oref process id))
			       :process process))
		  (result (jde-dbs-cmd-exec finish-cmd)))
	     (jde-dbs-proc-move-to-morgue process)))
	 (oref jde-dbs-the-process-registry proc-alist))
	(slot-makeunbound jde-dbs-the-process-registry :target-process)
	(jde-dbs-debugger-quit jde-dbs-the-debugger))
    (error "Debugger is not running.")))


(add-hook
 'jde-mode-hook
 (lambda ()
   (if (buffer-file-name)
       (let ((this-file (file-name-nondirectory (buffer-file-name))))
	 (mapc
	  (lambda (spec)
	    (let* ((file (car spec))
		   (line (cdr spec))
		   (bp (jde-db-find-breakpoint file line)))
	      (when (not bp)
		(setq jde-db-breakpoint-id-counter (1+ jde-db-breakpoint-id-counter))
		(setq bp
		      (jde-db-breakpoint
		       (format "breakpoint%d" jde-db-breakpoint-id-counter)
		       :id jde-db-breakpoint-id-counter
		       :file file
		       :line line))
		(jde-db-breakpoints-add bp))
	      (if (string-match file this-file)
		  (jde-db-mark-breakpoint-specified file line))))
	  jde-bug-saved-breakpoints)))))


(defun jde-bug-set-breakpoint()
  "Sets a breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line))
	 (proc (jde-dbs-get-target-process)))
    (unless bp
      (setq bp (jde-db-spec-breakpoint))
      (oset bp line line)
      (jde-db-breakpoints-add bp))
    (if (and bp proc)
	(let* ((set-breakpoint (jde-dbs-set-breakpoint
				"set breakpoint"
				:process proc
				:breakpoint bp))
	       (result (jde-dbs-cmd-exec set-breakpoint)))
	  (message "Breakpoint set at line %d in class %s." line file)))))


(defun jde-bug-set-conditional-breakpoint ()
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Breakpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jde-bug-clear-breakpoint()
  "Clear the breakpoint at the current line in the current buffer."
  (interactive)
  (let* ((file (buffer-file-name))
	 (line (jde-get-line-at-point))
	 (bp (jde-db-find-breakpoint file line))
	 (proc (jde-dbs-get-target-process)))
    (if (and bp proc)
	(let* ((clear-breakpoint
		(jde-dbs-clear-breakpoint
		 "clear breakpoint"
		 :process proc
		 :breakpoint bp))
	       (result (jde-dbs-cmd-exec clear-breakpoint)))))
    (if bp
	(jde-db-delete-breakpoint bp))))

;; test by lea

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Toggle Breakpoint Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-toggle-breakpoint ()
  "Toggles the breakpoint on the current line."
  (interactive)

  (assert (equal major-mode 'jde-mode)
	  nil "You can only toggle a breakpoint within jde-mode")

  (assert (jde-db-src-dir-matches-file-p (buffer-file-name))
	  nil "The current buffer is not in the source path.  See `jde-sourcepath' for more information.")

  (let*  ((file (buffer-file-name))
	  (line (jde-get-line-at-point))
	  (bp (jde-db-find-breakpoint file line)))
    (if bp
	(jde-bug-clear-breakpoint)
      (jde-bug-set-breakpoint))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Save Breakpoints Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-save-breakpoints ()
  "Save breakpoints in project file."
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Methods Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-bug-trace-methods-dialog (efc-dialog)
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

(defmethod initialize-instance ((this jde-bug-trace-methods-dialog) &rest fields)
  "Constructor for trace methods dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "entry")
	      (string= (oref this trace-type) "exit")))
)


(defmethod efc-dialog-create ((this jde-bug-trace-methods-dialog))

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

(defmethod efc-dialog-ok ((this jde-bug-trace-methods-dialog))
  (let* ((thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-exclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-methods-request "trace methods request"
						 :trace-type (oref this trace-type)))
	 (cmd  (jde-dbs-trace-methods
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

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jde-bug-trace-method-entry ()
  "Displays the trace method entry dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-methods-dialog "trace method entry dialog")))
    (efc-dialog-show dialog)))

(defun jde-bug-trace-method-exit ()
  "Displays the trace method exit dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-methods-dialog
		 "trace method exit dialog" :trace-type "exit")))
    (efc-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Classes Command                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jde-bug-trace-classes-dialog (efc-dialog)
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

(defmethod initialize-instance ((this jde-bug-trace-classes-dialog) &rest fields)
  "Constructor for trace classes dialog."

  ;; Call parent initializer.
  (call-next-method)

  (assert (or (string= (oref this trace-type) "preparation")
	      (string= (oref this trace-type) "unloading")))
)


(defmethod efc-dialog-create ((this jde-bug-trace-classes-dialog))

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

(defmethod efc-dialog-ok ((this jde-bug-trace-classes-dialog))
  (let* ((thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-classes-request "trace classes request"
						 :trace-type (oref this trace-type)))
	 (cmd  (jde-dbs-trace-classes
		"trace classes command"
		:process process :trace-request request)))

    (if (and thread-suspension-policy (not (string= thread-suspension-policy "")))
	(oset request :suspend-policy thread-suspension-policy))

    (if class-inclusion-filters
	(oset request :inclusion-filters class-inclusion-filters))

    (if class-exclusion-filters
	(oset request :exclusion-filters class-exclusion-filters))

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jde-bug-trace-class-prep ()
  "Displays the trace class preparation dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-classes-dialog "trace class prep dialog")))
    (efc-dialog-show dialog)))

(defun jde-bug-trace-class-unload ()
  "Displays the trace class unloading dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-classes-dialog
		 "trace class unloading dialog" :trace-type "unloading")))
    (efc-dialog-show dialog)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Trace Exceptions Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass jde-bug-trace-exceptions-dialog (efc-dialog)
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

(defmethod initialize-instance ((this jde-bug-trace-exceptions-dialog) &rest fields)
  "Constructor for trace exceptions dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod efc-dialog-create ((this jde-bug-trace-exceptions-dialog))

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

(defmethod efc-dialog-ok ((this jde-bug-trace-exceptions-dialog))
  (let* ((exception-class (widget-value (oref this exception-class-field)))
	 (trace-type (widget-value (oref this trace-type-field)))
	 (thread-restriction (widget-value (oref this thread-restriction-field)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-field)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-field)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-field)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-exceptions-request
		   "trace exceptions request"
		   :exception-class exception-class
		   :trace-type trace-type))
	 (cmd  (jde-dbs-trace-exceptions
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

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))

(defun jde-bug-break-on-exception (exception-class)
  (interactive "sFully qualified exception: ")
  (let* ((process (jde-dbs-get-target-process))
	 (request (jde-dbs-trace-exceptions-request
		   "break on exceptions request"
		   :exception-class exception-class
		   :trace-type "both"
		   :suspend-policy "all"))
	 (cmd (jde-dbs-trace-exceptions
	       "break on exceptions command"
	       :process process :trace-request request)))
    (jde-dbs-cmd-exec cmd)
    (jde-dbs-proc-display-debug-message process "Use JDEbug->Trace->Cancel to remove this breakpoint")))


(defun jde-bug-trace-exceptions ()
  "Displays the trace exceptions dialog."
  (interactive)
  (let ((dialog (jde-bug-trace-exceptions-dialog "trace exceptions dialog")))
    (efc-dialog-show dialog)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Trace Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-cancel-trace-request (process request)
  "Cancels a specified trace request on a specified process."
  (let ((cmd (jde-dbs-cancel-trace "cancel trace" :process process
				  :trace-request request)))
    (jde-dbs-cmd-exec cmd)))

(defclass jde-bug-cancel-trace-dialog (efc-dialog)
  ((process          :initarg :process
		     :type jde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod efc-dialog-create ((this jde-bug-cancel-trace-dialog))
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
		(if (typep request 'jde-dbs-trace-methods-request)
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

(defmethod efc-dialog-ok ((this jde-bug-cancel-trace-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jde-bug-cancel-trace-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jde-bug-cancel-trace ()
  "Cancels method and class trace requests for the target process.
If only one trace request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive)
 (let* ((process (jde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'trace-req)
	   (let ((trace-requests (oref process :trace-req)))
	     (if (= (length trace-requests) 1)
		 (jde-bug-cancel-trace-request process (cdr (car trace-requests)))
	       (let ((dialog
		      (jde-bug-cancel-trace-dialog "cancel trace dialog"
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


(defclass jde-bug-watch-field-dialog (efc-dialog)
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

(defmethod initialize-instance ((this jde-bug-watch-field-dialog) &rest fields)
  "Constructor for watch field dialog."

  ;; Call parent initializer.
  (call-next-method)

)


(defmethod efc-dialog-create ((this jde-bug-watch-field-dialog))

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

(defmethod efc-dialog-ok ((this jde-bug-watch-field-dialog))
  (let* ((obj-class (widget-value (oref this object-class-widget)))
	 (field-name (widget-value (oref this field-name-widget)))
	 (expression (widget-value (oref this expression-widget)))
	 (object-id (widget-value (oref this object-id-widget)))
	 (thread-restriction (widget-value (oref this thread-restriction-widget)))
	 (thread-suspension-policy (widget-value (oref this suspend-policy-widget)))
	 (class-inclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (class-exclusion-filters (widget-value (oref this class-inclusion-widget)))
	 (process (jde-dbs-get-target-process))
	 (request (jde-dbs-watch-field-request
		   "watch field request"
		   :watch-type (oref this watch-type)
		   :object-class obj-class
		   :field-name field-name))
	 (cmd  (jde-dbs-watch-field
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

    (jde-dbs-cmd-exec cmd)
    (call-next-method)))


(defun jde-bug-watch-field-access ()
  "Request that the debugger watch for access of a
field of an object or class of objects."
  (interactive)
  (let ((dialog
	 (jde-bug-watch-field-dialog
	  "watch field dialog"
	  :object-class (concat
			 "*."
			 (car (jde-parse-get-innermost-class-at-point)))
	  :field (thing-at-point 'symbol))))
    (efc-dialog-show dialog)))

(defun jde-bug-watch-field-modification ()
  "Request that the debugger watch for modifiction of a
field of an object or class of objects."
  (interactive)
  (let ((dialog (jde-bug-watch-field-dialog
		 "watch field dialog"
		 :watch-type "modification"
		 :object-class (concat
				"*."
				(car (jde-parse-get-innermost-class-at-point)))
		 :field (thing-at-point 'symbol))))
    (efc-dialog-show dialog)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Cancel Watch Request Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-cancel-watch-request (process request)
  "Cancels a specified watch field request on a specified process."
  (let ((cmd (jde-dbs-cancel-watch "cancel watch" :process process
				  :watch-request request)))
    (jde-dbs-cmd-exec cmd)))

(defclass jde-bug-cancel-watch-dialog (efc-dialog)
  ((process          :initarg :process
		     :type jde-dbs-proc)
   (requests         :initarg :requests
		     :type list)
   (check-boxes      :initarg :check-boxes))
)

(defmethod efc-dialog-create ((this jde-bug-cancel-watch-dialog))
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

(defmethod efc-dialog-ok ((this jde-bug-cancel-watch-dialog))
  (message (format "Check boxes: %s)" (widget-value (oref this check-boxes))))
  (mapc
   (lambda (id-x)
     (let ((request
	    (cdr
	     (find-if
	      (lambda (x) (= (car x) id-x))
	     (oref this requests)))))
     (jde-bug-cancel-watch-request  (oref this process) request)))
   (widget-value (oref this check-boxes)))
  (call-next-method))


(defun jde-bug-cancel-watch ()
  "Cancels watch requests for the target process.
If only one watch request is outstanding, this command cancels that request.
Otherwise, this command displays a cancel dialog that lets you choose the
requests to cancel."
 (interactive)
 (let* ((process (jde-dbs-get-target-process)))
   (if process
       (if (slot-boundp process 'watch-req)
	   (let ((watch-requests (oref process :watch-req)))
	     (if (= (length watch-requests) 1)
		 (jde-bug-cancel-watch-request process (cdr (car watch-requests)))
	       (let ((dialog
		      (jde-bug-cancel-watch-dialog "cancel watch dialog"
						   :process process
						   :requests watch-requests)))
		 (efc-dialog-show dialog))))
	 (error "The target process has no outstanding watch requests"))
     (error "There is no active process."))))

(defun jde-make-frame-names-alist ()
  (let* ((current-frame (selected-frame))
	 (falist
	  (cons
	   (cons (if jde-xemacsp
		     (frame-property current-frame 'name)
		   (frame-parameter current-frame 'name))
		 current-frame) nil))
	 (frame (next-frame nil t)))
    (while (not (eq frame current-frame))
      (progn
	(setq falist (cons (cons
			    (if jde-xemacsp
				(frame-property frame 'name)
			      (frame-parameter frame 'name))
			    frame) falist))
	(setq frame (next-frame frame t))))
    falist))

(defun jde-bug-show-debug-frame ()
  "Show or open a new frame with the locals, thread, and CLI buffer."
  (interactive)
  (let* ((existing-frame (cdr (assoc "JDebug" (jde-make-frame-names-alist)))))
    (if existing-frame
      (progn
	(make-frame-visible existing-frame)
	(raise-frame existing-frame)
	(select-frame existing-frame))
      (let* ((process (jde-dbs-get-target-process))
	     (cli-buffer (when (slot-boundp process 'cli-buf)
			(oref process cli-buf)))
	     (locals-buffer (when (slot-boundp process 'locals-buf)
			   (oref process locals-buf)))
	     (threads-buffer (when (slot-boundp process 'threads-buf)
			    (oref process threads-buf)))
	     (msg-buffer (when (slot-boundp process 'msg-buf)
			(oref process msg-buf)))
	     (frame (new-frame '((name . "JDebug") (minibuffer . nil))))
	     (height (/ (frame-height frame) (count-if 'identity
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
	      (when (not jde-bug-local-variables)
		(jde-bug-toggle-local-variables)
		(if (featurep 'xemacs)
		  (sleep-for 0.1)
		  (sleep-for 0 100)))
	      (set-window-buffer locals-win locals-buffer)))
	  (when threads-buffer
	    (let ((threads-win (split-window prev-window height)))
	      (setq prev-window threads-win)
	      (jde-bug-show-threads)
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

(defun jde-bug-display-variable ()
  (interactive)
  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))
  ;; Previously we used semantic. I recently have been having problems
  ;; with it, so I just replaced it with word-at-point
  (let ((qualified-expr (thing-at-point 'word)))
    (jde-bug-evaluate-expression qualified-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Evaluate Expression Command                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-evaluate-expression (expression)
"Evaluates a Java expression. The Java expression may include
any variables in scope in the program being debugged."
  (interactive
   "sExpression: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (if  (string= expression "")
      (error "Empty expression."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process state-info))
	 (thread-id (oref state-info thread-id))
	 (evaluate-command
	  (jde-dbs-evaluate
	   (format "Evaluate %s" expression)
	   :process process
	   :expression expression
	   :thread-id thread-id))
	 (result
	  (jde-dbs-cmd-exec evaluate-command)))
    (if result
	(let* ((type  (nth 0 result))
	       (value (nth 1 result))
	       (buf (get-buffer-create (concat "Expression: " expression))))
	    (jde-dbo-view-var-in-buf (jde-dbs-objectify-value result)
				     expression process t buf t)
	    (pop-to-buffer buf (split-window nil (- (window-height) 4))))
      (message "Error: could not evaluate \"%s\"." expression))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Loaded Classes Command                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-loaded-classes ()
  "Displays the classes currently loaded by the target process."
  (interactive)

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-get-loaded-classes
	   "get_loaded_classes"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec cmd)))
    (if (not result)
      (error "Could not get loaded classes."))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Threads Command                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-show-threads ()
"Shows all threads and thread-groups running in the target process.
This command displays the threads as a tree structure. To expand
a node of the tree, click the + sign next to the node, using mouse
button 2."
  (interactive)

  (if (not (jde-dbs-get-target-process))
      (error "No target process."))

  (if (not jde-bug-stack-info)
      (progn
	(jde-bug-toggle-stack-info)
	(if (featurep 'xemacs)
	    (sleep-for 0.1)
	  (sleep-for 0 100))))

  (let* ((process (jde-dbs-get-target-process))
	 (get-threads-command
	  (jde-dbs-get-threads
	   "get_threads"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec get-threads-command)))
    (if (not result)
      (error "Could not get threads"))))

(defun jde-bug-thread-show-thread-info ()
  (interactive)
  (message "not implemented"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Show Object Monitors                                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-show-object-monitors (object-id)
"Shows the threads that are monitoring a specified object, including the thread
that currently owns the object and threads that are waiting to access the object."
  (interactive
   "nObject ID: ")

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (get-monitors-command
	  (jde-dbs-get-object-monitors
	   "get_object_monitors"
	   :process process :object-id object-id))
	 (result
	  (jde-dbs-cmd-exec get-monitors-command)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Display Path Info Command                                                  ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-display-path-info ()
  "Displays the base directory, boot classpath, and classpath of the target process."
  (interactive)

  (if (not (jde-dbs-target-process-runnable-p))
      (error "No target process or process is not suspended."))

  (let* ((process (jde-dbs-get-target-process))
	 (cmd
	  (jde-dbs-get-path-info
	   "get_path_info"
	   :process process))
	 (result
	  (jde-dbs-cmd-exec cmd)))
    (if (not result)
      (error "Could not get path info."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Clear Watchpoint Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-clear-watchpoint ()
  (interactive)
  (message "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Up Stack Command                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-up-stack ()
  "Moves the source cursor up one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not
       (let* ((process (jde-dbs-get-target-process))
	      (stack-max (1- (length (oref process stack))))
	      (stack-ptr (oref process stack-ptr)))
	 (< stack-ptr stack-max)))
      (error "The debugger is displaying the top of the stack."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1+ (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))

    (oset process :stack-ptr stack-ptr)
    (jde-db-set-debug-cursor class file line)
    (jde-dbo-update-locals-buf process thread-id stack-ptr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Down Stack Command                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-down-stack ()
  "Moves the source cursor down one frame in the call stack and displays the local
variables at that point in the stack. This command works only for the stack of
the thread at a breakpoint or step point."
  (interactive)

  (if (not (jde-dbs-target-process-steppable-p))
      (error "The target process is not suspended at a breakpoint or steppoint."))

  (if (not (let* ((process (jde-dbs-get-target-process))
		  (stack-ptr (oref process stack-ptr)))
	     (> stack-ptr 0)))
      (error "The debugger is displaying the bottom of the stack."))

  (let* ((process (jde-dbs-get-target-process))
	 (state-info (oref process :state-info))
	 (thread-id (oref state-info :thread-id))
	 (stack (oref process stack))
	 (stack-ptr (1- (oref process stack-ptr)))
	 (frame (nth stack-ptr stack))
	 (class (nth 1 frame))
	 (file (nth 2 frame))
	 (line (nth 3 frame)))

    (oset process :stack-ptr stack-ptr)
    (jde-db-set-debug-cursor class file line)
    (jde-dbo-update-locals-buf process thread-id stack-ptr)))



(defun jde-bug-suspend-thread (thread-id)
"Suspends the thread or group of threads specified by THREAD-ID.
If the thread or group is already suspended, this command increments
the thread's suspend count. Use JDEBug->Threads->Show Threads (`jde-bug-thread-show-threads')
to display the IDs of all threads and thread groups running in the
target process. Use JDEBug->Processes->Suspend Process
(`jde-bug-suspend-process') to suspend the entire process. Use
Threads->Resume Thread (`jde-bug-resume-thread') to resume the thread."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (suspend-command
	  (jde-dbs-suspend-thread
	       (format "suspend thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec suspend-command)))


(defun jde-bug-resume-thread (thread-id)
"Resumes the previously suspended thread or group of threads specified
by THREAD-ID.  This command has no effect if the specified thread or
thread-group is running or was not suspended by you, using the
JDEBug->Threads->Suspend Thread command (`jde-bug-suspend-thread').
If you suspended the thread more than once, this command reduces the
suspend count by 1. The thread resumes only when the suspend count
reaches 0. Use JDEBug->Threads->Show Threads
(`jde-bug-thread-show-threads') to display the IDs of all threads and
thread groups running in the target process. Use
JDEBug->Processes->Resume Process (`jde-bug-resume-process') to resume
the entire process."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (resume-command
	  (jde-dbs-resume-thread
	       (format "resume thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec resume-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Interrupt Thread Command                                                   ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-interrupt-thread (thread-id)
"Interrupts the thread specified by THREAD-ID. The thread cannot be
resumed. Use JDEBug->Threads->Show Threads
(`jde-bug-thread-show-threads') to display the IDs of all threads
running in the target process. Use Threads->Suspend Thread
(`jde-bug-suspend-thread') to suspend a thread temporarily."
  (interactive
   "nThread ID: ")
  (let* ((process (jde-dbs-get-target-process))
	 (interrupt-command
	  (jde-dbs-interrupt-thread
	       (format "interrupt thread %d" thread-id)
	       :process process
	       :thread-id thread-id)))
    (jde-dbs-cmd-exec interrupt-command)))


(defun jde-bug-stop-thread (thread-id exception-id)
"Stops a thread and throws an exception. THREAD-ID is the id of the thread you want
to stop. EXCEPTION-ID is the id of the exception object you want to throw. Use
JDEBug->Threads->Show Threads (`jde-bug-thread-show-threads') to display the IDs of
all threads and thread groups running in the target process. Use JDEBug->Evaluate Expression
to creae the exception object."
 (interactive
   "nThread ID: \nnException Id: ")
  (let* ((process (jde-dbs-get-target-process))
	 (stop-command
	  (jde-dbs-stop-thread
	       (format "stop thread %d" thread-id)
	       :process process
	       :thread-id thread-id
	       :exception-id exception-id)))
    (jde-dbs-cmd-exec stop-command)))

(defun jde-bug-jpda-installed-p ()
  "Returns t if the jpda is installed."
  (interactive)
  (cond
   ((jde-bug-vm-includes-jpda-p)
    t)
   ((string= jde-bug-jpda-directory "")
    (error "jde-bug-jpda-directory variable is not set.")
    nil)
   ((not (file-exists-p
	  (expand-file-name "lib/jpda.jar" (jde-normalize-path
					    'jde-bug-jpda-directory))))
    (error "Cannot find JPDA jar file at %s"
	     (expand-file-name "lib/jpda.jar" jde-bug-jpda-directory))
    nil)
   (t
    t)))


(defun jde-bug-start-debugger ()
  "Starts the debugger."
  (interactive)
  (if (not (jde-dbs-debugger-running-p))
      (if (and (jde-bug-jpda-installed-p)
	   (jde-dbs-debugger-start jde-dbs-the-debugger))
      (message "Debugger started successfully." )
      (message "Could not start debugger."))
    (message "Debugger is already started")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Launch Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-launch-process ()
  "Starts a virtual machine to run the application
in the current source buffer. Halts at the beginning
of the process to let you set breakpoints. The started
process becomes the target process for debugger
commands. Select Processes->Set Target Process from the JDEBug
menu or run the `jde-bug-set-target-process' command
to set another process as the target process."
  (interactive)
  (let* ((main-class (jde-run-get-main-class)))
    (unless (and
	     (jde-dbs-proc-set-find jde-dbs-the-process-registry
				    :main-class main-class)
	     (not (yes-or-no-p
		   (format "An instance of %s is already running. Continue?" main-class))))
      (let* ((process
	      (jde-dbs-proc (format "process%d"
				    (setq jde-dbs-proc-counter
					  (1+ jde-dbs-proc-counter)))
			    :id jde-dbs-proc-counter :main-class main-class))
	     (old-target (jde-dbs-get-target-process))
	     (launch (jde-dbs-launch-process
		      (format "Launch %s" main-class)
		      :process process
		      :vmexec (car jde-bug-vm-executable)
		      ;; :vmexec "xyz"
		      ))
	     (succeededp t))
	(jde-dbs-proc-set-add jde-dbs-the-process-registry process)
	(if (not (string= jde-bug-jre-home ""))
	    (oset launch :jre-home (jde-normalize-path 'jde-bug-jre-home)))
	(oset jde-dbs-the-process-registry :target-process process)
	(when (not (jde-dbs-cmd-exec launch))
	  (jde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jde-dbs-the-process-registry :target-process old-target))
	  (jde-dbs-proc-set-state process "unknown")
	  (jde-dbs-proc-set-state-reason process "Error launching process.")
	  (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
	  (setq succeededp nil))
	succeededp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Local Process Command                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-bug-attach-via-shared-memory (process-name)
  "Attaches the debugger to a process running on the same machine via shared
memory. This command works only on Windows."
  (interactive
   "sProcess name: ")
  (let* ((process
	  (jde-dbs-proc (format "process%d"
				(setq jde-dbs-proc-counter
				      (1+ jde-dbs-proc-counter)))
			:id jde-dbs-proc-counter :main-class process-name))
	 (old-target (jde-dbs-get-target-process))
	 (attach (jde-dbs-attach-shmem
		  (format "Attach %s" process-name)
		  :process process
		  :process-name process-name)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (if (not (jde-dbs-cmd-exec attach))
	(progn
	  (jde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jde-dbs-the-process-registry :target-process old-target))
	  (jde-dbs-proc-set-state process "unknown")
	  (jde-dbs-proc-set-state-reason process "Error launching process.")
	  (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
	  nil)
      (jde-bug-set-breakpoints process jde-db-breakpoints))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Local Host Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-attach-local-host (process-port)
  "Attaches the debugger to a process running on local host. This command connects
to the process via a socket."
  (interactive
   "sProcess Port: ")
  (let* ((process
	  (jde-dbs-proc (format "process%d"
				(setq jde-dbs-proc-counter
				      (1+ jde-dbs-proc-counter)))
			:id jde-dbs-proc-counter :main-class process-port))
	     (old-target (jde-dbs-get-target-process))
	     (attach (jde-dbs-attach-socket
		      (format "Attach %s" process-port)
		      :process process
		      :port process-port)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (if (not (jde-dbs-cmd-exec attach))
	(progn
	  (jde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jde-dbs-the-process-registry :target-process old-target))
	  (jde-dbs-proc-set-state process "unknown")
	  (jde-dbs-proc-set-state-reason process "Error launching process.")
	  (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
	  nil)
      (jde-bug-set-breakpoints process jde-db-breakpoints))
    t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Attach Process on Remote Host Command                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-attach-remote-host (process-host process-port)
  "Attaches the debugger to a process running on a remote host. This command connects
to the process via a socket."
 (interactive
   "sHost: \nsProcess Port: ")
  (let* ((process
	  (jde-dbs-proc (format "process%d"
				(setq jde-dbs-proc-counter
				      (1+ jde-dbs-proc-counter)))
			:id jde-dbs-proc-counter :main-class process-port))
	     (old-target (jde-dbs-get-target-process))
	     (attach (jde-dbs-attach-socket
		      (format "Attach %s" process-port)
		      :process process
		      :host process-host
		      :port process-port)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (if (not (jde-dbs-cmd-exec attach))
	(progn
	  (jde-dbs-proc-move-to-morgue process)
	  (if old-target
	      (oset jde-dbs-the-process-registry :target-process old-target))
	  (jde-dbs-proc-set-state process "unknown")
	  (jde-dbs-proc-set-state-reason process "Error launching process.")
	  (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
	  nil)
      (jde-bug-set-breakpoints process jde-db-breakpoints))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Shared Memory Command                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jde-bug-server-shmem-name-history nil
  "History of shared memory names for debugger.")

(defun jde-bug-listen-shmem (shmem-name)
  "Listens on shared memory for a vm requiring debugging services."
  (interactive
   (list
    (if (car jde-bug-server-shmem-name)
	(read-from-minibuffer "Name: "
			      (car jde-bug-server-shmem-name-history)
			      nil nil
			      'jde-bug-server-shmem-name-history)
      (cdr jde-bug-server-shmem-name))))
  (let* ((process
	  (jde-dbs-proc (format "process%d"
				(setq jde-dbs-proc-counter
				      (1+ jde-dbs-proc-counter)))
			:id jde-dbs-proc-counter :main-class shmem-name))
	     (old-target (jde-dbs-get-target-process))
	     (listen (jde-dbs-listen-for-process
		      (format "Listen %s" shmem-name)
		      :process process
		      :address shmem-name)))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec listen))
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error listening for process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Listen for Process on Socket Command                                       ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jde-bug-server-socket-history nil
  "History of sockets used by debugger to listen for debuggee vms.")

(defun jde-bug-listen-socket (socket)
  "Listens on socket for a vm requiring debugging services.
If `jde-bug-server-socket' is set to \"prompt for,\" this command
prompts you to enter the socket on which to listen. Otherwise, it
listens on the socket specified by `jde-bug-server-socket'."
  (interactive
   (list
    (if (car jde-bug-server-socket)
	(read-from-minibuffer "Socket: "
			      (car jde-bug-server-socket-history)
			      nil nil
			      'jde-bug-server-socket-history)
      (cdr jde-bug-server-socket))))
  (let* ((process
	  (jde-dbs-proc (format "process%d"
				(setq jde-dbs-proc-counter
				      (1+ jde-dbs-proc-counter)))
			:id jde-dbs-proc-counter :main-class socket))
	     (old-target (jde-dbs-get-target-process))
	     (listen (jde-dbs-listen-for-process
		      (format "Listen %s" socket)
		      :process process
		      :address socket
		      :transport "socket")))
    (jde-dbs-proc-set-add jde-dbs-the-process-registry process)
    (oset jde-dbs-the-process-registry :target-process process)
    (when (not (jde-dbs-cmd-exec listen))
      (jde-dbs-proc-move-to-morgue process)
      (if old-target
	  (oset jde-dbs-the-process-registry :target-process old-target))
      (jde-dbs-proc-set-state process "unknown")
      (jde-dbs-proc-set-state-reason process "Error listening for process.")
      (jde-dbs-proc-set-add jde-dbs-the-process-morgue process)
      nil)
    t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Detach Process Command                                                     ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-detach-process ()
  "Detaches the debugger from the target process. The target process continues
to run."
  (interactive)
  (jde-bug-finish-process))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Suspend Process Command                                                    ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jde-bug-suspend-process ()
"Suspends the target process. To suspend a particular thread or thread group,
use JDEbug->Threads->Suspend Thread (`jde-bug-suspend-thread')."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (suspend-command
	  (jde-dbs-suspend-thread "suspend process" :process process)))
    (jde-dbs-cmd-exec suspend-command)))


(defun jde-bug-resume-process ()
"Resumes the target process. To resume a particular thread or thread group,
use JDEbug->Threads->Resume Thread (`jde-bug-resume-thread')."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (resume-command
	  (jde-dbs-resume-thread "resume process" :process process)))
    (jde-dbs-cmd-exec resume-command)))


(defun jde-bug-finish-process ()
  "Terminates the target process."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	 (finish (jde-dbs-finish-process
		  (format "finish %d" (oref process id))
		  :process process))
	 (result (jde-dbs-cmd-exec finish)))
    (jde-dbs-proc-move-to-morgue process)
    (slot-makeunbound jde-dbs-the-process-registry :target-process)
    (jde-dbs-proc-set-state-reason process "finish")))

(defun jde-bug-set-target-process (process-id)
  "Sets the process whose process-id is PROCESS-ID to be
the focus of debugger commands."
  (interactive
   "nEnter process id: ")
  (jde-dbs-proc-registry-set-target-proc
   jde-dbs-the-process-registry process-id))


(defun jde-bug-show-processes ()
  (interactive)
  (message "not implemented"))


(defun jde-bug-remove-dead-processes ()
  "Remove dead processes and their associated buffers from the Emacs environment."
  (interactive)
  (if (oref jde-dbs-the-process-morgue proc-alist)
      (jde-dbs-proc-morgue-bury-the-dead jde-dbs-the-process-morgue)))


(defun jde-bug-show-locals-buf ()
  "Show the local variables buffer of the target process.
This command splits the window and shows the locals buffer."
  (interactive)
  (if (not jde-bug-local-variables)
      (progn
	(jde-bug-toggle-local-variables)
	(if (featurep 'xemacs)
	    (sleep-for 0.1)
	  (sleep-for 0 100))))

  (let* ((process (jde-dbs-get-target-process))
	 (locals-buf (oref process locals-buf))
	 (source-window (selected-window))
	 locals-window)
    (if (one-window-p)
	(progn
	  (setq locals-window (split-window source-window))
	  (set-window-buffer locals-window locals-buf)))
    (set-window-buffer (next-window source-window) locals-buf)
    (select-window source-window)))

(defun jde-bug-show-cli-buf ()
  "Show the command-line interface (CLI) buffer of the target process."
  (interactive)
  (let* ((process (jde-dbs-get-target-process))
	(cli-buf (oref process cli-buf))
	(source-window (selected-window))
	cli-window)
    (if (one-window-p)
	(progn
	  (setq cli-window (split-window source-window))
	  (set-window-buffer cli-window cli-buf)))
    (set-window-buffer (next-window source-window) cli-buf)
    (select-window source-window)))


(defun jde-bug-show-threads-buf ()
  "Show the threads buffer of the target process."
  (interactive)

  ;;Updating the threads buffer
  (jde-bug-show-threads)

  (let* ((process (jde-dbs-get-target-process))
	 (threads-buf (oref process threads-buf))
	 (source-window (selected-window))
	 threads-window)
    (if (one-window-p)
	(progn
	  (setq threads-window (split-window source-window))
	  (set-window-buffer threads-window threads-buf)))
    (set-window-buffer (next-window source-window) threads-buf)
    (select-window source-window)))


(defun jde-bug-show-preferences ()
  (interactive)
  (customize-apropos "jde-bug" 'groups))


(defun jde-bug-set-breakpoints (process breakpoints)
  "Sets BREAKPOINTS in PROCESS."
  (mapc
   (lambda (assoc-x)
     (let* ((breakpoint (cdr assoc-x))
	    (set-breakpoint (jde-dbs-set-breakpoint
			     (format "set breakpoint%d"
				     (oref breakpoint id))
			     :process process
			     :breakpoint breakpoint))
	    (result (jde-dbs-cmd-exec set-breakpoint)))))
   breakpoints))

;;;###autoload
(defun jde-bug-debug-app ()
  "Runs the debugger on the application in the current source buffer."
  (interactive)
  (if (and
       (jde-bug-jpda-installed-p)
       (not (jde-dbs-debugger-running-p)))
      (jde-dbs-debugger-start jde-dbs-the-debugger))
  (if (jde-dbs-debugger-running-p)
      (let ((result (jde-bug-launch-process)))
	(if result
	    (let ((process (oref jde-dbs-the-process-registry :target-process)))
	      (jde-bug-set-breakpoints process jde-db-breakpoints)
	      (setq result (jde-bug-continue)))))))

(defun jde-bug-help ()
  "Displays the JDEbug User's Guide."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
	 (jdebug-help
	  (if jde-dir
	      (expand-file-name "doc/html/jdebug-ug/jdebug-ug.html" jde-dir))))
    (if (and
	 jdebug-help
	 (file-exists-p jdebug-help))
	(browse-url (concat "file://" (jde-convert-cygwin-path jdebug-help)))
      (signal 'error '("Cannot find JDEbug User's Guide.")))))

(defun jde-bug-keys ()
  "Displays JDEbug keybindings. Use `jde-keys' to display JDE keybindings."
  (interactive)
  (jde-describe-map 'jde-bug-keymap))

(defun jde-bug-toggle-local-variables ()
  "Enables and disables the retrieval of the local variables. It toggles
the value of the variable `jde-bug-local-variables'"
  (interactive)
  (setq jde-bug-local-variables (not jde-bug-local-variables))
  (if (and jde-dbo-current-process
	   jde-dbo-current-thread-id
	   jde-bug-local-variables)
      (jde-dbo-update-locals-buf jde-dbo-current-process
				 jde-dbo-current-thread-id 0)
    (save-excursion
      (if jde-dbo-current-process
	  (progn
	    (set-buffer (oref jde-dbo-current-process locals-buf))
	    (kill-all-local-variables))))))

(defun jde-bug-toggle-stack-info ()
  "Enables and disables the retrieval of stack info. It toggles the value
of the variable `jde-bug-stack-info'"
  (interactive)
  (setq jde-bug-stack-info (not jde-bug-stack-info))
  (if (and jde-dbo-current-process
	   jde-dbo-current-thread-id
	   jde-bug-stack-info)
      (jde-dbo-update-stack jde-dbo-current-process
			    jde-dbo-current-thread-id)
    (save-excursion
      (if jde-dbo-current-process
	  (progn
	    (set-buffer (oref jde-dbo-current-process threads-buf))
	    (kill-all-local-variables)
	    (erase-buffer))))))

(provide 'jde-bug)

;; End of jde-bug.el
