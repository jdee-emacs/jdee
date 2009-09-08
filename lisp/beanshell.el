;;; beanshell.el
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
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

;; This package is intended to serve as an interface between Emacs and
;; Pat Neimeyer's BeanShell, a Java source code interpreter (see
;; http://www.beanshell.org/).

;; This package is intended to serve both interactive users and Emacs
;; applications that want to use Java as an extension language for
;; Emacs. To facilitate this dual usage, this package implements the
;; BeanShell interface as an eieio class named bsh. This class is
;; intended to serve as a base class for classes tailored to specific
;; applications. This class defines the following methods:
;;
;; bsh               Constructs an instance of the bsh class.
;; bsh-launch        Launches the BeanShell and creates an Emacs buffer
;;                   for interacting with the BeanShell.
;; bsh-eval          Sends a Java expression to the BeanShell for evaluation and
;;                   waits for a response.
;; bsh-eval-r        Sends a Java expression to the BeanShell for evaluation
;;                   and then evaluates the resulting BeanShell output
;;                   as a Lisp expression.
;; bsh-async-eval    Sends a Java expression to the BeanShell for evaluation
;;                   and sets a listener to evaluate the response as a Lisp
;;                   expression
;;
;; bsh-eval and bsh-eval-r synchronous functions. After sending a Java
;; expression to the BeanShell, they suspend Emacs until the BeanShell
;; responds. bsh-async-eval does not suspend Emacs. It does, however,
;; set a listener that evaluates any responses from the BeanShell as
;; Lisp expressions.
;;
;; See bsh-demo-eval for an example of how to use bsh-async-eval to evaluate a
;; Java expression.

;;
;; The bsh class also defines fields that specify the path of the
;; BeanShell jar file, the Java vm used to run the BeanShell, a
;; startup classpath and directory, vm arguments, etc. Emacs package
;; developers can easily extend this base class to tailor the BeanShell
;; to specific applications.
;; This package provides one such derived class named
;; bsh-standalone-bsh.  This is a BeanShell implementation intended
;; for use by interactive users.

;; To use the standalone BeanShell, download the BeanShell jar file
;; from http://www.beanshell.org/ and install it on your system.
;; Set bsh-jar to the path of the BeanShell jar. Set bsh-vm to
;; the path of a Java vm on your system (this is necessary only
;; if the java vm is not on your system's command path). Execute
;; M-x bsh to start the BeanShell. To terminate the BeanShell,
;; execute M-x bsh-exit.
;;

(require 'eieio)
(require 'comint)
(require 'lmenu)

(defgroup bsh nil
  "Customizations for the Emacs inteface to Pat Neimeyer's Java
interpreter, the Beanshell."
  :group 'tools
  :prefix "bsh-")

(defcustom bsh-jar "bsh.jar"
  "Path to the jar file containing the BeanShell classes."
  :group 'bsh
  :type 'file)

;; (makunbound 'bsh-vm)
(defcustom bsh-vm nil
  "Java vm to be used to run the standalone BeanShell. By default,
bsh uses the first vm that it finds on the Emacs
`exec-path' (usually the same as your system's command
path). To specify a vm, select Path and enter the
path to the vm. "
  :group 'bsh
  :type '(choice
	  (const :tag "Default" :value nil)
	  (file :tag "Path")))

(defcustom bsh-classpath nil
  "Startup classpath for the BeanShell."
  :group 'bsh
  :type '(repeat (file :tag "Path")))

;; (makunbound 'bsh-startup-timeout)
(defcustom bsh-startup-timeout 10
  "*Length of time Emacs waits for the Beanshell to startup.
Increase the value of this variable if you get Lisp errors on
BeanShell startup on Unix. Setting this value on some versions of
XEmacs, e.g., Windows, seems to cause premature timeouts.  If you keep
getting timeout errors no matter how large a value you set, try
setting this variable to no timeout (nil)."
  :group 'bsh
  :type '(choice (const :tag "No timeout" :value nil)
		 (number :tag "Length")))

;; (makunbound 'bsh-eval-timeout)
(defcustom bsh-eval-timeout 30
  "*Length of time in seconds Emacs waits for the Beanshell to
evaluate a Java expression before giving up and signaling an
error. Setting this value on some versions of XEmacs, e.g., Windows,
seems to cause premature timeouts. If you keep getting timeout errors
no matter how large a value you set, try setting this variable to no
timeout (nil)."
  :group 'bsh
  :type '(choice (const :tag "No timeout" :value nil)
		 (number :tag "Length")))

(defcustom bsh-vm-args nil
  "*Specify arguments to be passed to the Java vm.
This option allows you to specify one or more arguments to be passed
to the Java vm that runs the BeanShell. Note that the value of this
variable should be a list of strings, each of which represents an
argument. When customizing this variable, use a separate text field
for each argument."
  :group 'bsh
  :type '(repeat (string :tag "Argument")))

(defcustom bsh-startup-directory ""
  "Path of directory in which to start the beanshell.
The path may start with a tilde (~) indication your
home directory and may include environment variables.
If this variable is the null string (the default),
the beanshell starts in the directory of the current
buffer."
  :group 'bsh
  :type 'directory)

(defvar bsh-the-bsh nil
  "The BeanShell instance associated with the current BeanShell buffer.")
(make-variable-buffer-local 'bsh-the-bsh)

(defclass bsh-buffer ()
  ((buffer-name   :initarg :buffer-name
		  :initform "*bsh*"
		  :type string
		  :documentation
		  "Name of buffer used to interact with BeanShell process.")
   (buffer        :initarg :buffer
		  :type buffer
		  :documentation
		  "Buffer used to interact with BeanShell process.")

   (process       :initarg :process
		  :documentation
		  "Beanshell process.")

   (filter        :initarg :filter
		  :type function
		  :documentation
		  "Function used to propcess buffer output."))
  "Buffer that displays BeanShell output.")


(defmethod initialize-instance ((this bsh-buffer) &rest fields)
  "Constructor for BeanShell buffer instance."
  (call-next-method)

  (oset this buffer (get-buffer-create (oref this buffer-name))))

(defmethod bsh-buffer-live-p ((this bsh-buffer))
  "Return t if this buffer has not been killed."
  (buffer-live-p (oref this buffer)))

(defmethod bsh-buffer-display ((this bsh-buffer))
  "Display this buffer."
  (pop-to-buffer (oref this buffer-name)))


(defclass bsh-comint-buffer (bsh-buffer)
  ()
  "BeanShell buffer that runs in `comint-mode'.")

(defmethod initialize-instance ((this bsh-comint-buffer) &rest fields)
  "Constructor for BeanShell buffer instance."
  (call-next-method)
  (with-current-buffer (oref this buffer)
    (comint-mode)
    (setq comint-prompt-regexp "bsh % ")))

(defmethod bsh-comint-buffer-exec ((this bsh-comint-buffer) vm vm-args)
  (let ((win32-start-process-show-window t)
	(w32-start-process-show-window t)
	(w32-quote-process-args ?\")   ;; Emacs
	(win32-quote-process-args ?\") ;; XEmacs
	(windowed-process-io t)
	(process-connection-type nil)
	;; XEmacs addition
	(coding-system-for-read
	 (if (or (member system-type '(cygwin32 cygwin))
		 (eq system-type 'windows-nt))
	     'raw-text-dos)))
    (comint-exec (oref this buffer) "bsh"  vm  nil vm-args))

  (oset this process (get-buffer-process (oref this buffer)))
  (oset this filter (process-filter (oref this process)))

  ;; moved to `process-query-on-exit-flag' per compile warning hint: 
  ;; `process-kill-without-query' is an obsolete function (as of Emacs 22.1);
  ;; use `process-query-on-exit-flag' or `set-process-query-on-exit-flag'.
  ;;
  ;;(process-kill-without-query (oref this process))
  (set-process-query-on-exit-flag (oref this process) nil)

  (if (eq system-type 'windows-nt)
      (accept-process-output (oref this process) bsh-startup-timeout 0)
    (while (accept-process-output (oref this process) bsh-startup-timeout 0))))


(defclass bsh-compilation-buffer (bsh-buffer)
  ()
  "Implements a `compilation-mode' buffer for BeanShell output.")

(defmethod initialize-instance ((this bsh-compilation-buffer) &rest fields)
  "Constructor for BeanShell compilation buffer instance."

  (bsh-compilation-buffer-create-native-buffer this)

  (oset
   this
   filter
   (lexical-let ((this-buf this))
     (lambda (process output)
       (bsh-compilation-buffer-filter this-buf process output))))

  ;; This buffer does not have its own process.
  (oset this process nil)
  (bsh-compilation-buffer-set-mode this))

(defmethod bsh-compilation-buffer-create-native-buffer ((this bsh-compilation-buffer))
  "Creates the native Emacs buffer encapsulated by this eieio object."
  (oset this buffer-name "*bsh compilation*")
  (oset this buffer (get-buffer-create (oref this buffer-name))))

(defmethod bsh-compilation-buffer-set-mode ((this bsh-compilation-buffer))
  "Define buffer mode."
  (let ((thisdir default-directory))
    (with-current-buffer (oref this buffer)
      (let ((buf (oref this buffer))
	    ;; Some or all of these variables may not be defined by
	    ;; the various versions of compile.el shipped with Emacs
	    ;; and XEmacs.
	    (error-regexp-alist
	     (if (boundp  'compilation-error-regexp-alist)
		 compilation-error-regexp-alist))
	    (enter-regexp-alist
	     (if (boundp 'compilation-enter-directory-regexp-alist)
		 compilation-enter-directory-regexp-alist))
	    (leave-regexp-alist
	     (if (boundp 'compilation-leave-directory-regexp-alist)
		 compilation-leave-directory-regexp-alist))
	    (file-regexp-alist
	     (if (boundp 'compilation-file-regexp-alist)
		 compilation-file-regexp-alist))
	    (nomessage-regexp-alist
	     (if (boundp 'compilation-nomessage-regexp-alist)
		 compilation-nomessage-regexp-alist))
	    (parser
	     (if (boundp 'compilation-parse-errors-function)
		 compilation-parse-errors-function))
	    (error-message "No further errors"))

	;; In case the compilation buffer is current, make sure we get the
	;; global values of compilation-error-regexp-alist, etc.
	(kill-all-local-variables)

	;; Clear out the compilation buffer and make it writable.
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))

	(compilation-mode)
	(setq buffer-read-only nil)

	(if (boundp 'compilation-parse-errors-function)
	    (set (make-local-variable 'compilation-parse-errors-function) parser))
	(if (boundp 'compilation-error-message)
	    (set (make-local-variable 'compilation-error-message) error-message))
	(set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)

	(when (not (featurep 'xemacs))
	  (dolist (elt `((compilation-enter-directory-regexp-alist
			  ,enter-regexp-alist)
			 (compilation-leave-directory-regexp-alist
			  ,leave-regexp-alist)
			 (compilation-file-regexp-alist
			  ,file-regexp-alist)
			 (compilation-nomessage-regexp-alist
			  ,nomessage-regexp-alist)))
	    (if (boundp (car elt))
		(set (make-local-variable (car elt)) (second elt)))))

	(setq default-directory thisdir)
	(if (boundp 'compilation-directory-stack)
	    (setq compilation-directory-stack (list default-directory)))))))

(defmethod bsh-compilation-buffer-filter ((this bsh-compilation-buffer) proc string)
  "This filter prints out the result of the process without buffering.
The result is inserted as it comes in the compilation buffer."
  (with-current-buffer (oref this buffer)
    (let ((end-of-result (string-match ".*bsh % " string))
	  (win (get-buffer-window (oref this buffer)))
	  output len (status " "))
      (save-excursion
	;;Insert the text, advancing the process marker
	(goto-char (point-max))
	(if end-of-result
	    (progn
	      (setq output (substring string 0 end-of-result))
	      (set-buffer-modified-p nil)

	      ;;Searching backwards for the status code
	      (while (member status '(" " "\r" "\n"))
		(setq len (length output))
		(if (not (= len 0))
		    (progn
		      (setq status (substring output (- len 1)))
		      (setq output (substring output 0 (- len 1))))
		  (progn
		    (setq status (buffer-substring (- (point-max) 1)
						   (point-max)))
		    (delete-region (- (point-max) 1) (point-max)))))

	      (insert output)
	      (compilation-handle-exit
	       'exit (string-to-number status)
	       (if (string= "0" status)
		   "finished\n"
		 (format "exited abnormally with code %s\n"
			 status))))
	  (insert string)))
      (if (not (featurep 'xemacs))
	  (if compilation-scroll-output
	      (save-selected-window
		(if win
		    (progn
		      (select-window win)
		      (goto-char (point-max))))
		(sit-for 0)))))))



(defclass bsh ()
  ((buffer        :initarg :buffer
		  :type bsh-comint-buffer
		  :documentation
		  "Buffer used to interact with BeanShell process.")


   (eval-buffer   :initarg :eval-buffer
		  :type (or null bsh-buffer)
		  :initform nil
		  :documentation
		  "Buffer used to display evaluation result.")

   (eval-filter   :initarg :eval-filter
		  :type function
		  :documentation
		  "Function used to capture Lisp output from the BeanShell.")

   (async-filter  :initarg :async-filter
		  :type function
		  :documentation
		  "Function used to capture and evaluate BeanShell Lisp output.")

   (redir-filter  :initarg :redir-filter
		  :type function
		  :documentation
		  "Redirects BeanShell output to eval-buffer.")

   (java-expr     :initarg :java-expr
		  :initform ""
		  :type string
		  :documentation
		  "Last Java expression evaluated in the BeanShell.")

   (lisp-output   :initarg :lisp-output
		  :initform ""
		  :type string
		  :documentation
		  "Lisp output from the BeanShell.")

   (vm            :initarg :vm
		  :initform "java"
		  :type string
		  :documentation
		  "Path of Java vm used to run the BeanShell.")

   (vm-args       :initarg :vm-args
		  :initform nil
		  :type list
		  :documentation
		  "List of arguments to be passed to the Beanshell vm.")

   (startup-dir   :initarg :startup-dir
		  :initform ""
		  :type string
		  :documentation
		  "Directory in which to start the BeanShell")

   (cp            :initarg :cp
		  :type list
		  :documentation
		  "Startup classpath for BeanShell")

   (jar           :initarg :jar
		  :initform "bsh.jar"
		  :type string
		  :documentation
		  "Path of the BeanShell jar file.")

   (class-name    :initarg :class-name
		  :initform "bsh.Interpreter"
		  :type string
		  :documentation
		  "Name of BeanShell class.")

   (separate-error-buffer :initarg :separate-error-buffer
			  :initform nil
			  :type boolean
			  :documentation
			  "Whether or not to use a separate error buffer."))
"Defines an instance of a BeanShell process.")

(defmethod initialize-instance ((this bsh) &rest fields)
  "Constructor for BeanShell instance."

  (call-next-method)

  (bsh-create-buffer this)

  (lexical-let ((this-bsh this))
    ;; Filter for synchronous evaluations.
    (oset
     this eval-filter
     (lambda (process output)
       (with-current-buffer (process-buffer process)
	 (bsh-snag-lisp-output this-bsh process output))))

    ;; Filter for asynchronous Java statement evaluations.
    (oset
     this async-filter
     (lambda (process output)
       (with-current-buffer (process-buffer process)
	 (bsh-snag-and-eval-lisp-output this-bsh process output))))

    ;; Filter for redirecting result of evaluating an expression to the
    ;; buffer specified by eval-buffer. This is typically used to
    ;; redirect compiler like output to a compilation-mode buffer.
    (oset
     this redir-filter
     (lambda (process output)
       (with-current-buffer (process-buffer process)
	 (bsh-redirect-eval-output this-bsh process output))))))

(defmethod bsh-create-buffer ((this bsh))
  "Creates the buffer used by this beanshell instance."
    (oset this buffer (bsh-comint-buffer "bsh main buffer")))


(defmethod bsh-build-classpath-argument ((this bsh))
  "Convert the list of classes specified by the cp slot
to the string form required by the vm."
  (mapconcat
   'identity
   (append
    (and (slot-boundp this 'cp) (oref this cp))
    (list (oref this jar)))
   path-separator))

(defmethod bsh-running-p ((this bsh))
  "Return t if this instance of a BeanShell is running; otherwise nil"
  (let* ((buffer (oref this buffer))
	 (process
	  (if (slot-boundp buffer  'process)
	      (oref buffer process))))
    (and
     process
     (processp process)
     (eq (process-status process) 'run))))

(defmethod bsh-get-process ((this bsh))
  "Return the Lisp object representing the Beanshell process."
  (if (bsh-running-p this)
      (oref (oref this buffer) process)))

(defmethod bsh-launch ((this bsh) &optional display-buffer)

  (assert
   (or (file-exists-p (oref this vm))
       (executable-find (oref this vm)))
   nil "Specified vm does not exist: %s" (oref this vm))

  (assert
   (file-exists-p (oref this jar))
   nil
   "Specified BeanShell jar filed does not exist: %s" (oref this jar))


  (if (not (bsh-running-p this))
      (let*  ((dir
		 (cond
		  ((not (string= (oref this startup-dir) ""))
		   (expand-file-name (oref this startup-dir)))
		  ((buffer-file-name)
		   (file-name-directory (buffer-file-name)))
		  (t
		   default-directory)))
		(vm-args (list "-classpath" (bsh-build-classpath-argument this)))
		(buffer
		 (let ((buf (oref this buffer)))
		   (if (bsh-buffer-live-p buf)
		       buf
		     (bsh-create-buffer this))))
		(native-buff (oref buffer buffer)))


	  (setq vm-args (append vm-args (oref this vm-args)))
	  (setq vm-args (append vm-args bsh-vm-args))
	  (setq vm-args (append vm-args (list (oref this class-name))))

	  (with-current-buffer native-buff
	    (erase-buffer)

	    (cd dir)
	    (insert (concat "cd " dir "\n"))
	    (insert
	     (concat (oref this vm) " "
		     (mapconcat (lambda (x) x) vm-args " ") "\n\n"))

	    (setq bsh-the-bsh this))

	  (message "%s" "Starting the BeanShell. Please wait...")
	  (bsh-comint-buffer-exec buffer (oref this vm) vm-args)

	  (if display-buffer
	      (bsh-buffer-display buffer)))
    (when display-buffer
      (message "The BeanShell is already running.")
      (bsh-buffer-display (oref this buffer)))))

(defmethod bsh-snag-lisp-output ((this bsh) process output)
  "Assemble Lisp OUTPUT from the BeanShell."
    (let ((end-of-output (string-match ".*bsh % " output)))
      ;; Check for case
      ;;   %bsh\n...eval output...%bsh\n
      ;; This can happen because the beanshell outputs two or more
      ;; prompts after evaluating some expressions.
      ;; Thanks to Stephane Nicolas.
      ;; (if (eq end-of-result 0)
      ;; (accept-process-output process 0 5))
      (if end-of-output
	(oset
	 this
	 lisp-output
	 (concat (oref this lisp-output) (substring output 0 end-of-output)))
	(oset this lisp-output (concat (oref this lisp-output) output))
	(accept-process-output process bsh-eval-timeout 5))))

(defmethod bsh-detect-java-eval-error ((this bsh) bsh-output)
  (if (string-match "// Error:" bsh-output)
      (if (oref this separate-error-buffer)
	  (save-excursion
	    (set-buffer (get-buffer-create "*Beanshell Error*"))
	    (erase-buffer)
	    (insert (format "Expression: %s" (oref this java-expr)))
	    (newline)
	    (insert (format "Error: %s" bsh-output))
	    (goto-char (point-min))
	    (display-buffer (current-buffer))
	    (error "Beanshell eval error."))
	(message
	 "Beanshell expression evaluation error.\n  Expression: %s\n  Error: %s"
	 (oref this java-expr) bsh-output)
	(error "Beanshell eval error. See messages buffer for details."))))

(defmethod bsh-eval-lisp-output ((this bsh))
  (if (not (string= (oref this lisp-output) ""))
      (flet ((format-error-msg (error-symbols)
	       (mapconcat (lambda (s) (symbol-name s)) error-symbols " ")))
	(condition-case eval-error
	    (eval (read (oref this lisp-output)))
	  (error
	    (message "Error evaluating Lisp result of Java expression evaluation.")
	    (message "  Java expression: %s." (oref this java-expr))
	    (message "  Java evaluation result: %s." (oref this lisp-output))
	    ;; The following causes an unreadable object error on XEmacs when
	    ;; trying to load the byte-compiled file:
	    ;;
	    ;; (message "  Error from evaluating result as Lisp: %s"
	    ;;	  (mapconcat (lambda (s) (symbol-name s)) eval-error " ")
	    (message "  Error from evaluating result as Lisp: %s"
		     (format-error-msg eval-error))
	    (error "Error evaluating Java expresson. See *Messages* buffer."))))
    (progn
      (message "bsh-eval-r error: Beanshell result is null. Cannot evaluate.")
      (message "  Expression: %s" (oref this java-expr)))))

(defmethod bsh-snag-and-eval-lisp-output ((this bsh) process output)
  "Assemble and parse Lisp OUTPUT from the BeanShell PROCESS."
  (bsh-snag-lisp-output this process output)
  (bsh-detect-java-eval-error this output)
  (bsh-eval-lisp-output this))


(defmethod bsh-redirect-eval-output ((this bsh) process output)
  (let ((eval-buffer (oref this eval-buffer)))
    (assert eval-buffer)
    (funcall (oref eval-buffer filter) process output)
    ;; Restore the Beanshell's standard output filter when
    ;; the Beanshell prompt reappears.
    (if (string-match ".*bsh % " output)
       (progn
	(set-process-filter process (oref (oref this buffer) filter))
	 (oset this eval-buffer nil)))))

(defmethod bsh-eval ((this bsh) expr &optional eval-return)
  "Uses the BeanShell Java interpreter to evaluate the Java expression
EXPR.  This function returns any text output by the Java interpreter's
standard out or standard error pipes.  If EVAL-RETURN is non-nil, this
function returns the result of evaluating the Java output as a Lisp
expression."

  (unless (bsh-running-p this)
    (bsh-launch this))

  (when (bsh-running-p this)
    (oset this java-expr expr)
    (oset this lisp-output "")
    (set-process-filter (oref (oref this buffer) process) (oref this eval-filter))
    ;; (message "Evaluating: %s" expr)
    (process-send-string (oref (oref this buffer) process) (concat expr "\n"))

    (if (not (accept-process-output (oref (oref this buffer) process) bsh-eval-timeout))
	(progn
	  (set-process-filter (oref (oref this buffer) process) (oref (oref this buffer) filter))
	  (error "No reply from BeanShell")))

    (set-process-filter (oref (oref this buffer) process) (oref (oref this buffer) filter))

    (bsh-detect-java-eval-error this (oref this lisp-output))
    ;; (if eval-return (message "Evaluating reply: %s" (oref this lisp-output)))
    (if eval-return
	(bsh-eval-lisp-output this)
      (oref this lisp-output))))

(defmethod bsh-eval-r ((this bsh) java-statement)
  "Convenience method for evaluating Java statements
that return Lisp expressions as output. This function
invokes bsh-eval with the evaluate-return option set to
t."
  (bsh-eval this java-statement t))

(defmethod bsh-async-eval ((this bsh) expr)
  "Send the Java statement EXPR to the BeanShell for
evaluation. Do not wait for a response."
  (unless (bsh-running-p this)
    (bsh-launch this))

  (oset this lisp-output "")
  (oset this java-expr expr)
  (set-process-filter (oref (oref this buffer) process) (oref this async-filter))

  (when (bsh-running-p this)
    (process-send-string (oref (oref this buffer) process) (concat expr "\n"))))

(defmethod bsh-buffer-eval ((this bsh) expr buffer)
  "Evaluate EXPR and displays its output in BUFFER. BUFFER
must be an instance of `bsh-buffer' class. A typical use for this
method is to invoke a Java application whose output is compiler-like,
e.g., javac, ant, or checkstyle. In this case, BUFFER would be
an instance of a subclass of `bsh-compiler-buffer'."
  (assert (typep expr 'string))
  (assert (typep buffer 'bsh-buffer))
  (unless (bsh-running-p this)
    (bsh-launch this))
  (let* ((comint-buffer (oref this buffer))
	 (bsh-process (oref comint-buffer process)))
    (oset this eval-buffer buffer)
    (set-process-filter bsh-process (oref this redir-filter))
    (process-send-string bsh-process expr)))

(defmethod bsh-kill-process ((this bsh))
  "Terminates the BeanShell process."
  (process-send-string  (oref (oref this buffer) process) "exit();\n"))

(defun get-bsh(buffer-name)
  "Get the `bsh' object associated
with the buffer named BUFFER-NAME."
  (let (bsh-object
	(buffer (get-buffer buffer-name)))
    (if buffer
	(save-excursion
	  (set-buffer buffer)
	  (setq bsh-object bsh-the-bsh)))
    bsh-object))


(defun bsh-demo-eval ()
  "This command demonstrates use of the BeanShell to
execute Java code. It assumes that
`bsh-jar' points to the location of the BeanShell
jar file on your system and that either `exec-path'
or `bsh-vm' point to a Java vm on your system."
   (interactive)

   (assert
    (file-exists-p (expand-file-name bsh-jar))
    nil
    (concat
     "Cannot find the BeanShell jar file at "
     (expand-file-name bsh-jar)
     ". Type C-h bsh-jar for more info."))

   (if bsh-vm
       (assert
	(or
	 (executable-find bsh-vm)
	 (file-exists-p bsh-vm))
	nil
	"The vm specified by bsh-vm does not exist: %s." bsh-vm)
     (assert
      (executable-find (if (eq system-type 'windows-nt) "javaw" "java"))
      nil
      "Cannot find a Java vm on exec-path."))


   (let ((beanshell  (get-bsh "*bsh demo*"))
	 (demo-script
	  (concat
	   "demo() {"
	     "name = JOptionPane.showInputDialog(\"Enter your name.\");"
	     "print(\"(message \\\"Your name is \" + name + \"\\\")\");"
	   "};")
	 ))

     (if (not beanshell)
	 (progn

	   ;; Create a BeanShell wrapper object
	   (setq beanshell (bsh "BeanShell"))

	   (oset (oref beanshell buffer) buffer-name "*bsh demo*")

	   ;; Set the wrapper's jar slot (eieio/CLOS speak for field) to
	   ;; point to the location of the BeanShell on the user's
	   ;; system.
	   (oset beanshell jar (expand-file-name bsh-jar))

	   ;; Set the wrapper's vm slot to point to the location
	   ;; of a vm on the user's system. The wrapper's launch
	   ;; method (see below) uses the specified vm to launch
	   ;; the BeanShell.
	   (oset
	    beanshell
	    vm
	    (if bsh-vm
		(or
		 (executable-find bsh-vm)
		 bsh-vm)
	      (executable-find (if (eq system-type 'windows-nt) "javaw" "java"))))))

     (unless (bsh-running-p beanshell)
       (bsh-launch beanshell)
       (bsh-async-eval beanshell demo-script))
     (bsh-async-eval beanshell "demo();")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standalone BeanShell                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bsh-standalone-bsh (bsh)
  ((the-bsh        :type bsh-standalone-bsh
		   :allocation :class
		   :documentation
		   "The single instance of the standalone BeanShell."))
  "BeanShell intended to be used independently of any other
Emacs package.")

(defmethod initialize-instance ((this bsh-standalone-bsh) &rest fields)
  "Constructor for the standard bsh BeanShell instance."
  (call-next-method)

  (assert
   (file-exists-p (expand-file-name bsh-jar))
   nil
   (concat
    "Cannot find the BeanShell jar file at "
    (expand-file-name bsh-jar)
    ". Type C-h bsh-jar for more info."))

   (if bsh-vm
       (assert
	(or
	 (executable-find bsh-vm)
	 (file-exists-p bsh-vm))
	nil
	"The vm specified by bsh-vm does not exist: %s." bsh-vm)
     (assert
      (executable-find (if (eq system-type 'windows-nt) "javaw" "java"))
      nil
      "Cannot find a Java vm on exec-path."))


  (oset this jar (expand-file-name bsh-jar))

  (oset
   this
   vm
   (if bsh-vm
       (or
	(executable-find bsh-vm)
	bsh-vm)
     (executable-find (if (eq system-type 'windows-nt) "javaw" "java"))))

  (oset this cp bsh-classpath)

  (oset this startup-dir bsh-startup-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beanshell commands                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bsh-run()
  "*Starts the standalone version of the BeanShell, a Java interpreter developed
by Pat Niemeyer."
  (interactive)
  (oset-default 'bsh-standalone-bsh the-bsh (bsh-standalone-bsh "Standalone BeanShell"))
  (bsh-launch (oref bsh-standalone-bsh the-bsh) t))

(defun bsh-exit ()
  "Closes the standalone version of the BeanShell."
  (interactive)
  (let ((bsh
	 (if (slot-boundp 'bsh-standalone-bsh 'the-bsh)
	     (oref 'bsh-standalone-bsh the-bsh))))
    (if (and bsh (bsh-running-p bsh))
	(bsh-kill-process bsh)
      (message "The beanshell is not running"))))


(defun bsh-find-bsh-data-directory ()
  "Return the path of the bsh data directory.
Returns the path of the directory containing the documentation directory;  nil if the
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions,
the bsh expects to find the documentation
in the same directory that contains the bsh.el file."
  (let (dir)
    (flet ((find-data-dir
	    ()
	    (expand-file-name
	     "../"
	     (file-name-directory (locate-library "beanshell")))))
      (if (featurep 'xemacs)
	  (progn
	    (setq dir (locate-data-directory "beanshell"))
	    (if (not dir)
		(setq dir (find-data-dir))))
	(setq dir (find-data-dir))))
      dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beanshell mode                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode
  bsh-script-mode java-mode "bsh script"
  "Major mode for developing Beanshell scripts.
  \\(bsh-script-mode-map)"

  (set (make-local-variable 'font-lock-defaults)
       (if (featurep 'xemacs)
	   (get 'java-mode 'font-lock-defaults)
	 (cdr (assq 'java-mode font-lock-defaults))))
  (set (make-local-variable 'font-lock-maximum-decoration) t)
  (set (make-local-variable 'font-lock-multiline) t)
  (turn-on-font-lock))

(add-to-list 'auto-mode-alist '("\\.bsh\\'" . bsh-script-mode))

;;;###autoload
(defun bsh-script-help ()
  "Display BeanShell User's Guide."
  (interactive)
  (let* ((bsh-dir (bsh-find-bsh-data-directory))
	 (bsh-help
	  (if bsh-dir
	      (expand-file-name "bsh-ug.html" bsh-dir))))
    (if (and
	 bsh-help
	 (file-exists-p bsh-help))
	(browse-url (concat "file://" bsh-help)
		    (if (boundp 'browse-url-new-window-flag)
			browse-url-new-window-flag))
      (signal 'error '("Cannot find BeanShell help file.")))))

(defcustom bsh-script-menu-definition
  (list "Bsh"
	["Help" bsh-script-help t])
  "Definition of menu for BeanShell script buffers."
  :group 'bsh
  :type 'sexp
  :set '(lambda (sym val)
	  (set-default sym val)
	  ; Define Bsh script menu for FSF Emacs.
	  (if (or (not (featurep 'xemacs))
		  (featurep 'infodock))
	      (easy-menu-define bsh-script-menu
				bsh-script-mode-map
				"Menu for BeanShell Script Buffer."
				val))
	  (if (and (featurep 'xemacs)
		   (eq major-mode 'bsh-script-mode))
	      (bsh-script-insert-menu-in-xemacs-menubar))))

(defun bsh-script-insert-menu-in-xemacs-menubar ()
  "Insert BeanShell script menu in the XEmacs menu bar."
  (if (and
       (not (featurep 'infodock))
       (boundp 'c-emacs-features)
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil bsh-script-menu-definition)
	(add-menu nil "Bsh" (cdr bsh-script-menu-definition)))))

(provide 'beanshell)

;; End of beanshell.el
