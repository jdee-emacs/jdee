;;; beanshell.el
;; $Revision: 1.76 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>
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
	(w32-quote-process-args ?\") ;; Emacs
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
  (process-kill-without-query (oref this process))

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

	;; In case the compilation buffer is current, make sure we get the global
	;; values of compilation-error-regexp-alist, etc.
	(kill-all-local-variables)

	;; Clear out the compilation buffer and make it writable.
	(setq buffer-read-only nil)
	(buffer-disable-undo (current-buffer))
	(erase-buffer)
	(buffer-enable-undo (current-buffer))

	(compilation-mode)
        (setq buffer-read-only nil)

	(set (make-local-variable 'compilation-parse-errors-function) parser)
	(set (make-local-variable 'compilation-error-message) error-message)
	(set (make-local-variable 'compilation-error-regexp-alist)
	     error-regexp-alist)

	(if (not (featurep 'xemacs))
	    (progn
	      (set (make-local-variable 'compilation-enter-directory-regexp-alist)
		   enter-regexp-alist)
	      (set (make-local-variable 'compilation-leave-directory-regexp-alist)
		   leave-regexp-alist)
	      (set (make-local-variable 'compilation-file-regexp-alist)
		   file-regexp-alist)
	      (set (make-local-variable 'compilation-nomessage-regexp-alist)
		   nomessage-regexp-alist)))

	(setq default-directory thisdir)
	(setq compilation-directory-stack (list default-directory))))))

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
                  "Name of BeanShell class."))
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

(defvar bsh-the-bsh nil
  "The BeanShell instance associated with the current BeanShell buffer.")
(make-variable-buffer-local 'bsh-the-bsh)


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
      (progn
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
  (let ((directory-sep-char ?/)
	dir)
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
			'browse-url-new-window-flag
		      browse-url-new-window-p))
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
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil bsh-script-menu-definition)
	(add-menu nil "Bsh" (cdr bsh-script-menu-definition)))))

  

(provide 'beanshell)

;; $Log: beanshell.el,v $
;; Revision 1.76  2005/01/23 00:02:39  jslopez
;; Set buffer-read-only to nil after invoking compilation-mode, which sets it to t
;; in latest version of Emacs.
;;
;; Revision 1.75  2004/12/22 04:20:14  paulk
;; Fix bsh-exit to handle case where the standalone beanshell is not running.
;;
;; Revision 1.74  2004/10/14 04:32:43  paulk
;; Set process-connection-type to nil (always uses pipes for process I/O)
;; when starting a beanshell comint buffer. This change is intended to
;; avoid code 129 errors that occur on some versions of Linux.
;;
;; Revision 1.73  2004/06/06 04:31:08  paulk
;; Fixed reference to obsolete font-lock variable.
;;
;; Revision 1.72  2004/06/03 02:54:43  paulk
;; Remove unnecessary format clauses in assert forms.
;;
;; Revision 1.71  2004/05/04 03:59:23  paulk
;; Eliminate optional argument to compilation-mode to ensure compatiblity with next release of Emacs.
;;
;; Revision 1.70  2004/04/30 02:38:16  paulk
;; Makes beanshell not depend on compilation- variables being defined.
;;
;; Revision 1.69  2004/03/21 15:05:02  paulk
;; The beanshell redirect filter now restores the standard filter only when the Beanshell prompt reappears. This ensures that the redirect filter redirects the entire output of the previous Beanshell expression evaluation. Thanks to JÅˆrg Mensmann.
;;
;; Revision 1.68  2003/06/30 03:52:39  paulk
;; Include bsh-vm-args in the command line that starts the BeanShell, thus fixing a
;; regression error reported by TAKAHASHI Toru.
;;
;; Revision 1.67  2003/06/22 16:06:43  paulk
;; Fix "unreadable object" error when XEmacs tries to load beanshell.elc.
;;
;; Revision 1.66  2003/05/07 04:29:37  paulk
;; Include Lisp error in messages buffer when a Lisp error results from evaluating the result of a Java evaluation as Lisp.
;;
;; Revision 1.65  2003/04/16 04:13:18  paulk
;; Added a require statement for the comint package. This is not necessary for use with the JDEE, which already requires comint, but may be necessary when beanshell.el is used independently of the JDEE.
;;
;; Revision 1.64  2003/04/11 22:51:33  jslopez
;; Fixes typo, orer to oref.
;;
;; Revision 1.63  2003/04/08 03:14:07  paulk
;; Fixes bug that caused the working directory of the compilation buffer to be set incorrectly. Also add methods to bsh-compilation-buffer class that simplify subclassing.
;;
;; Revision 1.62  2003/04/06 07:32:25  paulk
;; Fixed bugs caused by not handling the case when the user has killed a
;; Beanshell buffer. Also added a bsh-create-buffer method to allow
;; derived classes to override the type of buffer used to interact
;; with the BeanShell.
;;
;; Revision 1.61  2003/03/04 10:10:40  paulk
;; Made the following changes to provide generalized support
;; for displaying BeanShell output in a compilation-mode buffer.
;;
;; - Defined a new eieio class hierarchy that wraps Emacs buffers.
;;   The hierarchy contains three classes: bsh-buffer, the base,
;;   and bsh-comint-buffer and bsh-compilation-buffer class, which
;;   wrap comint-mode and compilation-mode buffers, respectively.
;;
;; - Added a bsh-buffer-eval method to the bsh class that displays
;;   the results of evaluating a Java expression in a specified
;;   bsh-buffer.
;;
;; Revision 1.60  2003/02/17 08:09:29  paulk
;; Refactors beanshell.el so that it supports but does not require the
;; JDEE. This allows beanshell.el to be used to support other Emacs
;; applications that want to use Java as an extension language. See
;; the package commentary for more information.
;;
;; Revision 1.59  2003/02/15 20:56:53  nsieger
;; (bsh-eval): Add second optional IGNORE-EMPTY parameter which
;; suppresses errors if evaluation produces no forms.  Part of initial
;; JUCI checkin, which necessitated this feature.
;;
;; Revision 1.58  2003/01/27 06:38:49  paulk
;; Moved error-handling for when jde-get-tools-jar returns a nil value into
;; jde-get-tools-jar itself.
;;
;; Revision 1.57  2003/01/20 04:43:47  paulk
;; Slightly expanded error message for not being able to find the JDK tools jar file.
;;
;; Revision 1.56  2003/01/09 18:21:14  ahyatt
;; Beanshell should be started with an expanded classpath
;;
;; Revision 1.55  2003/01/03 16:35:24  jslopez
;; Fixes bug that was not adding the ant jar files to the beanshell classpath
;; when jde-ant-home is not bound. This could happen because the jde-ant
;; package has not been loaded.
;;
;; Revision 1.54  2002/12/30 05:09:00  paulk
;; Define bsh-script-mode for editing BeanShell scripts.
;;
;; Revision 1.53  2002/12/14 03:54:09  jslopez
;; Adds all the jar files in the ant/lib directory to the beanshell classpath.
;;
;; Revision 1.52  2002/12/06 03:47:37  ahyatt
;; Changes to support Mac OS X, which does not use tools.jar
;;
;; Revision 1.51  2002/11/14 06:03:34  paulk
;; Fix regression bug in bsh-exit caused by deferring loading of ant package until used.
;;
;; Revision 1.50  2002/10/31 05:19:34  paulk
;; Applied compatibility fix for Mac OS X. Thanks to Andi Vajda <avajda@nanospace.com>.
;;
;; Revision 1.49  2002/10/22 04:41:19  paulk
;; Checks whether jde-ant-home is bound. This is necessary because jde-ant is now autoloaded and hence jde-ant-home is not bound until the user executes an ant command.
;;
;; Revision 1.48  2002/09/16 05:05:59  paulk
;; Cygwin Emacs compatibility fix. Check for Cygwin Emacs when processing paths. Thanks
;; to Klaus Berndl.
;;
;; Revision 1.47  2002/09/11 03:32:56  paulk
;; If jde-devel-debug is true, the BeanShell now uses the JDEE classes
;; in the jde/java/classes directory instead of those in the
;; jde/java/lib/jde.jar file. This simplifies testing changes
;; to the JDEE classes.
;;
;; Revision 1.46  2002/09/10 04:44:31  paulk
;; The JDEE now waits for the BeanShell startup message
;; on Windows. Previously, it returned possibly creating
;; subtle timing bugs.
;;
;; - Paul
;;
;; Revision 1.45  2002/08/27 05:03:30  paulk
;; Put the JDEE libraries ahead of jde-global-classpath in the beanshell classpath to ensure that the
;; JDEE versions of these libraries are loaded and not other versions that the user may have installed
;; in their classpath. Thanks to Andy Piper.
;;
;; Revision 1.44  2002/06/22 05:52:18  paulk
;; Fixed minor compilation error.
;;
;; Revision 1.43  2002/06/18 06:02:51  paulk
;; XEmacs compatibility fix: allow user to set bsh-startup-timeout
;; and bsh-eval-timeout to floating point values and to nil
;; (no timeout) to accommodate an apparent bug in the way
;; accept-process-output works on the Windows version of XEmacs.
;;
;; Revision 1.42  2002/06/12 07:04:31  paulk
;; XEmacs compatibility fix: set win32-quote-process-args wherever
;; the JDEE sets w32-quote-process-args. This allows use of spaces in
;; paths passed as arguments to processes (e.g., javac)  started by
;; the JDEE.
;;
;; Revision 1.41  2002/06/11 06:30:37  paulk
;; Provides support for paths containing spaces as beanshell vm arguments via the following change:
;; locally set the w32-quote-process-args variable to a quotation mark when launching
;; the beanshell vm process.
;;
;; Revision 1.40  2002/02/26 04:35:27  paulk
;; * Fixes regression bug that calls bsh-eval to fail when ant-home is nil.
;; * Adds a new eieo class, bsh-compiler, intended to serve as a base class
;;   for compiler-like tools (e.g., javac, ant, checkstyle) based on
;;   the beanshell.
;;
;; Revision 1.39  2002/02/25 20:07:38  jslopez
;; Remove no buffering filter.
;; Add get-process method.
;;
;; Revision 1.38  2002/02/21 12:42:33  jslopez
;; Fixes bug adding incorrect path for ant.jar in
;; the beanshell classpath.
;;
;; Revision 1.37  2002/02/15 17:50:46  jslopez
;; Removes reference to bsh-eval-comint-filter.
;; Switch the order of the sit-for and the accept-process-output.
;;
;; Revision 1.36  2002/02/15 02:48:20  jslopez
;; Adds a new non buffering filter, this filter is meant to be used
;; by the compile server and the ant server.
;; Modifies bsh-eval to support a new filter.
;;
;; Revision 1.35  2001/12/09 17:17:48  jslopez
;; Replaces repetitive code with jde-get-global-classpath.
;;
;; Revision 1.34  2001/11/05 14:18:32  jslopez
;; Modifies bsh-exit to use two different exit methods.
;; If Ant Server is enable it uses JdeUtilities.exit()
;; otherwise it uses the beanshell exit() method.
;;
;; Revision 1.33  2001/11/05 02:13:59  jslopez
;; Modified bsh-exit to class jde.util.JdeUtilities.exit() instead
;; of using the beanshell exit method.
;;
;; Revision 1.32  2001/10/24 05:27:56  paulk
;; Updated bsh-internal to use jde-run-get-vm (instead of the obsolete
;; jde-run-java-vm) to get the path to the vm to run the Beanshell.
;;
;; Revision 1.31  2001/10/19 09:47:55  paulk
;; XEmacs compatibility fix: Now correctly converts new lines (^M) on Windows.
;; Thanks to Andy Piper.
;;
;; Revision 1.30  2001/09/05 06:18:05  paulk
;; bsh-internal now uses jde-get-jdk-directory to determine the JDK directory.
;;
;; Revision 1.29  2001/08/30 04:15:06  paulk
;; Beanshell now uses jde-jdk-directory instead of jde-bug-jdk-directory (soon to be deprecated) to find the JDK tools.jar file.
;;
;; Revision 1.28  2001/08/30 01:34:47  paulk
;; Adds JDK tools.jar file to beanshell classpath. Needed to support compile server. Thanks to Javier Lopez.
;;
;; Revision 1.27  2001/08/14 06:11:35  paulk
;; Add bsh-exit, bsh-open-class-browser, and bsh-open-desktop. Thanks to Javier Lopez.
;;
;; Revision 1.26  2001/07/31 05:11:48  paulk
;; ReleaseNotes.txt
;;
;; Revision 1.25  2001/06/13 03:51:44  paulk
;; Now defines bsh customization group.
;;
;; Revision 1.24  2001/05/31 05:14:38  paulk
;; Provide support for per-project caching of class data in the Beanshell. Thanks to Matt Conway.
;;
;; Revision 1.23  2001/05/19 02:39:18  paulk
;; Put jde-global-classpath first on the classpath to facilitate debugging of Java code run in hthe Beanshell.
;;
;; Revision 1.22  2001/04/16 05:33:20  paulk
;; Normalized paths. Thanks to Nick Sieger.
;;
;; Revision 1.21  2001/03/21 20:46:34  paulk
;; Updated bsh-internal to handle case where both jde-global-classpath and CLASSPATH environment variable are nil. Thanks to Toru TAKAHASHI <torutk@alles.or.jp> for reporting this bug and supply an initial version of a fix.
;;
;; Revision 1.20  2001/03/01 05:01:28  paulk
;; Adds the customization variable bsh-startup-directory.
;;
;; Revision 1.19  2001/02/25 04:23:12  paulk
;; Fixed bug in processing CLASSPATH environment variable.
;;
;; Revision 1.18  2001/02/03 07:44:26  paulk
;; Now uses jde-build-classpath to build BeanShell classpath. This allows enviromnent variables in the classpath.
;;
;; Revision 1.17  2000/10/08 12:55:38  paulk
;; *** empty log message ***
;;
;; Revision 1.16  2000/08/10 09:09:47  paulk
;; Now handles Lisp eval errors gracefully.
;;
;; Revision 1.15  2000/08/07 05:11:38  paulk
;; Adds bsh-vm-args variable.
;;
;; Revision 1.14  2000/08/04 02:51:19  paulk
;; Added bsh-eval-timeout variable.
;;
;; Revision 1.13  2000/02/16 04:39:28  paulk
;; Implemented Cygwin/XEmacs compatiblity fix provided by Fred Hart
;; <cfhart@Z-TEL.com> in bsh-internal.
;;
;; Revision 1.12  2000/02/02 05:51:00  paulk
;; Expanded doc string.
;;
;; Revision 1.11  2000/01/28 04:28:00  paulk
;; Fixed startup timing bug that cause commands that use the beanshell to
;; failt the first time on Unix systems.
;;
;; Revision 1.10  2000/01/15 08:00:03  paulk
;; Corrected typo.
;;
;; Revision 1.9  1999/11/01 03:13:07  paulk
;; No change.
;;
;; Revision 1.8  1999/09/17 06:55:26  paulk
;; Set comint-prompt-regexp to the beanshell prompt.
;; Fixed bug where Emacs was querying user whether to kill the beanshell
;; buffer on exit from Emacs.
;;
;; Revision 1.7  1999/01/15 22:18:41  paulk
;; Added Andy Piper's NT/XEmacs compatibility changes.
;;
;; Revision 1.6  1998/12/13 22:10:04  paulk
;; Add check for chunked traffic between Emacs and the BeanShell.
;;
;; Revision 1.5  1998/12/09 00:59:43  paulk
;; Added a startup message for beanshell.
;;
;; Revision 1.4  1998/11/27 10:07:57  paulk
;; Use CLASSPATH environment variable if jde-global-classpath is nil.
;;
;; Revision 1.3  1998/11/22 23:14:28  paulk
;; Fixed path separator bug.
;;
;; Revision 1.2  1998/11/22 18:11:56  paulk
;; Changed path to use jde.jar.
;;
;; Revision 1.1  1998/10/22 00:07:56  paulk
;; Initial revision
;;


;; End of beanshell.el
