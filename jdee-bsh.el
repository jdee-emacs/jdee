;;; jdee-bsh.el -- Beanshell JDEE integration.

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes
;; Keywords: java, tools, bsh, beanshell

;; Copyright (C) 1997-2008 Paul Kinnucan.
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

;; This library has beanshell specific functionality.

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'eieio)
(require 'jdee-classpath)
(require 'beanshell)
(require 'jdee-parse-expr)
(require 'jdee-util)

;; FIXME: there is no cl-lexical-let
;; Change file to lexical-binding t
(eval-when-compile
  '(require 'cl))

;; FIXME: refactor to eliminate these
(defvar jdee-current-project);; jdee-project-file.el
(declare-function jdee-get-project "jdee-project-file" (symbol project));;
(declare-function jdee-run-get-vm "jdee-run" ())
(declare-function jdee-get-tools-jar "jdee-jdk-manager" ())
;; FIXME: remove this ugly dep:
(declare-function jdee-backend-create-prj-values-str "jdee-backend" ())

;; Avoid recursive requires, where a plugin might require this file
(autoload 'jdee-pi-get-bsh-classpath "jdee-plugins")

(defcustom jdee-bsh-separate-buffer nil
  "*Whether or not to use a separate buffer for errors."
  :group 'jdee
  :type 'boolean)

(defcustom jdee-server-dir ""
  "Path to JDEE Java Backend JARs.
See https://github.com/jdee-emacs/jdee-server"
  :group 'jdee
  :type 'directory)

(defcustom jdee-devel-debug nil
  "If true, use the JDEE Java classes in the jde/java/classes
directory instead of the jde.jar. This variable is intended for
use in testing the JDEE's java classes."
  :group 'jdee-project
  :type 'boolean)

(defclass jdee-bsh-buffer (bsh-comint-buffer) ()
  "JDEE's beanshell buffer")

(defmethod initialize-instance ((this jdee-bsh-buffer) &rest fields)
  (oset this buffer-name "*JDEE bsh*")
  (call-next-method))

(defclass jdee-bsh (bsh)
  ((bsh-cmd-dir      :initarg :bsh-cmd-dir
		     :type string
		     :documentation
		     "Path of the BeanShell commmand directory.")

   (jdee-jar         :initarg :jdee-jar
		    :type string
		    :documentation
		    "Path of the JDEE jar.")

   (jdee-classes-dir :initarg :jdee-classes-dir
		    :type string
		    :documentation
		    "Path of the JDEE classes directory.")

   (the-bsh        :type jdee-bsh
		   :allocation :class
		   :documentation
		   "The single instance of the JDEE's BeanShell."))
  "Class of JDEE BeanShells. There is only one per Emacs session.")

(defmethod initialize-instance ((this jdee-bsh) &rest fields)
  "Constructor for the JDEE BeanShell instance."
  (call-next-method)

  (let* ((jdee-java-directory jdee-server-dir))
    (oset this bsh-cmd-dir (expand-file-name "bsh-commands" jdee-java-directory))
    (oset this jdee-classes-dir (expand-file-name "classes" jdee-java-directory))
    (oset this jdee-jar (expand-file-name "jde.jar" jdee-java-directory))
    (oset this jar  (expand-file-name "bsh.jar" jdee-java-directory))
    (oset this separate-error-buffer jdee-bsh-separate-buffer)
    (oset-default 'jdee-bsh the-bsh this)))

(defmethod bsh-create-buffer ((this jdee-bsh))
  "Creates the JDEE's beanshell buffer."
  (oset this buffer (jdee-bsh-buffer "JDEE bsh buffer")))

(defmethod bsh-build-classpath-argument ((this jdee-bsh))
  (jdee-build-classpath (oref this cp) 'jdee-global-classpath t))

(defmethod bsh-launch :BEFORE ((this jdee-bsh) &optional display-buffer)
  "Sets the vm and classpath to the vm and classpath for the current project before
the PRIMARY launch method is invoked."
  (let* ((project-ant-home
	  ;; Code referring to jdee-ant variables uses symbols to
	  ;; avoid causing compilation errors since jdee-ant is not required.
	  (jdee-get-project 'jdee-ant-home jdee-current-project))
	 (ant-home (if (and (boundp 'jdee-ant-home)
			    (not (string= (symbol-value 'jdee-ant-home) "")))
		       (symbol-value 'jdee-ant-home)     ;jdee-ant loaded
		     (if (and project-ant-home
			      (not (string= project-ant-home "")))
			 project-ant-home ; jdee-ant not loaded but
					; jdee-ant-home set in project
					; file
		       (getenv "ANT_HOME")))) ; jdee-ant-home not set in
					; project file and not
					; customized
	 )

    (oset this vm (oref (jdee-run-get-vm) :path))
    (oset this cp (delq
                   nil
                   (append
                    (list
                     (if jdee-devel-debug
                         (oref this jdee-classes-dir))
                     (jdee-get-tools-jar)
                     (if ant-home (expand-file-name "lib" ant-home)))
                    (directory-files jdee-server-dir t ".*\\.jar")
                    (jdee-pi-get-bsh-classpath)
                    (jdee-expand-classpath (jdee-get-global-classpath)))))))

;; Create the BeanShell wrapper object.
(jdee-bsh "JDEE BeanShell")

(defun jdee-bsh-running-p ()
  "Returns t if the JDEE's BeanShell instance is running."
  (bsh-running-p (oref-default 'jdee-bsh the-bsh)))

(defvar java-bsh-read-java-expression-history nil)

(defun jdee-bsh-read-java-expression ()
  "Read an expression as input guessing initial input at the current point."
  (if mark-active
      (progn
	(setq java-bsh-read-java-expression-history
	      (cons (buffer-substring (region-beginning)
				      (region-end))
		    java-bsh-read-java-expression-history))
	(jdee-bsh-quote-expr (region-beginning) (region-end) t t))
    (let ((bnd (if (eq major-mode 'jdee-mode)
		   (bounds-of-thing-at-point 'java-expression)))
	  initial)
      (if bnd (setq initial (buffer-substring (car bnd) (cdr bnd))))
      (if (and initial
	       (or (>= (length initial) 80)
		   (save-match-data (string-match "\n" initial))))
	  (setq initial nil))
      (read-string "Expression: " initial
		   'java-bsh-read-java-expression-history))))

(defvar jdee-jeval-debug nil
  "*Whether or not turn on debug logging.
This logs requests and responses to *Bsh Debug Log*")

;;;###autoload
(defun jdee-jeval (java-statement &optional eval-return no-print-p)
  "Uses the JDEE's instance of the BeanShell
Java interpreter to evaluate the Java expression EXPR.  If the
BeanShell is not running, the JDEE starts an instance. This function
returns any text output by the Java interpreter's standard out or
standard error pipes.  If EVAL-RETURN is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression.

NO-PRINT-P, if non-nil, don't wrap JAVA-STATEMENT with a `print'
command yeilding the output.  This is going to need to be true
for most things since unless `show()' was invoked and output
prints out, Emacs has nothing to evaluate or report."
  (interactive (list (jdee-bsh-read-java-expression)))
  (cl-flet ((log
	     (msg logtype)
	     (when jdee-jeval-debug
	       (with-current-buffer (get-buffer-create "*Bsh Debug Log*")
		 (goto-char (point-max))
		 (insert (format "%S<" logtype))
		 (insert (if (stringp msg) msg (prin1-to-string msg)))
		 (insert ">")
		 (newline)))))
    (let ((the-bsh (oref-default 'jdee-bsh the-bsh)))
      (when (not (bsh-running-p the-bsh))
	(bsh-launch the-bsh)
	(bsh-eval the-bsh (jdee-backend-create-prj-values-str)))
      (when (not no-print-p)
	(if (string= (substring java-statement -1) ";")
	    (setq java-statement (substring java-statement 0 -1)))
	(setq java-statement (format "\
{
  boolean _prevShowValue = this.interpreter.getShowResults();
  Object _retVal = null;
  this.interpreter.setShowResults(false);
  _jdeCustEvalFn() { %s; };
  try { _retVal = eval(\"_jdeCustEvalFn();\"); }
  finally {
    this.interpreter.setShowResults(_prevShowValue);
  }
  if (_retVal != null) print(_retVal);
}" java-statement)))
      (log java-statement 'request)
      (let ((output (bsh-eval the-bsh java-statement eval-return))
	    len)
	(when (stringp output)
	  (when (> (length output) 0)
	    (setq len (length output))
	    (if (eq ?\n (elt output (1- len)))
		(setq output (substring output 0 (1- len)))))
	  (if (= 0 (length output)) (setq output nil)))
	(log output 'response)
	(when (called-interactively-p 'interactive)
	  (if output (kill-new output))
	  (message (if output
		       (concat "Copied `"
			       (replace-regexp-in-string "%" "%%" output t t)
			       "'")
		     "No result")))
	output))))

(defun jdee-jeval-r (java-statement)
  "Uses the JDEE's instance of the BeanShell to
evaluate JAVA-STATEMENT and then uses the Emacs Lisp
interpreter to evaluate the result. This function
is intended to be used to implement Emacs extensions
coded in Java and executed by the BeanShell. The function
assumes that the Java extension interacts with Emacs
by printing Lisp forms to the BeanShell's standard output \
port."
  (jdee-jeval java-statement t))

(defun jdee-jeval-cm (java-expr &optional buffer-head finish-fcn)
  "Evaluate JAVA-EXPR and display the result in a compilation-mode buffer.
The optional argument BUFFER-HEAD specifies text to appear at the head of
the compilation buffer. The optional argument FINISH-FCN specifies a
function to be called when the compilation is finished. This function
is intended to be used to invoke Java development utilities, such as
source code style checkers, that emit compiler-like error messages.
Displaying the output in a compilation-mode buffer enables the user to
use compilation-mode's error message navigation and hyperlinking
capabilities.

The following example uses this function to invoke the javac compiler on
a file in the current directory:

 (jdee-bsh-compile-mode-eval \"jde.util.CompileServer.compile(\\\"Test.java\\\");\"
   \"Compile Test.java\" 'jdee-compile-finish-kill-buffer)"
  (let* ((buffer-obj (bsh-compilation-buffer "buffer"))
	 (native-buf (oref buffer-obj buffer))
	 (bufwin (display-buffer native-buf)))

    (compilation-set-window-height bufwin)

    (save-some-buffers (not compilation-ask-about-save) nil)

    (if finish-fcn
	(lexical-let ((finish finish-fcn))
	  (setq compilation-finish-functions
		(lambda (buf msg)
		  (funcall finish buf msg)
		  (setq compilation-finish-functions nil)))))


    (if compilation-process-setup-function
        (funcall compilation-process-setup-function))

    (if compilation-process-setup-function
        (funcall compilation-process-setup-function))

    (with-current-buffer native-buf

      (if buffer-head
	  (insert buffer-head)
	(insert java-expr))

      (insert "\n")


      (if (not (jdee-bsh-running-p))
	  (progn
	    (bsh-launch (oref-default 'jdee-bsh the-bsh))
	    (bsh-eval (oref-default 'jdee-bsh the-bsh)
                      (jdee-backend-create-prj-values-str))))

      (bsh-buffer-eval
       (oref-default 'jdee-bsh the-bsh)
       java-expr
       buffer-obj)

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer native-buf))))

;;;###autoload
(defun jdee-bsh-run()
  "Starts the JDEE version of the BeanShell."
  (bsh-launch (oref-default 'jdee-bsh the-bsh) t))

(defun jdee-bsh-exit ()
  "Closes the existing beanshell process."
  (if (jdee-bsh-running-p)
      (let ((process (bsh-get-process (oref-default 'jdee-bsh the-bsh))))
	(if (and
	     (boundp 'jdee-ant-invocation-method) ;; ant package may not be loaded.
	     (string= (car (symbol-value 'jdee-ant-invocation-method)) "Ant Server"))
	    (process-send-string process "jde.util.JdeUtilities.exit();\n")
	  (process-send-string process "exit();\n")))
    (message "The beanshell is not running")))


;;; interactive evaluation

;;;###autoload
(defun jdee-bsh-quote-expr (&optional start end no-param-p no-quote-wrap-p)
  "Add necessary syntax for a beanshell string (parameter) in the current
buffer.  This is useful for quoting a whole buffer or strings with newlines,
etc.  This gives the outside double quotes as well.

NO-PARAM-P if non-nil, don't split string (surround double quotes) using string
concatentation.  The only way to get quotes in strings is to split them up and
concatenate the quote (') char using the plus (+) operator.  This doesn't do
this syntax change.

NO-QUOTE-WRAP-P, if non-nil, don't add double quotes around the whole statement."
  (interactive "r")
  (setq start (or start (point-min))
	end (or end (point-max)))
  (let ((expr (buffer-substring-no-properties start end))
	(repls (append '(("\\" . "\\\\"))
		       (if (not no-param-p)
			   '(("\n" . "\\n"))))))
    (save-match-data
      (if (not no-param-p)
	  (setq expr (mapconcat #'identity (split-string expr "\"")
				"\" + '\"' + \"")))
      (setq expr (with-temp-buffer
		   (insert expr)
		   (dolist (repl repls)
		     (goto-char (point-min))
		     (while (search-forward (car repl) nil t)
		       (replace-match (cdr repl) nil t)))
		   (when (not no-quote-wrap-p)
		     (goto-char (point-min))
		     (insert "\"")
		     (goto-char (point-max))
		     (insert "\""))
		   (buffer-substring (point-min) (point-max))))
      (when (called-interactively-p 'interactive)
	(save-excursion
	  (delete-region start end)
	  (goto-char start)
	  (insert expr)))
      expr)))

(provide 'jdee-bsh)

;;; jdee-bsh.el ends here
