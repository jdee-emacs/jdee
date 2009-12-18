;;; jde-bsh.el -- Beanshell JDEE integration.
;; $Id: jde.el 127 2009-08-12 08:22:57Z paullandes $

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

;; This library has beanshell specific functionality.  Most of it was taken
;; from jde.el and placed here in an attempt to make jde.el a little skinner
;; and make a home for beanshell specific code.

;;; Code:

(require 'eieio)
(require 'beanshell)
(require 'jde-parse-expr)

(defcustom jde-bsh-separate-buffer nil
  "*Whether or not to use a separate buffer for errors."
  :group 'jde
  :type 'boolean)

(defclass jde-bsh-buffer (bsh-comint-buffer) ()
  "JDEE's beanshell buffer")

(defmethod initialize-instance ((this jde-bsh-buffer) &rest fields)
  (oset this buffer-name "*JDEE bsh*")
  (call-next-method))

(defclass jde-bsh (bsh)
  ((bsh-cmd-dir      :initarg :bsh-cmd-dir
		     :type string
		     :documentation
		     "Path of the BeanShell commmand directory.")

   (checkstyle-jar  :initarg :checkstyle-jar
		    :type string
		    :documentation
		    "Path of the Checkstyle jar.")

   (regexp-jar      :initarg :regexp-jar
		    :type string
		    :documentation
		    "Path of the Jakarta regexp jar.")

   (jde-jar         :initarg :jde-jar
		    :type string
		    :documentation
		    "Path of the JDEE jar.")

   (jde-classes-dir :initarg :jde-classes-dir
		    :type string
		    :documentation
		    "Path of the JDEE classes directory.")

   (the-bsh        :type jde-bsh
		   :allocation :class
		   :documentation
		   "The single instance of the JDEE's BeanShell."))
  "Class of JDEE BeanShells. There is only one per Emacs session.")

(defmethod initialize-instance ((this jde-bsh) &rest fields)
  "Constructor for the JDEE BeanShell instance."
  (call-next-method)
  (let* ((jde-java-directory
	  (concat
	   (jde-find-jde-data-directory)
	   "java/")))

    (oset this bsh-cmd-dir (expand-file-name "bsh-commands" jde-java-directory))
    (oset this checkstyle-jar  (expand-file-name "lib/checkstyle-all.jar" jde-java-directory))
    (oset this regexp-jar (expand-file-name "lib/jakarta-regexp.jar" jde-java-directory))
    (oset this jde-classes-dir (expand-file-name "classes" jde-java-directory))
    (oset this jde-jar (expand-file-name "lib/jde.jar" jde-java-directory))
    (oset this jar  (expand-file-name "lib/bsh.jar" jde-java-directory))
    (oset this separate-error-buffer jde-bsh-separate-buffer)
    (oset-default 'jde-bsh the-bsh this)))

(defmethod bsh-create-buffer ((this jde-bsh))
  "Creates the JDEE's beanshell buffer."
  (oset this buffer (jde-bsh-buffer "JDEE bsh buffer")))

(defmethod bsh-build-classpath-argument ((this jde-bsh))
  (jde-build-classpath (oref this cp) 'jde-global-classpath t))

(defmethod bsh-launch :BEFORE ((this jde-bsh) &optional display-buffer)
  "Sets the vm and classpath to the vm and classpath for the current project before
the PRIMARY launch method is invoked."
  (let* ((project-ant-home
	  ;; Code referring to jde-ant variables uses symbols to
	  ;; avoid causing compilation errors since jde-ant is not required.
	  (jde-get-project 'jde-ant-home jde-current-project))
	 (ant-home (if (and (boundp 'jde-ant-home)
			    (not (string= (symbol-value 'jde-ant-home) "")))
		       (symbol-value 'jde-ant-home)     ;jde-ant loaded
		     (if (and project-ant-home
			      (not (string= project-ant-home "")))
			 project-ant-home ; jde-ant not loaded but
					; jde-ant-home set in project
					; file
		       (getenv "ANT_HOME")))) ; jde-ant-home not set in
					; project file and not
					; customized
	 )

    (oset this vm (oref (jde-run-get-vm) :path))
    (oset  this  cp (delq
		     nil
		     (append
		      (list
		       (oref this jar)
		       (oref this bsh-cmd-dir)
		       (oref this checkstyle-jar)
		       (oref this regexp-jar)
		       (if jde-devel-debug
			   (oref this jde-classes-dir))
		       (oref this jde-jar)
		       (jde-get-tools-jar)
		       (if ant-home (expand-file-name "lib" ant-home)))
		      (jde-pi-get-bsh-classpath)
		      (jde-expand-classpath (jde-get-global-classpath)))))))

;; Create the BeanShell wrapper object.
(jde-bsh "JDEE BeanShell")

(defun jde-bsh-running-p ()
  "Returns t if the JDEE's BeanShell instance is running."
  (bsh-running-p (oref 'jde-bsh the-bsh)))

(defvar java-bsh-read-java-expression-history nil)

(defun jde-bsh-read-java-expression ()
  "Read an expression as input guessing initial input at the current point."
  (if mark-active
      (progn
	(setq java-bsh-read-java-expression-history
	      (cons (buffer-substring (region-beginning)
				      (region-end))
		    java-bsh-read-java-expression-history))
	(jde-quote-expr (region-beginning) (region-end) t t))
    (let ((bnd (if (eq major-mode 'jde-mode)
		   (bounds-of-thing-at-point 'java-expression)))
	  initial)
      (if bnd (setq initial (buffer-substring (car bnd) (cdr bnd))))
      (if (and initial
	       (or (>= (length initial) 80)
		   (save-match-data (string-match "\n" initial))))
	  (setq initial nil))
      (read-string "Expression: " initial
		   'java-bsh-read-java-expression-history))))

;;;###autoload
(defun jde-jeval (java-statement &optional eval-return no-print-p)
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
  (interactive (list (jde-bsh-read-java-expression)))
  (let ((the-bsh (oref 'jde-bsh the-bsh)))
    (when (not (bsh-running-p the-bsh))
      (bsh-launch the-bsh)
      (bsh-eval the-bsh (jde-create-prj-values-str)))
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
    (let ((output (bsh-eval the-bsh java-statement eval-return))
	  len)
      (when (stringp output) 
	(when (> (length output) 0)
	  (setq len (length output))
	  (if (eq ?\n (elt output (1- len)))
	      (setq output (substring output 0 (1- len)))))
	(if (= 0 (length output)) (setq output nil)))
      (when (interactive-p)
	(if output (kill-new output))
	(message (if output
		     (concat "Copied `"
			     (replace-regexp-in-string "%" "%%" output t t)
			     "'")
		   "No result")))
      output)))

(defun jde-jeval-r (java-statement)
  "Uses the JDEE's instance of the BeanShell to
evaluate JAVA-STATEMENT and then uses the Emacs Lisp
interpreter to evaluate the result. This function
is intended to be used to implement Emacs extensions
coded in Java and executed by the BeanShell. The function
assumes that the Java extension interacts with Emacs
by printing Lisp forms to the BeanShell's standard output \
port."
  (jde-jeval java-statement t))

(defun jde-jeval-cm (java-expr &optional buffer-head finish-fcn)
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

 (jde-bsh-compile-mode-eval \"jde.util.CompileServer.compile(\\\"Test.java\\\");\"
   \"Compile Test.java\" 'jde-compile-finish-kill-buffer)"
  (let* ((buffer-obj (bsh-compilation-buffer "buffer"))
	 (native-buf (oref buffer-obj buffer))
	 (bufwin (display-buffer native-buf)))

    (compilation-set-window-height bufwin)

    (save-some-buffers (not compilation-ask-about-save) nil)

    (if finish-fcn
	(lexical-let ((finish finish-fcn))
	  (setq compilation-finish-function
		(lambda (buf msg)
		  (funcall finish buf msg)
		  (setq compilation-finish-function nil)))))


    (if (not (featurep 'xemacs))
	(if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))


    (if (not (featurep 'xemacs))
	(if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))


    (save-excursion
      (set-buffer native-buf)

      (if buffer-head
	  (insert buffer-head)
	(insert java-expr))

      (insert "\n")


      (if (not (jde-bsh-running-p))
	  (progn
	    (bsh-launch (oref 'jde-bsh the-bsh))
	    (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))


      (bsh-buffer-eval
       (oref 'jde-bsh the-bsh)
       java-expr
       buffer-obj)

    (set-buffer-modified-p nil)
    (setq compilation-last-buffer native-buf))))

;;;###autoload
(defun jde-bsh-run()
  "*Starts the JDEE version of the BeanShell."
  (interactive)
  (bsh-launch (oref 'jde-bsh the-bsh) t))

(defun jde-bsh-exit ()
  "Closes the existing beanshell process"
  (interactive)
  (if (jde-bsh-running-p)
      (let ((process (bsh-get-process (oref 'jde-bsh the-bsh))))
	(if (and
	     (boundp 'jde-ant-invocation-method) ;; ant package may not be loaded.
	     (string= (car (symbol-value 'jde-ant-invocation-method)) "Ant Server"))
	    (process-send-string process "jde.util.JdeUtilities.exit();\n")
	  (process-send-string process "exit();\n")))
    (message "The beanshell is not running")))


;;; interactive evaluation

;;;###autoload
(defun jde-bsh-quote-expr (&optional start end no-param-p no-quote-wrap-p)
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
      (when (interactive-p)
	(save-excursion
	  (delete-region start end)
	  (goto-char start)
	  (insert expr)))
      expr)))

(provide 'jde-bsh)

;; End of jde-bsh.el
