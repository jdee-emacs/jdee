;;; jdee-juci.el --- JDEE Universal Communication Interface
;; $Id$

;; Copyright (C) 2002, 2003 by Nick Sieger
;; Copyright (C) 2009 by Paul Landes

;; Author: Nick Sieger <nsieger@bitstream.net>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: processes, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; Declaratively connect the elisp side of the JDEE with the java
;; engine.

;; To call Java from Elisp, use one of the following functions:
;;
;; 1) `jdee-juci-invoke-java' for calling methods on java
;; interface/implementation pairs;
;; 2) `jdee-juci-invoke-script' for calling Beanshell command scripts.
;;
;; See the documentation for these two functions for details.
;;
;; For further information about JUCI, see the package Javadoc
;; (package.html) for the jde.juci package.

;;; Code:

(require 'jdee)

;;; These aren't really user-customizable variables, hence the
;;; defvar's.

(defvar jdee-juci-java-connection-name "juciConn"
  "*Name of beanshell symbol to hold the JUCI connection object.")

(defvar jdee-juci-logger-filename nil
  "*Filename where JUCI connection logging is sent if non-nil.")

(defvar jdee-juci-has-proxy nil
  "*Non-nil if the underlying JDK in which the beanshell is run
supports the java.lang.reflect.Proxy mechanism.  Without it, JUCI is
unavailable.")

(defvar jdee-juci-checked-proxy nil
  "*Non-nil if we already checked for java.lang.reflect.Proxy.")

(defun jdee-juci-check-proxy ()
  "Ensure that the proxy mechanism is available before JUCI
invocation."
  (or jdee-juci-has-proxy
      (and (not jdee-juci-checked-proxy)
	   (condition-case nil
	       (progn
		 (setq jdee-juci-checked-proxy t)
		 (jdee-jeval "java.lang.reflect.Proxy.class;")
		 (setq jdee-juci-has-proxy t))
	     (error (setq jdee-juci-has-proxy nil))))
      (error "JDK version 1.3 or higher required for JUCI (needs java.lang.reflect.Proxy)")))

(defun jdee-juci-invoke-java (java-class method-name &rest method-args)
  "Invoke METHOD-NAME (with METHOD-ARGS) on JAVA-CLASS declaratively
through JUCI.  Example: you have a java interface `Ticker' with a
method called `getStockPrice' (and an implementation in `TickerImpl'):

  package my.finance;

  public interface Ticker {
      double getStockPrice(String symbol);
  }

  public class TickerImpl implements Ticker {
      public TickerImpl() {}

      public double getStockPrice(String symbol) {
	  double price;
	  // implementation details ...
	  return price;
      }
  }

\(Note that the declaration of the callable method(s) in an interface
is necessary because of how JUCI operates.)  Calling this java class
through JUCI is as simple as defining an elisp function as follows:

  \(defun my-finance-ticker-stock-price (symbol)
    (jdee-juci-invoke-java \"my.finance.TickerImpl\" \"getStockPrice\" symbol))

The arguments to `my-finance-ticker-stock-price' are automatically
passed to the java method.  Translation of lisp objects into beanshell
script is done by the function `jdee-juci-bshify-object'."
;;; If JAVA-CLASS is null, the special behavior is that METHOD-NAME is
;;; interpreted to be a Beanshell script name.  Arguments are still
;;; processed and passed to the script.  But, this behavior shouldn't
;;; be publicly documented, because `jdee-juci-invoke-script' should be
;;; used for those cases.
  (jdee-juci-check-proxy)
  (let ((cnt 0) converted-args arg-ptr)
    (jdee-juci-eval (concat jdee-juci-java-connection-name
			   " = jde.juci.ConnectionFactory.getConnection("
			   (jdee-juci-connection-class java-class)
			   ", this, \""
			   jdee-juci-java-connection-name "\");"))
    (condition-case err
	(progn
	  (jdee-juci-eval (concat jdee-juci-java-connection-name ".begin();"))
	  (jdee-juci-setup-logger)
	  (setq arg-ptr method-args)
	  (while arg-ptr
	    (setq converted-args (nconc converted-args
					(list (jdee-juci-setup-method-arg (car arg-ptr) cnt))))
	    (setq arg-ptr (cdr arg-ptr))
	    (setq cnt (1+ cnt)))
	  (if (null java-class)
	      (jdee-juci-eval-r (concat jdee-juci-java-connection-name ".evalBshScript(\"" method-name
				     "(" (mapconcat 'identity converted-args ",") ");\");"))
	    (jdee-juci-eval-r (concat jdee-juci-java-connection-name "." method-name
				   "(" (mapconcat 'identity converted-args ",") ");")))
	  (jdee-juci-eval-r (concat jdee-juci-java-connection-name ".end();")))
      (error
       (progn
	 (jdee-log-msg "juci-invoke-java: error signaled %S" err)
	 (jdee-juci-eval (concat jdee-juci-java-connection-name ".reset();"))
	 (signal (car err) (cdr err)))))))

(defun jdee-juci-invoke-script (script-name &rest script-args)
  "Invoke a Beanshell command script via JUCI, returning the result to
Emacs.  This must be a command script in the bsh/commands path on the
classpath; not any arbitrary script statement.  SCRIPT-NAME must match
the name of the command script filename, minus the .bsh extension.
Any provided SCRIPT-ARGS are converted and passed through to the
script."
  (jdee-juci-invoke-java nil script-name script-args))

(defun jdee-juci-invoke-elisp (form)
  "Function used by JUCI connection infrastructure to call/eval an
elisp form and return the result to the java code.  Any elisp code may
be invoked EXCEPT that which could eventually call back to the
beanshell.  JUCI only allows at most a inter-boundary call stack of
depth 2, i.e., elisp calls java/java calls elisp."
  (let (result arg-name)
    (condition-case err
	(progn
	  (setq result (eval form))
	  (setq arg-name (jdee-juci-setup-method-arg result 0)))
      (error
       (setq arg-name "error")
       (jdee-juci-eval (concat arg-name " = new jde.juci.ElispError("
			      (jdee-juci-bshify-object err) ");"))))
    (jdee-juci-eval (concat jdee-juci-java-connection-name
			   ".pushResult(" arg-name ");"))))

(defun jdee-juci-connection-class (java-class)
  "Prepare the connection class argument to the JUCI
ConnectionFactory.getConnection() call."
  (cond
   ((stringp java-class)
    (concat java-class ".class"))
   ((listp java-class)
    (concat "new Class[] {"
	    (mapconcat #'(lambda (cls)
			   (concat cls ".class"))
		       java-class ",")
	    "}"))
   (t
    (error "java-class must be a string or a list of strings"))))

(defun jdee-juci-setup-method-arg (arg arg-num)
  "Setup and save a JUCI java method argument inside of a beanshell
variable.  Returns the name of the beanshell variable assigned to."
  (let ((arg-name (concat jdee-juci-java-connection-name "Arg"
			  (number-to-string arg-num))))
    (jdee-juci-eval (concat arg-name " = " (jdee-juci-bshify-object arg) ";"))
    arg-name))

(defun jdee-juci-setup-logger ()
  "Setup the JUCI connection logger file name, if
`jdee-juci-logger-filename' is non-nil."
  (if jdee-juci-logger-filename
      (jdee-juci-eval (concat jdee-juci-java-connection-name
			     ".setLoggerFilename(\"" jdee-juci-logger-filename "\");"))))

(defun jdee-juci-eval (expr &optional eval-return)
  "Evaluate a JUCI expression in the JDEE's beanshell."
  (let (result)
    (prog1
	(setq result (jdee-jeval expr eval-return))
      (if eval-return
	  (jdee-log-msg "juci-eval: %s produced result: %S" expr result)
	(jdee-log-msg "juci-eval: %s" expr)))))

(defun jdee-juci-eval-r (expr)
  "Evaluate a JUCI expression in the JDEE's beanshell."
  (jdee-juci-eval expr t))

(defun jdee-juci-bshify-object (arg)
  "Convert a lisp object to a java object representation in BeanShell
script.  Conversion of lisp types is done as follows:

  Elisp             Java
  =====             ====
  t                 true (Boolean.TRUE)
  nil               false (Boolean.FALSE)
  'null             null
  number            no conversion
  string            no conversion
  any symbol        jde.juci.Symbol
  dotted-pair       jde.juci.Cons
  any sequence      java.util.List"
  (cond
   ((eq arg 'null)
    "null")
   ((eq arg nil)
    "false")
   ((eq arg t)
    "true")
   ((numberp arg)
    (number-to-string arg))
   ((stringp arg)
    (concat "\"" (jdee-juci-escape-string arg) "\""))
   ((symbolp arg)
    (concat "new jde.juci.Symbol(\"" (symbol-name arg) "\")"))
   ((and (consp arg)			; dotted-pair cons cell
	 (not (consp (cdr arg))))
    (concat "new jde.juci.Cons("
	    (jdee-juci-bshify-object (car arg)) ","
	    (jdee-juci-bshify-object (cdr arg))
	    ")"))
   ((sequencep arg)
    (concat "Arrays.asList(new Object[] {" (mapconcat 'jdee-juci-bshify-object arg ",") "})"))))

(defun jdee-juci-escape-string (string)
  "Escape a string for transport across the JUCI boundary."
  (mapconcat #'(lambda (c)
		 (cond
		  ;; TODO: more escapes here?
		  ((eq c ?\\)
		   "\\\\")
		  ((eq c ?\")
		   "\\\"")
		  ((eq c ?\')
		   "\\\'")
		  ((eq c ?\n)
		   "\\n")
		  ((eq c ?\t)
		   "\\t")
		  ((eq c ?\b)
		   "\\b")
		  ((eq c ?\f)
		   "\\f")
		  ((eq c ?\r)
		   "\\r")
		  (t
		   (char-to-string c))))
	     (string-to-list string) ""))



;;; Some test functions -- everything below here is only for either
;;; testing or sample usage.

(defun jdee-juci-test-echo (message)
  (jdee-juci-invoke-java "jde.juci.test.EchoImpl" "ack" message))

(defun jdee-juci-test-roundtrip (object)
  (jdee-juci-invoke-java "jde.juci.test.EchoImpl" "roundTrip" object))

(defun jdee-juci-test-callback-get-message ()
  (jdee-juci-invoke-java "jde.juci.test.CallbackImpl" "getMessage"))

(defun jdee-juci-test-callback-get-buffer-contents ()
  (jdee-juci-invoke-java "jde.juci.test.CallbackImpl" "getBufferContents"))

(defun jdee-juci-test-prompt-user-input ()
  (read-from-minibuffer "Input: "))

(defun jdee-juci-test-prompt-buffer-contents ()
  (buffer-string))

(defun jdee-juci-xunit-assert-equal (expected actual &optional msg)
  "xUnit-style assertion function to be used by unit tests.  Assert
that EXPECTED is `equal' to ACTUAL.  Signal an error if not."
  (or (equal expected actual)
      (error "expected:<%S> but was:<%S>.  %s" expected actual (or msg ""))))

(defmacro jdee-juci-xunit-assert-error (form &optional msg)
  "xUnit-style assertion function to be used by unit tests.  Assert
that a FORM, when executed, produces an error.  If no error is
signaled, then signal an error."
  `(condition-case nil
       (let ((message-log-max))	;; quiet (message)
	 ,form
	 (error "No error generated.  %S" (or ,msg "")))
     (error (message nil) t)))

(defun jdee-juci-test-roundtrips ()
  (jdee-juci-xunit-assert-equal "hello" (jdee-juci-test-echo "hello") "1a")
  (jdee-juci-xunit-assert-error (jdee-juci-test-echo nil) "Argument type mismatch 1b")
  (jdee-juci-xunit-assert-equal 'null (jdee-juci-test-echo 'null) "1c")
  (jdee-juci-xunit-assert-equal "hello" (jdee-juci-test-roundtrip "hello") "2a")
  (jdee-juci-xunit-assert-equal nil (jdee-juci-test-roundtrip nil) "2b")
  (jdee-juci-xunit-assert-equal t (jdee-juci-test-roundtrip t) "2c")
  (jdee-juci-xunit-assert-equal '(1 2 3 4) (jdee-juci-test-roundtrip '(1 2 3 4)) "3a")
  (jdee-juci-xunit-assert-equal '(1 2 3 4 (5 6 (7 8))) (jdee-juci-test-roundtrip '(1 2 3 4 (5 6 (7 8)))) "3b")
  (jdee-juci-xunit-assert-equal 'null (jdee-juci-test-roundtrip 'null) "3c"))

(defun jdee-juci-test-unit-tests ()
  "Run all JUCI Elisp unit tests."
  (interactive)
  (jdee-juci-test-roundtrips)
  (message "All unit tests completed successfully."))

(defun jdee-juci-test-bean-info-maker (class-name)
  "Example command that invokes the JDEE's beanInfoMaker.bsh script through JUCI."
  (interactive "sClass Name: ")
  (with-output-to-temp-buffer "*bean-info*"
    (princ (jdee-juci-invoke-script "beanInfoMaker" class-name)))
  (with-current-buffer "*bean-info*"
    (jdee-mode)))

(provide 'jdee-juci)

;;; jdee-juci.el ends here
