;;; regress.el --- Regression test harness for Emacs Lisp code
;; $Id$

;; Copyright (C) 1997, 2004 by Wayne Mesard
;; Copyright (C) 2009 by Paul Landes

;; Author: Wayne Mesard <wmesard@sgi.com>
;;	Tom Breton <tob@world.std.com>
;; Last modified: 1999-07-10
;; Version: 1.5.0
;; Keywords: lisp, tools, maint

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;$$ Move the documentation into its own file.

;;; Commentary:

;; This module provides support for writing and executing regression tests
;; for Emacs Lisp code.  When people bother to write regression tests at all,
;; they're typically included as off-hand comments:
;;
;;   ;; (add1 3)  ==> 4
;;   ;; (add1 -9) ==> -8
;;   (defun add1 (num)
;;     (1+ num))
;;
;; There are a couple of problems with this:
;;   o these tests are often treated as a supplement a textual comment,
;;     so they may not be very rigorous or thorough.
;;   o these tests won't get run very often; so if something breaks, it
;;     may not get detected until days or weeks later.
;;   o the motivation for a test case itself may be unclear, so when
;;     it returns a surprising value down the road, the programmer may
;;     not remember what the test was for in the first place.

;; This module makes it easy for Emacs lisp programmers to write
;; complete, well-documented regression tests and to run them often
;; during the developement and enhancement processes.

;; Here's the idea:
;;   1. The programmer puts one or more test suites directly in the lisp
;;      file, wrapped inside an "eval-when-compile" special form.  This
;;      causes the test suites to be available when .el file is loaded
;;      (or when the buffer is evaluated), but not when the .elc file is
;;      loaded.
;;
;;   2. The programmer runs the tests in one of two ways:
;;        a. Interactively, with the M-x regress command.
;;        b. Automatically, every time the file is evaluated, by putting
;;           a small bit of code at the end of the file.
;;
;;   3. If there are any regressions, a report is produced detailing the
;;      problems.

;; Here's the interface:
;;   M-x regress-insert-suite
;;         Insert a template for a new suite of tests.  Presumably, a
;;         suite appears immediately above the function or functions
;;         that are being tested; but it can appear anywhere--even in a
;;         separate file.
;;   M-x regress-insert-call
;;         Insert code which will cause regression tests to be run when
;;         the .el file is loaded if and only if the "regress" module
;;         has also been loaded.  Presumably, this will appear once at
;;         the end of the .el file; but it can appear anywhere, as many
;;         times as needed, or not at all  if the tests will be run in
;;   M-x regress
;;         Run one or more test suites.  Report any failures.
;;   M-x regress-forget
;;         This doesn't actually delete the test suites, but it does
;;         cause M-x regress to forget that they are test suites so that
;;         they won't appear in the list of suites that can be run.
;;         This can be useful when the programmer has finished work on
;;         one module and is moving on to another.  (The test suites
;;         will get ``remembered'' if the .el file is evaluated again.)

;; Here's the data structures:
;;   Summary:
;;     test-suite  := ([description] [test...])
;;     test        := ([description] probe [':test'] grader)
;;     description := <a string>
;;     probe       := a Lisp expression to perform the actual test.
;;     grader      := a Lisp expression to evaluate whether or not the
;;                    probe was successful.
;;
;;                    If <grader> is preceded by the keyword :test,
;;                    <grader> itself is evaluated, and the test
;;                    passes if the result of non-nil.  For
;;                    convenience, during the evaluation of <grader>,
;;                    a special variable, RESULT, will be bound to the
;;                    result of <probe>.

;;                    Otherwise the test will pass if the result of
;;                    <probe> is equal to <grader>, according to the
;;                    elisp function 'equal'.

;;  Failure indications, new in version 1.5.0.
;;
;;  You can use regress without knowing about failure indications, and
;;  it's probably best to ignore them when you first use regress.  But
;;  as your tests become more complicated, you may wish for an easy
;;  way to know exactly what part of your tests failed.  One way to do
;;  it is to assign to the special variable FAILURE-INDICATION.  If a
;;  regression test fails and FAILURE-INDICATION is non-nil, it will
;;  be printed along with the results.


;; Here are some contrived, simple examples.  Much of regress.el
;; itself contains regression tests.  Search for "eval-when-compile",
;; below.

;;Moved
(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'demo1 'regression-suite t)
  (put 'demo2 'regression-suite t)
  (put 'demo3 'regression-suite t)
  (put 'demo4 'regression-suite t)
  (put 'demo6 'regression-suite t)

  (defvar demo1
   '("three ways of writing the same test"
     ("Implicit test"
      (/ 30 2)
      15
      )

     (;; the description string is optional
      (/ 30 2)
      15
      )

      ("Implicit test with a calculated comparand"
	(/ 30 2)
	(* 3 5)
	)

     ("Explicit test"
       (/ 30 2)
       :test
       (eq RESULT 15)
       )

      ;; This test contains a deliberate error, so you can see what an error
      ;; report looks like.
     ("Deliberate error to demonstrate report format"
      (/ 30 2)
      17
      )
     ))

  ;; each test is run inside of a "save-excursion", so set-buffer et al
  ;; is allowed.

  (defvar demo2
   '("test uses a temp buffer; grader looks at the buffer to see how we did"
     ((progn (set-buffer (get-buffer-create "regress-demo"))
	    (erase-buffer)
	    (insert "abc")
	    )
     ;; If our string appears right before point, then the insert succeeded
     ;; Note that the result of the probe is not important; we're looking
     ;; at what the probe did to the current buffer
       :test
     (eq (point)
	 (save-excursion
	   (and (re-search-backward "abc" nil t)
		(match-end 0)
		))
	 )
     )))

  ;;

  (defvar demo3
   '("tests of setenv/getenv"
     (
      (setenv "regress" "foo")
       :test
      (equal "foo" (getenv "regress"))
      )
     (
      (setenv "regress" nil)
       :test
      (null (getenv "regress"))
      )
     (
      (getenv "USER")
       :test
      (equal RESULT user-login-name)
      )
     ))

  (defvar demo4
    '("Demonstrations of error recognition"

       ( "Expect an error of some kind, using regress-got-error"
	 (+ "Not a number" "Not one either")
	 :test
	 (regress-got-error RESULT))

       ( "Expect a specific error."
	 (+ "Not a number" "Not one either")
	 :test
	 (equal (car RESULT)  'wrong-type-argument))

       ("DELIBERATE FAILURE: Demonstrate that an error thrown by the
grader expression itself will not masquerade as a successful test"
       t
       :test
	 (progn
	   (error "Not a real error, but an incredible simulation")
	   t))

       ))


  (defvar demo6
    '("Demonstrations of failure indication."
       ("DELIBERATE FAILURE: demonstrate FAILURE-INDICATION"
	 t
	 (progn
	   (setq FAILURE-INDICATION
	     "Show this object if the test fails")
	   nil))

       (
	t
	(progn
	  (setq FAILURE-INDICATION
	    "FAILURE-INDICATION is not seen if the test succeeds.")
	  t))

       )))









;;; Variables
;;Moved into tools
(defvar regress-expert nil
  "*If nil, \\[regress-insert-suite] will insert some helpful comments.
If not nil and not t, it will insert a one-line helpful comment.")

(defvar regress-default-variable-name t
  "*If non-nil, \\[regress-insert-suite] will attempt to supply a default name
for the new test suite.")


;;Moved into report
(defvar regress-error-buffer "*Regression Error*")

;;; Commands

;;Moved into interaction
(defun regress-insert-suite (name docstring update-call-site)
  "Insert a template for a new test suite.
Prompts for the NAME of the variable to use, and for a short DOCSTRING
describing the purpose of the test suite.
  If regress-expert (which see) is non-nil, some helpful comments are also
inserted."
  (interactive
   (let* ((var (read-from-minibuffer
		"Variable name for this regression suite: "
		(regress-default-variable-name)
		nil t))
	  (vname (symbol-name var))
	  (doc (read-string
		(concat "Documentation: (default \"" vname "\"): ")))
	  )
     (list vname
	   (prin1-to-string (if (zerop (length doc)) vname doc))
	   (let ((loc (regress-call-site)))
	     (if (and loc
		      (or current-prefix-arg
			  (y-or-n-p "Add to the regress call site below? ")))
		 loc))
	   )
     ))

  (if (eq t update-call-site)
    ;; for non-interactive calls (otherwise the caller would have to call
    ;; regress-call-site itself).
    (setq update-call-site (regress-call-site)))

  (insert
    "\n(eval\-when-compile"
    "\n  ;; This code will not appear in the compiled (.elc) file"
    "\n  (put '" name " 'regression-suite t)"
    "\n  (setq " name
    "\n   '(" docstring
    )
  (cond
    ((eq t regress-expert))

    (regress-expert
      (insert "\n     ;; ([description] probe grader)"))

    (t
      (insert
	"\n     ;; Each test in the suite is of the form:"
	"\n     ;;   ([description] probe grader)"
	"\n     ;;   DESCRIPTION - string"
	"\n     ;;   PROBE -  a sexp which runs the actual test"
	"\n     ;;   GRADER - the desired result or a sexp which determines"
	"\n     ;;   how we did"
	)))

  (insert
    "\n     (")
  (save-excursion
    (insert
      "\n      )"
      "\n      )))"
      "\n"
      )
    (if update-call-site
      (progn
	(goto-char update-call-site)
	(insert " " name)))

    ))

(defun regress-call-site ()
  (save-excursion
    (let ((regexp "(if (featurep 'regress)[\n\t ]*(regress"))
    (and (re-search-forward regexp nil t)
	 (not (re-search-forward regexp nil t))
	 (point-marker)))
    ))


(defun regress-default-variable-name ()
  (if regress-default-variable-name
      (let ((str (if (looking-at "[\n\t ]*(defun[\t ]+\\([^\t ]+\\)")
		     (buffer-substring-no-properties
		      (match-beginning 1) (match-end 1))
		   (if buffer-file-name
		       (file-name-nondirectory
			(file-name-sans-extension buffer-file-name)))
		   )))
	(if str
	    (cons (concat str "-regress") 0)))
    ))


(defun regress-insert-call (&rest suites)
  "Inserts code to run one or more test SUITES.
The idea is that the programmer would put this at the end of the .el file."
  (interactive (regress-prompt-for-suites "Insert" ))
  (insert "\n;; Run diagnostics when this module is evaluated or compiled"
	  "\n;; if and only if the \"regress\" package is already loaded."
	  "\n;; This code will not appear in the compiled (.elc) file"
	  "\n(eval\-when-compile"
	  "\n  (autoload 'regress \"regress\" \"run regression test suites\" t)
"
	  "\n  (if (featurep 'regress)"
	  "\n      (regress "
	  (mapconcat (function symbol-name) suites " ")
	  "))\n  )\n")
  )


(defun regress-forget (&rest suites)
  "Forget that a variable contains a test suite.
This can be handy if you're done working on one module that has regression
tests and want to move on to another."
  (interactive (regress-prompt-for-suites "Forget"))
  (mapcar
    (function
      (lambda (x)
	(put x 'regression-suite nil)))
    suites))


;;;
;;; Running a regression test
;;;


(defun regress-do-test (item description)
  "Run a single test.

Return a list of failure-data if the test failed, otherwise return nil."

  (save-excursion
    (let
      ( obtained
	(FAILURE-INDICATION nil)
	success)

      (setq obtained
	(condition-case err

	  ;;Eval the probe expression.
	  (eval (nth 1 item))

	  ;;If probe made an error, take its value to be that error.
	  ;;The grader may be interested in exactly what the error is,
	  ;;and not trapping the error would stop the entire suite.
	  (error err)))

      (condition-case err
	(progn
	  (setq success
	    (if (regress-test-is-explicit-p item)

	      ;;An explicit grader succeeds if it returns non-nil
	      (let ((RESULT obtained))
		(eval (nth 3 item)))

	      ;;An implicit grader succeeds if it gives the same value
	      ;;as the probe.
	      (equal obtained
		(eval (nth 2 item)))))

	  (if
	    (not success)
	    (list description item obtained FAILURE-INDICATION)
	    nil))

	;;If the grader had an error, catch it and return a special
	;;error.
	(error
	  (list description item obtained err))

	))))


(defun regress (&rest suites)
 (interactive
    (mapcar (function symbol-value)
      (regress-prompt-for-suites "Run" )))
  (let ((description nil)
	 (failures nil)
	 (test-count 0)
	 (fail-count 0)
	 suite item
	 ideal;;Never used.
	 new-failure)

    (while suites
      (setq suite (car suites)
	    suites (cdr suites))
      (if (and (car suite) (not (stringp (car suite))))
	  (setq description "Untitled test suite")
	(setq description (car suite)
	      suite (cdr suite)))
      (while suite
	(setq item (car suite))
	;; Untitled test (and no nil placeholder, so add the placeholder
	(if (and (car item) (not (stringp (car item))))
	  (setq item (cons nil item)))


	(setq new-failure
	  (regress-do-test item description))

	(if
	  new-failure
	  (setq
	    failures    (cons new-failure failures)
	    fail-count  (1+ fail-count)
	    ;; only report the suite name the first time.
	    description nil))

	(setq suite (cdr suite)
	  test-count (1+ test-count)))

      (if description
	  ;; there were no failures, simply record the suite title
	  (setq failures (cons description failures))))

    (if (zerop fail-count)
      (message (if (= 1 test-count)
		   "The single regression test ran successfully"
		 (if (= 2 test-count)
		     "Both regression tests ran successfully"
		   "All %d regression tests ran successfully"))
	       test-count)
	(progn
	  (message "%d failure%s detected"
		   fail-count
		   (if (= 1 fail-count) "" "s"))
	  (regress-report test-count (reverse failures))
	  ))
    ))


;;;
;;; Report generator
;;;

(defun regress-report (num-tests failures)
  (let ((num-fails 0))
    (with-output-to-temp-buffer regress-error-buffer
      (while failures
	(if (stringp (car failures))
	    ;; this is not a failure, but the docstring from a suite
	    ;; that passed
	    (regress-report-one-success (car failures))
	  (regress-report-one-failure (setq num-fails (1+ num-fails))
				      (car failures)))
	(setq failures (cdr failures)))
      )
    ;; Now do some post-processing to make it more readable
    (set-buffer regress-error-buffer)

    ;; Insert a header and center it
    (goto-char (point-min))
    (insert "*** Emacs Lisp Regression Test Report ***\nGenerated by: "
	    user-mail-address
	    "\n" (current-time-string) "\nTests: "
	    (number-to-string num-tests)
	    "; Failures: "
	    (number-to-string num-fails)
	    "; Score: "
	    (number-to-string
	     (truncate (/ (float (* 100 (- num-tests num-fails)))
			  num-tests)))
	    "%\n\n")
    (center-region (point-min) (point))

    ;; Indent everything
    (indent-rigidly (point) (point-max) 2)

    ;; Unindent and bold the "count" lines
    (goto-char (point-min))
    (while (search-forward "\n  __regressFAILURE: " nil t)
      (replace-match "\nFailure #" nil t)
      (beginning-of-line 2)
      (put-text-property (1+ (match-beginning 0)) (1- (point)) 'face 'bold))
    ;; Find the suite titles, remove the tag, bold and center the text
    (goto-char (point-min))
    (while (search-forward "  __regressSUITE:" nil t)
      (replace-match "" nil t)
      (beginning-of-line 2)
      (put-text-property (match-beginning 0) (1- (point)) 'face 'bold)
      (center-region (match-beginning 0) (point)))
    (goto-char (point-min))
    ;; We don't want this indented.
    (while (search-forward "\n  __regressSUCCESS" nil t)
      (replace-match "\nAll tests passed." nil t)
      )
    (goto-char (point-min))
    ))

(defun regress-report-one-success (docstring)
  (princ "\n__regressSUITE: ")
  (princ docstring)
  (terpri)
  (terpri)
  (princ "__regressSUCCESS\n\n")
  )

(defun regress-report-one-failure (count failure)
  ;; failure is: (suite-docstring item ideal))
  (let ((item (nth 1 failure)))
  (if (car failure)
      ;; This is the first failure in the set, so print the docstring
      (progn
	(princ "\n__regressSUITE: ")
	(princ (car failure))
	(terpri)))
  (princ "\n__regressFAILURE: ")
  (princ count)
  (if (car item)
      ;; This test has a docstring, so print it
      (progn (princ "\n\nDescription:\n-----------\n")
	     (princ (car item))
	     ))
  (princ "\n\nTest:\n----\n")
  (pp (nth 1 item))
  (if (regress-test-is-explicit-p item)
    (progn
      (princ "\nRequirement:\n-----------\n")
      (pp (nth 3 item)))

    (princ "\nExpected value:\n--------------\n")
    (pp (nth 2 item)))

    (if (or
	  (not (regress-test-is-explicit-p item))
	  (regress-sexp-contains 'RESULT (nth 3 item)))
      ;; The test used the return value, so print it
      (progn
	(princ "\n\nActual Value:\n------------\n")
	(pp (nth 2 failure))
	))

    ;;Report a failure indication if there is one.
    (if
      (>= (length failure) 4)
      (progn
	(princ "\n\nFailure indication:\n------------\n")
	(pp (nth 3 failure))
	))


  (terpri) (terpri)
  ))


;;;
;;; Helpers
;;;


(defun regress-test-is-explicit-p (item)
  "Non-nil if the grader in item is an explicit test."
  (eq (nth 2 item) ':test))



;; Used by the interactive functions to prompt for a list of suites

(defun regress-prompt-for-suites (verb)
  (let (lis nam)
    (while
      (not
	(zerop
	  (length
	    (setq nam
	      (completing-read
		(concat verb " test suite "
		  (if lis
		    "(Return when done)"
		    "(Return for all)")
		  ": ")
		obarray
		(function (lambda (x) (get x 'regression-suite)))
		t)))))
      (setq lis (cons (intern nam) lis)))
    (if (null lis)
      (mapatoms
	(function
	  (lambda (x)
	    (if (get x 'regression-suite)
	      (setq lis (cons x lis)))
	    ))))
    lis))



(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (put 'regress-sexp-contains-tests 'regression-suite t)
  (defvar regress-sexp-contains-tests
   '("regress-sexp-contains tests"
     ((regress-sexp-contains 'foo '(foo man chu))
      t)
     ((regress-sexp-contains 'foo '(find a (((buried foo)) man) chu))
      t)
     ((regress-sexp-contains 'bar '(foo man chu))
      nil)
     ("Not good enough, the items are equal, but not eq"
      (regress-sexp-contains '(man) '(foo (man) chu))
      nil)
     ("Not good enough, the strings are equal, but not eq"
      (regress-sexp-contains "foo" '("foo" "man" "chu"))
      nil)
     ("The items are eq, so return t"
      (let ((it '(this is a test)))
	(regress-sexp-contains it (list 'abc it 'xyz)))
      t)
     )
   ))


;; Return t if ITEM appears in SEXP at any depth, nil if not.

(defun regress-sexp-contains (item sexp)
  (or (eq item sexp)
      (and (not (atom sexp))
	   (or (regress-sexp-contains item (car sexp))
	       (regress-sexp-contains item (cdr sexp))))
      ))


;;; Functions to help users easily grade expressions.

(defun regress-got-error (result)
  "t if RESULT is any sort of error, otherwise nil.

Result is the result of a probe expression."

  (and
    (consp result)
    (symbolp (car result))
    (get (car result) 'error-conditions)))

;;Moved
(eval-and-compile
  (defun regress-answer-parm (exp)
    "Define a single test parameter within a grader function.
Helper for regress-define-grader-function."

    `(,(car exp) nil ,(intern (concat "ask-" (symbol-name (car exp))))))


  (defun regress-answer-test (name comparand &optional comparer)
    "Define a single test within a grader function.
Helper for regress-define-grader-function.

If COMPARER is passed, it is used to compare NAME and COMPARAND,
otherwise equal is used."


    (let*
      ((ask-name-sym (intern-soft (concat "ask-" (symbol-name name))))
	(comparer-sym (or comparer 'equal)))

      `(if
	 ,ask-name-sym
	 (,comparer-sym ,comparand ,name)
	 t)))


  (defmacro regress-define-grader-function
    (function-name decomposition-list answerlist)
    "Build a grader function named FUNCTION-NAME.

Requires the cl package.

The result of the probe, which must be a list, will be decomposed
according to DECOMPOSITION-LIST.  Eg, if DECOMPOSITION-LIST is \(foo
bar\) and the result of the probe is \(1 2 3\), there will be an
object named foo with the value 1, and bar with the value 2.

ANSWERLIST is a list whose elements are of the form \(NAME TEST
&optional COMPARER\).  NAME is the name of a parameter to
FUNCTION-NAME, which can be passed as \( ... :NAME value ... \).  TEST
is arbitrary elisp code that will be tested against the parameter NAME
with 'equal.  If COMPARER is given, it is used to compare NAME and
COMPARAND, otherwise equal is used

Except for the required parm RESULT, all parms to FUNCTION-NAME are
optional."

    (let*
      (
	(result-sym (gensym))

	(parmlist
	  (mapcar
	    'regress-answer-parm
	    answerlist))

	(body
	  (mapcar
	    ( function
	      ( lambda (x)
		;;(regress-answer-test (car x) (cadr x))
		(apply 'regress-answer-test x)
		))
	    answerlist))


	(letlist
	  (loop
	    for X in decomposition-list
	    for I from 0
	    collect `(,X (nth ,I ,result-sym)))));;ch

      (require 'cl)

      `(defun* ,function-name
	 (,result-sym &optional &key;;ch
	   ,@parmlist)

	 (let*
	   ,letlist
	   (and
	     ,@body))))))



;;;
;;; Regression testing on regress.el itself
;;;

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (autoload 'regress "regress" "run regression test suites" t)
  (put 'regress-call-site-tests 'regression-suite t)
  (defvar regress-call-site-tests
   '("regress-call-site tests"
     (
      "Create the call site"
      (progn
	(set-buffer (get-buffer-create " regress-test-scratch"))
	(erase-buffer)
	(regress-insert-call 'foobar 'biz)
	(goto-char (point-min))
	(buffer-string)
	)
       :test
      (string-match "(if (featurep 'regress)[\n\t ]*(regress foobar biz))"
		    RESULT)
      )
     ("Insert a new suite and get it added to the call site."
      (progn
	(set-buffer (get-buffer-create " regress-test-scratch"))
	(regress-insert-suite "testme" "testmedoc" t))
       :test
      (re-search-forward
       "(if (featurep 'regress)[\n\t ]*(regress testme foobar biz))"
       nil t)
      )
     )
   ))

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (autoload 'regress "regress" "run regression test suites" t)
  (put 'regress-call-site-all-in-1-test 'regression-suite t)
  (defvar regress-call-site-all-in-1-test
   '("regress-call-site tests all in 1 big ugly test"
     (
      (progn
	;; each test is run in a save-excursion, so set-buffer is safe
	(set-buffer (get-buffer-create " regress-test-scratch"))
	(erase-buffer)
	(regress-insert-call 'foobar 'biz)
	(goto-char (point-min))
	(regress-insert-suite "testme" "testmedoc" t)
	)
       :test
      (re-search-forward
       "(if (featurep 'regress)[\n\t ]*(regress testme foobar biz))"
       nil t)
      )
   )))




(eval-when-compile
  ;;demo5 can only be made if cl is available
  (if
    (featurep 'cl)
    (progn

      ;;Define a grader function.  This sort of grader function is
      ;;useful for when you have to grade different parts of a complex
      ;;result in different tests.
      (regress-define-grader-function

	;;It will be named regress-demo5-grader
	regress-demo5-grader

	;;This section says to decompose the result of probe, which
	;;must be a list, into 2 elements, my-first and my-second,
	;;which can be referenced in the next section.
	(my-first my-second)

	;;This says that the element my-first must be equal to the
	;;parameter the-first IF that parameter is passed otherwise we
	;;don't care.  Similarly, my-second / the-second.
	( (the-first my-first)
	  (the-second my-second)))

      (defvar demo5
	'(
	   "How to use a function defined by regress-define-grader-function."

	   ;;For simplicity, these examples use a literal as the probe.
	   ( "Test only the first element."
	     '(5  6)
	     :test
	     (regress-demo5-grader RESULT :the-first 5))


	   ( "Test only the second element."
	     '(5  6)
	     :test
	     (regress-demo5-grader RESULT :the-second 6))

	   ( "Test both."
	     '(5  6)
	     :test
	     (and
	       (regress-demo5-grader RESULT :the-second 6)
	       (regress-demo5-grader RESULT :the-first  5)))

	   ( "Deliberate failure."
	     '(5  6)
	     :test
	     (regress-demo5-grader RESULT :the-first 1000))

	   ))
      (put 'demo5 'regression-suite t)
      )))


;;;
;;; It's not a bug, it's a *feature*
;;;

(provide 'regress)

;;; End of regress.el
