;;; jde-javadoc.el --- JDE javadoc autodoc
;; $Id$

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce
;;             Paul Landes <landes <at> mailc dt net>

;; Copyright (C) 1998-2004 by David Ponce
;; Copyright (C) 2009 by Paul Landes

;; Keywords: java, tools
;;
;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library provides a javadoc comment checker and generator to
;; help handling of Java source documentation.

;;; Code:

(require 'tempo)
(require 'semantic-java)
(require 'regexp-opt)

(eval-when-compile
  (require 'jde)
  (require 'jde-parse))

;;;; Customization
;;;; -------------

(defgroup jde-javadoc nil
  "JDE javadoc utilities"
  :group 'jde
  :prefix "jde-javadoc-")

;; IMPORTANT: This function must be defined before the following
;; defcustoms because it is used in their :set clause.
(defun jde-javadoc-define-template (sym val)
  "Define a template (see `tempo-define-template').
The template name is the `symbol-name' of SYM from which the
'-template' suffix has been removed, prefixed by 'tempo-template-'.
VAL is the template value.  If VAL is a string it is converted to a
list of template elements."
  (let* ((name (symbol-name sym))
	 (template-name
	  (if (string-match "\\(.*\\)-template$" name)
	      (match-string 1 name)
	    (error "Invalid template variable name: %S" name)))
	 (template-val
	  (if (stringp val)
	      (car (read-from-string (format "(%s)" val)))
	    val)))
    (tempo-define-template template-name template-val nil name)
    (set-default sym val)))

(defcustom jde-javadoc-describe-class-template
  "\"* Describe class \" (jde-javadoc-code name) \" here.\""
  "*Line template used to describe a class.
If nil the line is not inserted.
The variable 'name' is set to the class name.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-describe-class'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-interface-template
  "\"* Describe interface \" (jde-javadoc-code name) \" here.\""
  "*Line template used to describe an interface.
If nil the line is not inserted.
The variable 'name' is set to the interface name.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-describe-interface'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-constructor-template
  "\"* Creates a new \" (jde-javadoc-code name) \" instance.\""
  "*Line template used to describe a constructor.
If nil the line is not inserted.
The variable 'name' is set to the constructor name (that is the class
name).  See `jde-javadoc-autodoc-at-line' for usage.  Define the
template variable `tempo-template-jde-javadoc-describe-constructor'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-method-template
  "\"* Describe \" (jde-javadoc-code name) \" method here.\""
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the method name.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-describe-method'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-describe-field-template
  "\"* Describe \" (jde-javadoc-field-type modifiers)
 \" \" (jde-javadoc-code name) \" here.\""
  "*Line template used to describe a method.
If nil the line is not inserted.
The variable 'name' is set to the field name.
The variable 'type' is set to the field type.
The variable 'modifiers' is set to the field modifiers.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-describe-field'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-param-tag-template
  "\"* @param \" name \" \" (jde-javadoc-a type)
 \" \" (jde-javadoc-code type) \" value\""
  "*Line template used to describe a parameter.
If nil the line is not inserted.
The variable 'name' is set to the parameter name.
The variable 'type' is set to the parameter type.
A line is inserted for each parameter.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-param-tag'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-return-tag-template
  "\"* @return \" (jde-javadoc-a type)
 \" \" (jde-javadoc-code type) \" value\""
  "*Line template used to describe a returned value.
If nil the line is not inserted.
The variable 'type' is set to the returned type.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-return-tag'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-exception-tag-template
  "\"* @exception \" type \" if an error occurs\""
  "*Line template used to describe an exception.
If nil the line is not inserted.
The variable 'type' is set to the exception type.
A line is inserted for each exception in the 'throws' clause.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-exception-tag'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-author-tag-template
  "\"* @author <a href=\\\"mailto:\" user-mail-address
 \"\\\">\" user-full-name \"</a>\""
  "*Line template used to give an author.
If nil the line is not inserted.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-author-tag'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-version-tag-template
  "\"* @version 1.0\""
  "*Line template used to give a version.
If nil the line is not inserted.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-version-tag'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

;; (defcustom jde-javadoc-see-tag-template
;;   '("* @see " ref)
;;   "*Line template used to give a reference.
;; If nil the line is not inserted.
;; The variable 'ref' is set to the class or interface name.
;; A line is inserted for each name in the 'extends' then 'implements'
;; clauses.  See `jde-javadoc-autodoc-at-line' for usage.  Define the
;; template variable `tempo-template-jde-javadoc-see-tag'."
;;   :group 'jde-javadoc
;;   :type '(choice :tag "Template form"
;;                  (text :format "%t\n%v" :tag "String")
;;                  (repeat :tag "Lisp Expressions" (sexp :tag "")))
;;   :set 'jde-javadoc-define-template)

;; (defcustom jde-javadoc-since-tag-template
;;   '("* @since 1.0")
;;   "*Line template used to give a since reference.
;; If nil the line is not inserted.
;; See `jde-javadoc-autodoc-at-line' for usage.  Define the template
;; variable `tempo-template-jde-javadoc-since-tag'."
;;   :group 'jde-javadoc
;;   :type '(choice :tag "Template form"
;;                  (text :format "%t\n%v" :tag "String")
;;                  (repeat :tag "Lisp Expressions" (sexp :tag "")))
;;   :set 'jde-javadoc-define-template)

(defcustom jde-javadoc-end-block-template
  nil
  "*Javadoc end comment block characters.
If nil \"*/\" is inserted.
See `jde-javadoc-autodoc-at-line' for usage.  Define the template
variable `tempo-template-jde-javadoc-end-block'."
  :group 'jde-javadoc
  :type '(choice :tag "Template form"
		 (text :format "%t\n%v" :tag "String")
		 (repeat :tag "Lisp Expressions" (sexp :tag "")))
  :set 'jde-javadoc-define-template)

;;;; Utilities
;;;; ---------
(defmacro jde-javadoc-status-forms (message donestr &rest forms)
  "Wrapper for `working-status-forms'.
See `working-status-forms' for details on MESSAGE, DONESTR and FORMS
arguments.  Does not override an outer `working-status-forms'
MESSAGE."
  `(working-status-forms (or working-message ,message) ,donestr
     ,@forms))

(defun jde-javadoc-dynamic-status (&rest args)
  "Wrapper for `working-dynamic-status'.
Does nothing if not called within the macro `working-status-forms'.
See `working-dynamic-status' for meaning of ARGS."
  (and working-message
       (apply #'working-dynamic-status args)))

(defalias 'jde-javadoc-skip-spaces-forward
  'semantic-java-skip-spaces-forward)

(defalias 'jde-javadoc-indent-line 'c-indent-line)

(defun jde-javadoc-indent-region (start end)
  "Indent region between START and END."
  (save-excursion
    (goto-char end)
    (setq end (point-marker))
    (goto-char start)
    (while (< (point) end)
      (jde-javadoc-dynamic-status)
      (or (and (bolp) (eolp))
	  (jde-javadoc-indent-line))
      (forward-line 1))
    (move-marker end nil)))

(defun jde-javadoc-map (f l &rest args)
  "Apply F to each element of L.
F receives optional ARGS after the current element of L."
  (while l
    (apply f (car l) args)
    (setq l (cdr l))))

(defun jde-javadoc-window-lines ()
  "Return the number of lines of the selected window.
This number may be greater than the number of actual lines in the
buffer if any wrap on the display due to their length."
  (let ((start (point-min))
	(end   (point-max)))
    (if (= start end)
	0
      (save-excursion
	(save-restriction
	  (widen)
	  (narrow-to-region start end)
	  (goto-char start)
	  (vertical-motion (buffer-size)))))))

(defun jde-javadoc-adjust-window (window)
  "Adjust WINDOW height to fit its buffer contents."
  (save-selected-window
    (select-window window)
    (let ((height (window-height))
	  (lines  (+ 3 (jde-javadoc-window-lines))))
      ;; ensure window will not be deleted if too small
      (if (< lines window-min-height)
	  (setq lines window-min-height))
      (enlarge-window (- lines height)))))

(defsubst jde-javadoc-variable-name (name)
  "Return canonical variable name from NAME.
That is strip any array brackets from NAME."
  (if (string-match "\\`\\([^[]+\\)[[]" name)
      (match-string 1 name)
    name))

(defcustom jde-javadoc-check-undeclared-exception-flag nil
  "*non-nil means to check for undeclared exceptions.
When an exception is implicitly declared (inherits from
RuntimeException) any associated @exception/@throws tag that already
exists will be just kept without issuing a warning.  Other extra tags
are automatically removed.

If nil extra @exception/@throws tags are never removed but warnings
are issued for (maybe) undeclared exceptions."
  :group 'jde-javadoc
  :type 'boolean)

(defun jde-javadoc-implicit-exception-p (type)
  "Return non-nil if TYPE is an implicitly declared exception.
That is if TYPE inherits from java.lang.RuntimeException or if Java
reflection failed to process TYPE."
  (condition-case nil
      (jde-jeval-r
       (format "jde.util.Completion.isAncestorOf(%S,%S);"
	       "java.lang.RuntimeException"
	       (jde-parse-get-qualified-name type)))
    (error t)))

;;;; Text helpers
;;;; ------------

(defun jde-javadoc-field-type (modifiers)
  "Return field category.
That is \"constant\" if field MODIFIERS contains \"static\" and
\"final\" or \"variable\" otherwise.  Useful to generate field
description."
  (if (and (member "static" modifiers) (member "final" modifiers))
      "constant"
    "variable"))

(defun jde-javadoc-a (word)
  "Return \"an\" if WORD begin with a vowel or \"a\" otherwise.
Useful to generate description like \"an int value\" or \"a long value\"."
  (if (string-match "^[aeiouyAEIOUY]" word)
      "an" "a"))

(defun jde-javadoc-code (text)
  "Return \"<code>TEXT</code>\".
Useful to generate HTML code style."
  (concat "<code>" text "</code>"))

;;;; Javadoc comment parser
;;;; ----------------------

;; tags and matchers (many are provided by Semantic)
;;
(defconst jde-javadoc-desc-tag
  "*DESCRIPTION*"
  "Special internal tag associated to descriptions.")

(defconst jde-javadoc-start-tag-regexp
  "[\r\n][ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@"
  "Regexp matching the beginning of a tag.")

(defconst jde-javadoc-end-tag-regexp
  (concat "\\(" jde-javadoc-start-tag-regexp
	  "\\|[ \n\r\t]*\\*/\\)")
  "Regexp matching the end of a tag or description.")

;; Core comment parser
;;
(defun jde-javadoc-normalize-description (desc)
  "Ensure DESC text begins with '\\n* ' and ends with '\\n*\\n'."
  (let ((i (string-match "[^ *\n\r\t]" desc)))
    (if i
	(setq desc (concat "\n* " (substring desc i))))
    ;; TODO: ensure empty line at end
    desc))

(defun jde-javadoc-normalize-ref (val)
  "Strip any [* \\n\\r\\t] from VAL."
  (let* ((keep "[^* \n\r\t]+")
	 (ref  "")
	 (i    (string-match keep val))
	 j)
    (while i
      (jde-javadoc-dynamic-status)
      (setq j   (match-end 0)
	    ref (concat ref (substring val i j))
	    i   (string-match keep val j)))
    ref))

(defun jde-javadoc-parse-description (docstring)
  "Return the description from DOCSTRING or nil if not found.
The returned value has the form ((DESC)).  See also
`jde-javadoc-parse-tag-values'."
  (let ((matcher "/\\*\\*")
	(i 0)
	j tag-val)
    (when (string-match matcher docstring)
      (jde-javadoc-dynamic-status)
      (setq j (match-end 0))
      (setq i (string-match jde-javadoc-end-tag-regexp docstring j))
      (setq tag-val (if i
			(substring docstring j i)
		      (substring docstring j)))
      ;; Ensure that a valid description exists
      (if (not (string-equal ""
			     (jde-javadoc-normalize-ref tag-val)))
	  (list (list tag-val))))))

(defun jde-javadoc-parse-tag-values (docstring tag &optional with-key)
  "Return from DOCSTRING the list of TAG values or nil if not found.
Each value is a pair (VALUE-STRING . VALUE-KEY).  If optional WITH-KEY
is 'name VALUE-KEY is the first word of VALUE-STRING.  If optional
WITH-KEY is 'ref VALUE-KEY is a normalized VALUE-STRING reference (see
`jde-javadoc-normalize-ref').  Otherwise VALUE-KEY is nil."
  (let ((matcher (concat jde-javadoc-start-tag-regexp tag))
	(i 0)
	j tag-val key tag-list)
    (while (string-match matcher docstring i)
      (jde-javadoc-dynamic-status)
      (setq j (match-end 0))
      (setq i (or (string-match
		   jde-javadoc-end-tag-regexp docstring j)
		  (length docstring)))
      (setq tag-val (substring docstring j i))
      (cond ((eq with-key 'name)
	     (setq key (and (string-match
			     "[* \n\r\t]*\\([^ \n\r\t]+\\)" tag-val)
			    (jde-javadoc-variable-name
			     (match-string 1 tag-val)))))
	    ((eq with-key 'ref)
	     (setq key (jde-javadoc-normalize-ref tag-val))))
      (setq tag-list (cons (cons tag-val key) tag-list)))
    (nreverse tag-list)))

(defun jde-javadoc-parse-tag (tag docstring)
  "Return the TAG documentation from DOCSTRING or nil if not found.
Documentation has the form (TAG VALUE-LIST).  See also
`jde-javadoc-parse-tag-values'."
  (cond ((string-equal tag jde-javadoc-desc-tag)
	 (jde-javadoc-parse-description docstring))
	((member tag semantic-java-doc-with-name-tags)
	 (jde-javadoc-parse-tag-values docstring tag 'name))
	((member tag semantic-java-doc-with-ref-tags)
	 (jde-javadoc-parse-tag-values docstring tag 'ref))
	(t
	 (jde-javadoc-parse-tag-values docstring tag))
	))

(defun jde-javadoc-parse-tag-list (docstring)
  "Return the list of tag found in DOCSTRING."
  (let* ((matcher (concat jde-javadoc-start-tag-regexp
			  "\\([^ \n\r\t]+\\)"))
	 (depth (regexp-opt-depth matcher))
	 (i (string-match matcher docstring))
	 j tag-list)
    (while i
      (jde-javadoc-dynamic-status)
      (setq tag-list (cons (match-string depth docstring) tag-list))
      (setq i (string-match matcher docstring (match-end depth))))
    (nreverse tag-list)))

(defun jde-javadoc-parse-docstring (docstring)
  "Return the parsed documentation tree from DOCSTRING.
Result has the following form: (DOCSTRING TAG-LIST TAG-VALUE-ALIST)."
  (if docstring
      (let (tag-list
	    tag-alist l tag
	    throws-assoc except-assoc merged-values)
	(jde-javadoc-status-forms "Parsing" "done"
	  (jde-javadoc-dynamic-status)
	  (setq tag-list (jde-javadoc-parse-tag-list docstring))
	  (setq l (cons jde-javadoc-desc-tag tag-list))
	  (while l
	    (jde-javadoc-dynamic-status)
	    (setq tag (car l))
	    (if (assoc tag tag-alist)
		nil                     ; tag already processed
	      (setq tag-alist
		    (cons (cons tag
				(jde-javadoc-parse-tag tag docstring))
			  tag-alist)))
	    (setq l (cdr l)))
	  ;; The 'throws' and 'exception' tags are equivalent, so
	  ;; their values are merged to allow access to 'exception'
	  ;; tag using 'throws' and vice versa.
	  (jde-javadoc-dynamic-status)
	  (setq throws-assoc (assoc "throws"    tag-alist))
	  (jde-javadoc-dynamic-status)
	  (setq except-assoc (assoc "exception" tag-alist))
	  (when (or throws-assoc except-assoc)
	    (jde-javadoc-dynamic-status)
	    (setq merged-values (append (cdr throws-assoc)
					(cdr except-assoc)))
	    (jde-javadoc-dynamic-status)
	    (if throws-assoc
		(setcdr throws-assoc merged-values)
	      (setq tag-alist (cons (cons "throws"    merged-values)
				    tag-alist)))
	    (jde-javadoc-dynamic-status)
	    (if except-assoc
		(setcdr except-assoc merged-values)
	      (setq tag-alist (cons (cons "exception" merged-values)
				    tag-alist))))
	  (jde-javadoc-dynamic-status t))
	(list docstring tag-list tag-alist))))

;; Handling of javadoc comment parsed tree
;;
(defmacro jde-javadoc-doctree-docstring (doctree)
  "Return the docstring part of DOCTREE."
  `(car ,doctree))

(defmacro jde-javadoc-doctree-tag-list (doctree)
  "Return the tag-list part of DOCTREE."
  `(car (cdr ,doctree)))

(defmacro jde-javadoc-doctree-tag-value-alist (doctree)
  "Return the tag-value-alist part of DOCTREE."
  `(car (cdr (cdr ,doctree))))

(defun jde-javadoc-doctree-tag (doctree tag &optional name)
  "Return from DOCTREE the list of TAG values.
If optional NAME is non-nil return its specific value."
  (let ((doc (cdr
	      (assoc
	       tag
	       (jde-javadoc-doctree-tag-value-alist doctree)))))
    (and doc
	 name
	 (setq doc (rassoc name doc))
	 (setq doc (list doc)))
    doc))

(defun jde-javadoc-doctree-known-tag-list (doctree)
  "Return the list of known tags in DOCTREE .
That is tags in `semantic-java-doc-line-tags'."
  (delq nil
	(mapcar (function
		 (lambda (tag)
		   (and (member tag semantic-java-doc-line-tags)
			tag)))
		(jde-javadoc-doctree-tag-list doctree))))

(defun jde-javadoc-doctree-unknown-tag-list (doctree)
  "Return the list of unknown tags in DOCTREE .
That is tags not in `semantic-java-doc-line-tags'."
  (delq nil
	(mapcar (function
		 (lambda (tag)
		   (and (not (member tag semantic-java-doc-line-tags))
			tag)))
		(jde-javadoc-doctree-tag-list doctree))))

;;;; semantic tags stuff
;;;; ---------------------

(defun jde-javadoc-tag-doctree (tag)
  "Return the parsed documentation tree from TAG."
  (jde-javadoc-parse-docstring
   (semantic-documentation-for-tag tag t)))

(defun jde-javadoc-replace-documentation (tag &optional docstring)
  "Replace TAG documentation with DOCSTRING.
If DOCSTRING is nil just delete the existing documentation."
  (let* ((comment (semantic-documentation-for-tag tag 'flex))
	 start end)
    (when comment
      (set-buffer (semantic-tag-buffer tag))
      (setq start (semantic-lex-token-start comment))
      (setq end   (semantic-lex-token-end   comment))
      (goto-char start)
      (save-excursion
	(goto-char end)
	(jde-javadoc-skip-spaces-forward)
	(delete-region start (point))
	(when docstring
	  (insert docstring)
	  (jde-javadoc-indent-documentation tag))))))

(defun jde-javadoc-delete-documentation (tag &optional noconfirm)
  "Delete TAG documentation.
Require confirmation if optional NOCONFIRM is non-nil.  Return non-nil
if done."
  (if (or noconfirm
	  (y-or-n-p (format "Delete '%s' previous documentation? "
			    (semantic-tag-name tag))))
      (progn
	(jde-javadoc-replace-documentation tag)
	t)))

(defun jde-javadoc-recenter-documentation (tag &optional arg)
  "Center TAG documentation in window and redisplay frame.
With ARG, put point on line ARG.  See also `recenter'."
  (let ((comment (semantic-documentation-for-tag tag 'flex))
	start)
    (if (not comment)
	(setq start (semantic-tag-start tag))
      (set-buffer (semantic-tag-buffer tag))
      (setq start (semantic-lex-token-start comment)))
    (goto-char start)
    (recenter arg)))

(defun jde-javadoc-indent-documentation (tag)
  "Indent TAG documentation."
  (save-excursion
    (let ((comment (semantic-documentation-for-tag tag 'flex))
	  start end)
      (when comment
	(set-buffer (semantic-tag-buffer tag))
	(setq start (semantic-lex-token-start comment))
	(setq end   (semantic-lex-token-end   comment))
	(goto-char end)
	(jde-javadoc-skip-spaces-forward)
	(jde-javadoc-indent-region start (point))))))

;;;; Doc checker
;;;; -----------

(defconst jde-javadoc-checker-report-buffer "*jde-javadoc-checker*"
  "Name of the checker report buffer.")

(defvar jde-javadoc-checker-tag nil
  "Current checked tag.
Local to checker report buffer.")

(defvar jde-javadoc-checker-buffer nil
  "Current checked buffer.
Local to checker report buffer.")

(condition-case nil
    (require 'jde-java-font-lock)
  (error nil))

(defvar jde-javadoc-checker-report-font-lock-keywords
  (list
   ;; References
   (list "`\\(.*\\)'"
	 1 'font-lock-warning-face)
   ;; Javadoc tags
   (list "\\(@[^ \n\r\t]+\\)"
	 1 (cond ((boundp 'jde-java-font-lock-doc-tag-face)
		  'jde-java-font-lock-doc-tag-face)
		 ((featurep 'xemacs)
		  'font-lock-keyword-face)
		 (t
		  'font-lock-constant-face)))
   ;; Misc.
   (list "\\[\\([fnpq]\\)\\]"
	 1 'font-lock-keyword-face)
   )
  "Keywords used to highlight the checker report buffer.")

(defvar jde-javadoc-checker-report-mode-map nil
  "Keymap used in `jde-javadoc-checker-report-mode'.")

(if jde-javadoc-checker-report-mode-map
    ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "q" 'jde-javadoc-checker-quit)
    (define-key keymap "p" 'jde-javadoc-checker-previous)
    (define-key keymap "n" 'jde-javadoc-checker-next)
    (setq jde-javadoc-checker-report-mode-map keymap)))

(defun jde-javadoc-checker-report-mode ()
  "Mode used in checker report buffer.
\\{jde-javadoc-checker-report-mode-map}"
  (kill-all-local-variables)
  (setq major-mode 'jde-javadoc-checker-report-mode)
  (setq mode-name "jde-javadoc-checker")
  (set (make-local-variable 'paragraph-start)
       "[ \t]*$")
  (set (make-local-variable 'paragraph-separate)
       paragraph-start)
  (set (make-local-variable 'font-lock-defaults)
       '((jde-javadoc-checker-report-font-lock-keywords)
	 t t ((?_ . "w"))))
  (use-local-map jde-javadoc-checker-report-mode-map)
  (turn-on-font-lock))

(defun jde-javadoc-checker-show-report (report tag)
  "Show the `jde-javadoc-checker' REPORT for TAG."
  (let ((buffer (semantic-tag-buffer tag)))
    (with-current-buffer
	(get-buffer-create jde-javadoc-checker-report-buffer)
      (setq buffer-read-only nil)
      (erase-buffer)
      (jde-javadoc-checker-report-mode)
      (set (make-local-variable 'jde-javadoc-checker-buffer) buffer)
      (set (make-local-variable 'jde-javadoc-checker-tag)  tag)
      (cond
       (report
	(define-key (current-local-map) "f" 'jde-javadoc-checker-fix)
	(insert (car report))
	(newline 2)
	(mapcar (function
		 (lambda (line)
		   (let* ((from (point))
			  (to (progn
				(fill-region
				 (point)
				 (progn
				   (insert "  " line)
				   (newline)
				   (point)))
				(point))))
		     (goto-char from)
		     (delete-char 1)
		     (insert-char ?\* 1)
		     (goto-char to))))
		(cdr report))
	(newline)
	(insert "[f]-try to fix  ")
	)
       (t
	(define-key (current-local-map) "f" nil)
	(insert "Documentation is up-to-date")
	(newline 2)))
      (insert "[p]-check previous  [n]-check next  [q]-quit")
      (goto-char (point-min))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer))
      (jde-javadoc-adjust-window
       (get-buffer-window (current-buffer)))
      (sit-for 0)
      (save-selected-window
	(let ((window (get-buffer-window buffer)))
	  (if (not window)
	      nil
	    (select-window window)
	    (goto-char (semantic-tag-start tag))
	    (jde-javadoc-skip-spaces-forward)
	    (when (looking-at "/\\*\\*")
	      (forward-comment 1)
	      (jde-javadoc-skip-spaces-forward))
	    (recenter -4))))
      (semantic-momentary-highlight-tag tag))))

(defun jde-javadoc-check-add-summary (report type name)
  "Add a summary to REPORT error list, using tag TYPE and NAME."
  (and (setq report (delq nil report))  ;; Clear empty entries
       (let* ((count (length report))
	      (eword (if (= count 1) "error" "errors")))
	 (cons (format "%s `%s' has %d documentation %s:"
		       type name count eword)
	       (nreverse report)))))

;; Basic doc checkers
;;
(defun jde-javadoc-check-description (doctree)
  "Return a message if DOCTREE does not contain a description."
  (if (jde-javadoc-doctree-tag doctree jde-javadoc-desc-tag)
      nil
    "Missing description"))

(defun jde-javadoc-check-required-tags (doctree allowed extra
						&rest nocheck)
  "Return a message if DOCTREE is missing a required tag.
ALLOWED and EXTRA are respectively the listes of allowed tags and
optional ones.  Optional arguments NOCHECK can be a listes of tags
that are not checked."
  (let (tag missing)
    (while allowed
      (setq tag     (car allowed)
	    allowed (cdr allowed))
      (or (member tag extra)
	  (member tag nocheck)
	  (let ((ignored nocheck) ignore)
	    (while (and (not ignore) ignored)
	      (setq ignore  (member tag (car ignored))
		    ignored (cdr ignored)))
	    ignore)
;;          (member tag semantic-java-doc-with-name-tags)
	  (jde-javadoc-doctree-tag doctree tag)
	  (setq missing (cons tag missing))))
    (if missing
	(concat "Missing tag"
		(if (> (length missing) 1) "s @" " @")
		(mapconcat 'identity missing ", @")))))

(defun jde-javadoc-check-suggest-tag-order (tag-list reference)
  "Return a list of tags in suggested order from TAG-LIST.
REFERENCE is the list of tags allowed."
  (let (otl utl)
    (while tag-list
      (if (member (car tag-list) semantic-java-doc-line-tags)
	  (and (member (car tag-list) reference)
	       (add-to-list 'otl (car tag-list)))
	(add-to-list 'utl (car tag-list)))
      (setq tag-list (cdr tag-list)))
    (append (sort otl #'semantic-java-doc-keyword-before-p)
	    (nreverse utl))))

(defun jde-javadoc-check-tag-ordered (doctree reference)
  "Return a message if tags in DOCTREE are not correctly ordered.
REFERENCE is the list of allowed tags in correct order.  See variable
`semantic-java-doc-line-tags'."
  (let* ((tag-list (jde-javadoc-doctree-tag-list doctree))
	 (tag      (car tag-list))
	 (l        (cdr tag-list))
	 (ok       t))
    (while (and l ok)
      (jde-javadoc-dynamic-status)
      (setq ok  (semantic-java-doc-keyword-before-p tag (car l))
	    tag (car l)
	    l   (cdr l)))
    (if ok
	nil
      (concat "Recommended tag order is @"
	      (mapconcat 'identity
			 (jde-javadoc-check-suggest-tag-order
			  tag-list reference)
			 ", @")))))

(defun jde-javadoc-check-tag-allowed (doctree allowed)
  "Return a message if some tags in DOCTREE are not in ALLOWED.
Third party tags (not in `semantic-java-doc-line-tags') are allways
allowed."
  (let ((invalids
	 (delq nil
	       (mapcar
		(function
		 (lambda (tag)
		   (jde-javadoc-dynamic-status)
		   (and (member tag semantic-java-doc-line-tags)
			(not (member tag allowed))
			tag)))
		(jde-javadoc-doctree-tag-list doctree)))))
    (if (not invalids)
	nil
      (concat "Invalid tag"
	      (if (> (length invalids) 1) "s @" " @")
	      (mapconcat 'identity invalids ", @")))))

;; Tag based doc checkers
;;
(defun jde-javadoc-check-type (tag doctree)
  "Check doc of 'type' (class or interface) TAG.
DOCTREE is the current doctree of TAG.  Return a non-nil report if
errors were found."
  (let ((name (semantic-tag-name tag))
	(type (semantic-tag-type tag))
	(main (jde-javadoc-checker-main-type-p tag))
	report)
    ;; Check for missing description
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-description doctree)
	   report))
    ;; Check for missing tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-required-tags
	    doctree semantic-java-doc-type-tags
	    semantic-java-doc-extra-type-tags
	    (if main
		nil
	      ;; Don't check these tags if internal type.
	      '("author" "version" "since")))
	   report))
    ;; Check for incorrect tag order
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-ordered
	    doctree semantic-java-doc-type-tags)
	   report))
    ;; Check for invalid tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-allowed
	    doctree semantic-java-doc-type-tags)
	   report))
    ;; Setup the error summary
    (jde-javadoc-dynamic-status)
    (jde-javadoc-check-add-summary report type name)))

(defun jde-javadoc-check-variable (tag doctree)
  "Check doc of 'variable' (field) TAG.
DOCTREE is the current doctree of TAG.  Return a non-nil report if
errors were found."
  (let ((name (semantic-tag-name tag))
	report)
    ;; Check for missing description
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-description doctree)
	   report))
    ;; Check for missing tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-required-tags
	    doctree semantic-java-doc-variable-tags
	    semantic-java-doc-extra-variable-tags)
	   report))
    ;; Check for incorrect tag order
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-ordered
	    doctree semantic-java-doc-variable-tags)
	   report))
    ;; Check for invalid tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-allowed
	    doctree semantic-java-doc-variable-tags)
	   report))
    ;; Setup the error summary
    (jde-javadoc-dynamic-status)
    (jde-javadoc-check-add-summary report "variable" name)))

(defun jde-javadoc-check-function (tag doctree)
  "Check doc of 'function' (method or constructor) TAG.
DOCTREE is the current doctree of TAG.  Return a non-nil report if
errors were found."
  (let ((name   (semantic-tag-name               tag))
	(type   (semantic-tag-type               tag))
	(args   (semantic-tag-function-arguments      tag))
	(throws (semantic-tag-function-throws    tag))
	report items item)
    ;; Check for missing description
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-description doctree)
	   report))
    ;; Check for missing tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-required-tags
	    doctree semantic-java-doc-function-tags
	    semantic-java-doc-extra-function-tags
	    ;; Don't check return, param and exception/throws tags.
	    '("return") semantic-java-doc-with-name-tags)
	   report))
    ;; Check for missing @param tags
    (setq items nil)
    (while args
      (jde-javadoc-dynamic-status)
      (if (semantic-tag-p (car args))
	  (progn
	    (setq item (jde-javadoc-variable-name
			(semantic-tag-name (car args))))
	    (setq items (cons item items))
	    (or (jde-javadoc-doctree-tag doctree "param" item)
		(setq report
		      (cons
		       (format "Missing @param tag for `%s'" item)
		       report)))))
      (setq args (cdr args)))
    ;; Check for extra @param tags
    (setq args (jde-javadoc-doctree-tag doctree "param"))
    (while args
      (jde-javadoc-dynamic-status)
      (setq item (jde-javadoc-variable-name (cdr (car args))))
      (or (member item items)
	  (setq report (cons (format "Invalid @param tag `%s'" item)
			     report)))
      (setq args (cdr args)))
    ;; Check for missing @exception tags
    (setq items nil)
    (while throws
      (jde-javadoc-dynamic-status)
      (setq item (car throws))
      (setq items (cons item items))
      (setq throws (cdr throws))
      (or (jde-javadoc-doctree-tag doctree "exception" item)
	  (setq report
		(cons
		 (format "Missing @exception tag for `%s'" item)
		 report))))
    ;; Check for extra @exception tags
    (setq args (jde-javadoc-doctree-tag doctree "exception"))
    (while args
      (jde-javadoc-dynamic-status)
      (setq item (cdr (car args)))
      (or (member item items)
	  (and jde-javadoc-check-undeclared-exception-flag
	       (jde-javadoc-implicit-exception-p item))
	  (setq report
		(cons
		 (format
		  "Extra @exception tag `%s' (maybe not an error)"
		  item)
		 report)))
      (setq args (cdr args)))
    ;; Check for missing or extra @return tag
    (setq item (jde-javadoc-doctree-tag doctree "return"))
    (jde-javadoc-dynamic-status)
    (cond ((and (not type) item)
	   (setq report (cons "Invalid @return tag for constructor"
			      report)))
	  ((and type (string-equal type "void") item)
	   (setq report (cons "Invalid @return tag for void method"
			      report)))
	  ((and type (not (string-equal type "void")) (not item))
	   (setq report (cons "Missing @return tag"
			      report))))
    ;; Check for incorrect tag order
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-ordered
	    doctree semantic-java-doc-function-tags)
	   report))
    ;; Check for invalid tags
    (jde-javadoc-dynamic-status)
    (setq report
	  (cons
	   (jde-javadoc-check-tag-allowed
	    doctree semantic-java-doc-function-tags)
	   report))
    ;; Setup the error summary
    (jde-javadoc-dynamic-status)
    (jde-javadoc-check-add-summary report
				   (if type "method" "constructor")
				   name)))

(defun jde-javadoc-checker (tag &optional noreport)
  "Call the javadoc checker associated to TAG.
If optional NOREPORT is non-nil does not show error report.  Return a
non-nil report if errors were found."
  (let* ((type    (semantic-tag-class tag))
	 (checker (intern (format "jde-javadoc-check-%s" type))))
    (or (fboundp checker)
	(error "No checker found to process '%S' tag" type))
    (goto-char (semantic-tag-start tag))
    (recenter 1)
    (let (doctree report)
      (jde-javadoc-status-forms "Checking" "done"
	(jde-javadoc-dynamic-status)
	(setq doctree (jde-javadoc-tag-doctree tag))
	(setq report  (funcall checker tag doctree))
	(or noreport
	    (jde-javadoc-checker-show-report report tag))
	(jde-javadoc-dynamic-status t))
      report)))

(defcustom jde-javadoc-checker-level 'protected
  "*Accessibility level to check.
Only 'type, 'function or 'variable tags with this level will be
checked.  The level is defined to be consistent with the javadoc show
options.  That is:

- - public    - check only public classes and members.
- - protected - check only protected and public classes and
		members.  This is the default.
- - package   - check only package, protected, and public classes
		and members.
- - private   - check all classes and members."
  :group 'jde-javadoc
  :type  '(choice :tag "level"
		  (const :tag "public"    public)
		  (const :tag "protected" protected)
		  (const :tag "package"   package)
		  (const :tag "private"   private)))

(defconst jde-javadoc-access-level-weights
  '((public    . 8)
    (protected . 4)
    (package   . 2)
    (private   . 0))
  "Java access level weights.")

(defun jde-javadoc-access-level-lower-p (l1 l2)
  "Return non-nil if access level L1 is lower than L2."
  (< (cdr (assq l1 jde-javadoc-access-level-weights))
     (cdr (assq l2 jde-javadoc-access-level-weights))))

(defun jde-javadoc-tag-access-level (tag)
  "Return the access level of TAG.
That is 'public 'package 'protected or 'private.  If TAG is included
in other ones its access level is the lowest one found in the
hierarchy."
  (let ((deps (semantic-find-tag-by-overlay
	       (semantic-tag-start tag)
	       (semantic-tag-buffer tag)))
	last-type tag categ modifiers levels)
    (while deps
      (setq tag (car deps))
      (setq deps  (cdr deps))
      (setq categ (semantic-tag-class tag))
      (setq modifiers
	    (cond
	     ((eq categ 'type)
	      (setq last-type (semantic-tag-type tag))
	      (semantic-tag-modifiers tag))
	     ((eq categ 'function)
	      (if (string-equal last-type "interface")
		  (list "public") ;; interface members are always public
		(semantic-tag-modifiers tag)))
	     ((eq categ 'variable)
	      (if (string-equal last-type "interface")
		  (list "public") ;; interface members are always public
		(semantic-tag-modifiers tag)))
	     (t ;; must never occurs
	      (error "Invalid %s tag" categ))))
      (setq levels
	    (cons (cond ((member "public" modifiers)
			 'public)
			((member "protected" modifiers)
			 'protected)
			((member "private" modifiers)
			 'private)
			(t
			 'package))
		  levels)))
    (car (sort levels 'jde-javadoc-access-level-lower-p))))

(defun jde-javadoc-checker-at-level-p (tag)
  "Return non-nil if checking is allowed for TAG.
That is TAG accessibility level is greater than or equal to the one
specified by `jde-javadoc-checker-level'.  TAG category must be
'type, 'function or 'variable."
  (let ((level (or jde-javadoc-checker-level 'protected)))
    ;; if level is 'private check all
    (or (eq level 'private)
	;; else check if tag access level >= checker level
	(not (jde-javadoc-access-level-lower-p
	      (jde-javadoc-tag-access-level tag)
	      level)))))

(defun jde-javadoc-checker-main-type-p (&optional tag)
  "Return non-nil if TAG is a main type one.
That is not an internal class or interface."
  (setq tag (or tag (semantic-current-tag)))
  (and (eq (semantic-tag-class tag) 'type)
       (memq tag (semantic-brute-find-tag-by-class
		    'type (current-buffer)))))

(defun jde-javadoc-checker-do-find-previous-tag (tags &optional
							  tag prev)
  "Visit TAGS and return the tag before TAG.
PREV is the last tag visited or nil at start.  If TAG is nil
return the last tag found.  Return only a 'type 'function or
'variable tag."
  (let (current categ)
    (while tags
      (setq current (car tags))
      (setq tags  (cdr tags))
      (setq categ   (semantic-tag-class current))
      (if (memq categ '(type function variable))
	  (cond
	   ((null tag)
	    (if (null tags)
		(throw 'found prev)))
	   ((>= (semantic-tag-start current)
		(semantic-tag-start tag))
	    (throw 'found prev))
	   (t
	    (setq prev
		  (if (eq categ 'type)
		      (jde-javadoc-checker-do-find-previous-tag
		       (semantic-tag-type-members current)
		       tag current)
		    current))))
	))
    prev))

(defun jde-javadoc-checker-find-previous-tag (tags
						&optional tag)
  "Visit TAGS and return the tag before TAG.
If TAG is nil return the last tag found.  Return only a 'type
'function or 'variable tag."
  (catch 'found
    (jde-javadoc-checker-do-find-previous-tag tags tag)))

(defun jde-javadoc-checker-find-next-tag (tags &optional tag)
  "Visit TAGS and return the tag following TAG.
If TAG is nil return the first tag found.  Return only a 'type
'function or 'variable tag."
  (let (current next categ)
    (while (and tags (not next))
      (setq current (car tags))
      (setq categ   (semantic-tag-class current))
      (if (memq categ '(type function variable))
	  (if (or (null tag)
		  (> (semantic-tag-start current)
		     (semantic-tag-start tag)))
	      (setq next current)
	    (if (eq categ 'type)
		(setq next (jde-javadoc-checker-find-next-tag
			    (semantic-tag-type-members current)
			    tag)))))
      (setq tags (cdr tags)))
    next))

(defun jde-javadoc-checker-previous-tag (buffer &optional tag)
  "Report the previous tag in BUFFER with documentation errors.
Start checking before TAG if non-nil or at the last tag found."
  (pop-to-buffer buffer)
  (let* ((tags (semantic-fetch-tags))
	 (prev   (jde-javadoc-checker-find-previous-tag
		  tags tag))
	 (report (and prev
		      (jde-javadoc-checker-at-level-p prev)
		      (jde-javadoc-checker prev t))))
    (while (and prev (not report))
      (setq prev   (jde-javadoc-checker-find-previous-tag
		    tags prev))
      (setq report (and prev
			(jde-javadoc-checker-at-level-p prev)
			(jde-javadoc-checker prev t))))
    (if report
	(jde-javadoc-checker-show-report report prev)
      (if tag
	  (if (y-or-n-p "No more doc error found.  Quit? ")
	      (jde-javadoc-checker-quit)
	    (jde-javadoc-checker tag))
	(message "No doc errors found")
	(jde-javadoc-checker-quit)))))

(defun jde-javadoc-checker-next-tag (buffer &optional tag)
  "Report the next tag in BUFFER with documentation errors.
Start checking after TAG if non-nil or at the first tag found."
  (pop-to-buffer buffer)
  (let* ((tags (semantic-fetch-tags))
	 (next   (jde-javadoc-checker-find-next-tag tags tag))
	 (report (and next
		      (jde-javadoc-checker-at-level-p next)
		      (jde-javadoc-checker next t))))
    (while (and next (not report))
      (setq next   (jde-javadoc-checker-find-next-tag tags next))
      (setq report (and next
			(jde-javadoc-checker-at-level-p next)
			(jde-javadoc-checker next t))))
    (if report
	(jde-javadoc-checker-show-report report next)
      (if tag
	  (if (y-or-n-p "No more doc error found.  Quit? ")
	      (jde-javadoc-checker-quit)
	    (jde-javadoc-checker tag))
	(message "No doc errors found")
	(jde-javadoc-checker-quit)))))

;;;; Doc generator
;;;; -------------

(defun jde-javadoc-insert (*name* &rest *texts*)
  "Insert the template *NAME* or *TEXTS* and a newline.
If *NAME* value is nil *TEXTS* are inserted if non-nil.  If *NAME* and
*TEXTS* are nil the function does nothing.
The name of variables local to this function are enclosed between
\"*\" to avoid conflicts with variables used in templates."
  (cond ((and *name* (symbolp *name*) (symbol-value *name*))
	 (tempo-insert-template *name* nil)
	 (newline))
	(*texts*
	 (apply #'insert *texts*)
	 (newline))))

;; Basic generators
;;
(defun jde-javadoc-insert-start-block ()
  "Insert a javadoc comment block start '/**' at point."
  (jde-javadoc-insert nil "/**"))

(defun jde-javadoc-insert-empty-line ()
  "Insert an empty javadoc line '*'."
  (jde-javadoc-insert nil "*"))

(defun jde-javadoc-insert-end-block (&optional indent)
  "Insert a javadoc end comment block."
  (jde-javadoc-insert 'tempo-template-jde-javadoc-end-block "*/")
  (if indent
      (jde-javadoc-indent-line))
  nil) ;; must return nil to prevent template insertion.

(defun jde-javadoc-insert-previous-description (doctree)
  "Insert a javadoc description if it already exists in DOCTREE."
  (let ((previous (jde-javadoc-doctree-tag
		   doctree jde-javadoc-desc-tag)))
    (if previous
	(jde-javadoc-insert
	 nil
	 "/**"
	 (jde-javadoc-normalize-description (car (car previous)))))
    previous))

(defun jde-javadoc-insert-previous-tag (doctree tag &optional key)
  "If it already exists in DOCTREE, insert javadoc TAG value(s).
If optional KEY is non-nil insert its specific value."
  (let ((previous (jde-javadoc-doctree-tag doctree tag key)))
    (if previous
	(jde-javadoc-map
	 (function
	  (lambda (item)
	    (jde-javadoc-dynamic-status)
	    (jde-javadoc-insert nil "* @" tag (car item))))
	 previous))
    previous))

(defun jde-javadoc-insert-unknown-tags (doctree)
  "Insert unknown tags found in DOCTREE.
See `jde-javadoc-doctree-unknown-tag-list'."
  (jde-javadoc-map
   (function
    (lambda (tag)
      (jde-javadoc-dynamic-status)
      (jde-javadoc-insert-previous-tag doctree tag)))
   (jde-javadoc-doctree-unknown-tag-list doctree)))

(defun jde-javadoc-insert-author-tag (doctree)
  "Insert javadoc @author tags.
If tags already exist in DOCTREE keep them, else insert a new default
one."
  (or (jde-javadoc-insert-previous-tag doctree "author")
      (jde-javadoc-insert 'tempo-template-jde-javadoc-author-tag)))

;; (defun jde-javadoc-insert-since-tag (doctree)
;;   "Insert a javadoc @since tag.
;; If tag already exists in DOCTREE keep it, else insert a new default
;; one."
;;   (or (jde-javadoc-insert-previous-tag doctree "since")
;;       (jde-javadoc-insert 'tempo-template-jde-javadoc-since-tag)))

(defun jde-javadoc-insert-version-tag (doctree)
  "Insert a javadoc @version tag.
If tag already exists in DOCTREE keep it, else insert a new default
one."
  (or (jde-javadoc-insert-previous-tag doctree "version")
      (jde-javadoc-insert 'tempo-template-jde-javadoc-version-tag)))

;; (defun jde-javadoc-insert-see-tag (doctree refs)
;;   "Insert javadoc @see tags.
;; If tags already exist in DOCTREE keep them, else insert a new
;; default one for each item in REFS."
;;   (or (jde-javadoc-insert-previous-tag doctree "see")
;;       (jde-javadoc-map
;;        (function
;;         (lambda (ref)
;;           (and ref (not (string= ref ""))
;;                (jde-javadoc-insert
;;                 'tempo-template-jde-javadoc-see-tag))))
;;        refs)))

(defun jde-javadoc-insert-param-tag (doctree type name)
  "Insert a javadoc @param tag.
If tag already exists in DOCTREE keep it, else insert a new default
one using parameter TYPE and NAME."
  (or (jde-javadoc-insert-previous-tag doctree "param" name)
      (and type (not (string= type ""))
	   name (not (string= name ""))
	   (jde-javadoc-insert
	    'tempo-template-jde-javadoc-param-tag))))

(defun jde-javadoc-insert-exception-tag (doctree types)
  "Insert javadoc @exception tags.
If tags already exist in DOCTREE keep them, else insert a new default
one for each exception in TYPES."
  (jde-javadoc-map
   (function
    (lambda (type)
      (jde-javadoc-dynamic-status)
      (or (jde-javadoc-insert-previous-tag doctree "exception" type)
	  (and type (not (string= type ""))
	       (jde-javadoc-insert
		'tempo-template-jde-javadoc-exception-tag)))))
   types)
  ;; Keep extra exception tags (maybe not invalid tags)
  (jde-javadoc-map
   (function
    (lambda (type)
      (jde-javadoc-dynamic-status)
      (setq type (cdr type))
      (or (member type types)
	  (if (or (not jde-javadoc-check-undeclared-exception-flag)
		  (jde-javadoc-implicit-exception-p type))
	      (jde-javadoc-insert-previous-tag
	       doctree "exception" type)))))
   (jde-javadoc-doctree-tag doctree "exception")))

(defun jde-javadoc-insert-return-tag (doctree type)
  "Insert a javadoc @return tag.
If tag already exists in DOCTREE keep it, else insert a new default
one using return TYPE."
  (and type (not (string= type "void"))
       (or (jde-javadoc-insert-previous-tag doctree "return")
	   (jde-javadoc-insert
	    'tempo-template-jde-javadoc-return-tag))))

(defun jde-javadoc-insert-field-desc (doctree modifiers type name)
  "Insert a field description.
If a description already exists in DOCTREE keep it, else insert a
default one using field MODIFIERS TYPE and NAME."
  (if (jde-javadoc-insert-previous-description doctree)
      nil
    (jde-javadoc-insert-start-block)
    (jde-javadoc-insert 'tempo-template-jde-javadoc-describe-field)
    (jde-javadoc-insert-empty-line)))

(defun jde-javadoc-insert-function-desc (doctree type name)
  "Insert a method or constructor description.
If a description already exists in DOCTREE keep it, else insert a
default one using method TYPE and NAME.  If TYPE is nil insert a
default constructor description."
  (if (jde-javadoc-insert-previous-description doctree)
      nil
    (jde-javadoc-insert-start-block)
    (if (and name (not (string= name "")))
	(jde-javadoc-insert
	 (if (and type (not (string= type "")))
	     'tempo-template-jde-javadoc-describe-method
	   'tempo-template-jde-javadoc-describe-constructor)))
    (jde-javadoc-insert-empty-line)))

(defun jde-javadoc-insert-class-desc (doctree name)
  "Insert a class description.
If a description already exists in DOCTREE keep it, else insert a
default one using class NAME."
  (if (jde-javadoc-insert-previous-description doctree)
      nil
    (jde-javadoc-insert-start-block)
    (jde-javadoc-insert 'tempo-template-jde-javadoc-describe-class)
    (jde-javadoc-insert-empty-line)))

(defun jde-javadoc-insert-interface-desc (doctree name)
  "Insert an interface description.
If a description already exists in DOCTREE keep it, else insert a
default one using interface NAME."
  (if (jde-javadoc-insert-previous-description doctree)
      nil
    (jde-javadoc-insert-start-block)
    (jde-javadoc-insert
     'tempo-template-jde-javadoc-describe-interface)
    (jde-javadoc-insert-empty-line)))

;; Main generators
;;
(defun jde-javadoc-type-doc (modifiers type name parents main
				       &optional doctree)
  "Document a class or interface using MODIFIERS TYPE NAME PARENTS.
If MAIN is non-nil this is a main class or interface (not internal).
If description and tags already exist in DOCTREE keep them."
  ;; description
  (jde-javadoc-dynamic-status)
  (if (string-equal type "interface")
      (jde-javadoc-insert-interface-desc doctree name)
    (jde-javadoc-insert-class-desc doctree name))
  (when main
    ;; author
    (jde-javadoc-dynamic-status)
    (jde-javadoc-insert-author-tag doctree)
    ;; version
    (jde-javadoc-dynamic-status)
    (jde-javadoc-insert-version-tag doctree))
  ;; Keep extra optional tags if they already exist
  (jde-javadoc-dynamic-status)
  (jde-javadoc-map (function
		    (lambda (tag)
		      (jde-javadoc-insert-previous-tag doctree tag)))
		   semantic-java-doc-extra-type-tags)
  ;; Keep unknown (not Sun's) tags
  (jde-javadoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (jde-javadoc-dynamic-status)
  (jde-javadoc-insert-end-block t))

(defun jde-javadoc-variable-doc (modifiers type name
					   &optional doctree)
  "Document a field using MODIFIERS TYPE NAME.
If description and tags already exist in DOCTREE keep them."
  ;; description
  (jde-javadoc-insert-field-desc doctree modifiers type name)
  ;; Keep extra optional tags if they already exist
  (jde-javadoc-map (function
		    (lambda (tag)
		      (jde-javadoc-insert-previous-tag doctree tag)))
		   semantic-java-doc-extra-variable-tags)
  ;; Keep unknown (not Sun's) tags
  (jde-javadoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (jde-javadoc-insert-end-block t))

(defun jde-javadoc-function-args-doc (tags &optional doctree)
  "Document function arguments in TAGS.
If tags already exist in DOCTREE keep them."
  (jde-javadoc-map
   (function
    (lambda (tag)
      (if (semantic-tag-p tag)      ; because TAGS can be (nil)
	  (let ((modifiers (semantic-tag-modifiers tag))
		(type      (semantic-tag-type               tag))
		(name      (semantic-tag-name               tag)))
	    (jde-javadoc-insert-param-tag doctree type name)))))
   tags))

(defun jde-javadoc-function-doc (modifiers type name args throws
					   &optional doctree)
  "Document a function using MODIFIERS TYPE NAME ARGS THROWS.
If description and tags already exist in DOCTREE keep them."
  ;; description
  (jde-javadoc-insert-function-desc doctree type name)
  ;; param
  (jde-javadoc-function-args-doc args doctree)
  ;; return
  (jde-javadoc-insert-return-tag doctree type)
  ;; exception
  (jde-javadoc-insert-exception-tag doctree throws)
  ;; Keep extra optional tags if they already exist
  (jde-javadoc-map (function
		    (lambda (tag)
		      (jde-javadoc-insert-previous-tag doctree tag)))
		   semantic-java-doc-extra-function-tags)
  ;; Keep unknown (not Sun's) tags
  (jde-javadoc-insert-unknown-tags doctree)
  ;; The end of comment block
  (jde-javadoc-insert-end-block t))

;; Main tag based generators
;;
(defun jde-javadoc-type (tag doctree)
  "Document a 'type' (class or interface) TAG.
DOCTREE is the current doctree of TAG."
  (let ((modifiers  (semantic-tag-modifiers tag))
	(type       (semantic-tag-type               tag))
	(name       (semantic-tag-name               tag))
	(parents    (semantic-tag-type-superclasses  tag))
	(main       (jde-javadoc-checker-main-type-p tag)))
    (jde-javadoc-dynamic-status)
    (jde-javadoc-type-doc modifiers type name parents main doctree)))

(defun jde-javadoc-variable (tag doctree)
  "Document a 'variable' (field) TAG.
DOCTREE is the current doctree of TAG."
  (let ((modifiers  (semantic-tag-modifiers tag))
	(type       (semantic-tag-type               tag))
	(name       (semantic-tag-name               tag)))
    (jde-javadoc-dynamic-status)
    (jde-javadoc-variable-doc modifiers type name doctree)))

(defun jde-javadoc-function (tag doctree)
  "Document a 'function' (method or constructor) TAG.
DOCTREE is the current doctree of TAG."
  (let ((modifiers  (semantic-tag-modifiers tag))
	(type       (semantic-tag-type               tag))
	(name       (semantic-tag-name               tag))
	(args       (semantic-tag-function-arguments tag))
	(throws     (semantic-tag-function-throws    tag)))
    (jde-javadoc-dynamic-status)
    (jde-javadoc-function-doc
     modifiers type name args throws doctree)))

(defun jde-javadoc-generator (tag &optional noconfirm)
  "Call the javadoc generator associated to TAG.
Require confirmation to delete existing documentation if optional
NOCONFIRM is non-nil."
  (let* ((type      (semantic-tag-class tag))
	 (generator (intern (format "jde-javadoc-%s" type)))
	 (checker   (intern (format "jde-javadoc-check-%s" type))))
    (or (fboundp generator)
	(error "No generator found to process '%S' tag" type))
    (goto-char (semantic-tag-start tag))
    (let ((report t) doctree)
      (jde-javadoc-status-forms "Updating" "done"
	(jde-javadoc-dynamic-status)
	(setq doctree (jde-javadoc-tag-doctree tag))
	(if (and (fboundp checker)
		 (not (funcall checker tag doctree)))
	    (setq report nil)
	  (let ((seas semantic-edits-are-safe))
	    (make-local-variable 'semantic-edits-are-safe)
	    (unwind-protect
		(progn
		  (setq semantic-edits-are-safe t)
		  (when (or (not doctree)
			    (jde-javadoc-delete-documentation
			     tag noconfirm))
		    (funcall generator tag doctree)
		    (jde-javadoc-indent-documentation tag)))
	      (setq semantic-edits-are-safe seas))))
	(jde-javadoc-dynamic-status t))
      (or report
	  (message "Documentation is up-to-date")))))

;;;; Misc command auxiliaries
;;;; ------------------------

(defun jde-javadoc-nonterminal-at-line ()
  "Search the bovine table for the tag at the current line."
  (save-excursion
    ;; Preserve current tags when the lexer fails (when there is an
    ;; unclosed block or an unterminated string for example).
    (let ((semantic-flex-unterminated-syntax-end-function
	   #'(lambda (syntax &rest ignore)
	       (throw 'jde-javadoc-flex-error syntax))))
      (catch 'jde-javadoc-flex-error
	(semantic-fetch-tags))))
  (save-excursion
    ;; Move the point to the first non-blank character found.  Skip
    ;; spaces, tabs and newlines.
    (beginning-of-line)
;;    (jde-javadoc-skip-spaces-forward)
    (forward-comment (point-max))
    (semantic-current-tag)))

(defvar jde-javadoc-checkdoc-excursion nil
  "Window configuration and point before running doc checker.")
;; Maybe `jde-javadoc-checkdoc-excursion' could be made buffer local?
;;(make-variable-buffer-local 'jde-javadoc-checkdoc-excursion)

(defun jde-javadoc-checkdoc-clear-excursion ()
  "Clear saved window configuration and point.
See also variable `jde-javadoc-checkdoc-excursion'."
  (setq jde-javadoc-checkdoc-excursion nil))

(defun jde-javadoc-checkdoc-save-excursion ()
  "Save window configuration and point.
See also function `jde-javadoc-checkdoc-restore-excursion'."
  (setq jde-javadoc-checkdoc-excursion
	(vector (current-window-configuration)
		(point))))

(defun jde-javadoc-checkdoc-restore-excursion ()
  "Restore window configuration and point.
See also function `jde-javadoc-checkdoc-save-excursion'."
  (let ((ex jde-javadoc-checkdoc-excursion))
    (and (vectorp ex)
	 (window-configuration-p (aref ex 0))
	 (integer-or-marker-p    (aref ex 1))
	 (condition-case nil
	     (progn
	       (set-window-configuration (aref ex 0))
	       (goto-char                (aref ex 1)))
	   (error nil)))
    (jde-javadoc-checkdoc-clear-excursion)))

;;;; Commands
;;;; --------

;;;###autoload
(defun jde-javadoc-checker-previous ()
  "Go to the previous tag with doc errors."
  (interactive)
  (and (eq major-mode 'jde-javadoc-checker-report-mode)
       (jde-javadoc-checker-previous-tag
	jde-javadoc-checker-buffer jde-javadoc-checker-tag)))

;;;###autoload
(defun jde-javadoc-checker-next ()
  "Goto the next tag with doc errors."
  (interactive)
  (and (eq major-mode 'jde-javadoc-checker-report-mode)
       (jde-javadoc-checker-next-tag jde-javadoc-checker-buffer
				       jde-javadoc-checker-tag)))
;;;###autoload
(defun jde-javadoc-checker-fix ()
  "Fix documentation of checked tag.
Used in `jde-javadoc-checker-report-mode'."
  (interactive)
  (and (eq major-mode 'jde-javadoc-checker-report-mode)
       (let ((tag  jde-javadoc-checker-tag)
	     (buffer jde-javadoc-checker-buffer))
	 (when (and tag buffer)
	   (pop-to-buffer buffer)
	   (goto-char (semantic-tag-start tag))
	   ;; do the fix (THE POINT STAY AT TAG START POSITION)
	   (jde-javadoc-generator tag t)
	   ;; recheck the tag
	   (jde-javadoc-checker tag)))))

;;;###autoload
(defun jde-javadoc-checker-quit ()
  "Quit the `jde-javadoc-checker' report buffer.
Used in `jde-javadoc-checker-report-mode'."
  (interactive)
  (let ((buffer (get-buffer jde-javadoc-checker-report-buffer)))
    (if (bufferp buffer)
	(kill-buffer buffer))
    (jde-javadoc-checkdoc-restore-excursion)))

;;;###autoload
(defun jde-javadoc-customize ()
  "Show the jde-javadoc options panel."
  (interactive)
  (customize-group "jde-javadoc"))

;;;###autoload
(defun jde-javadoc-autodoc-at-line ()
  "Update javadoc comment block for declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN.

In the following examples, point is located at the beginning of the
line, before the word 'public' (but it could be anywhere on this
line):

1- Class example:
   -------------

-|-  public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe class <code>MyClass</code> here.
+     *
+     * @author \"David Ponce\" <david.ponce@wanadoo.fr>
+     * @version 1.0
+     * @since 1.0
+     * @see MySuperClass
+     * @see Runnable
+     * @see java.io.Serializable
+     */
     public class MyClass
       extends MySuperClass implements Runnable, java.io.Serializable
     {
       ...

2- Method example:
   --------------

-|-  public
     void   myMethod( int  x,  int y )
       throws Exception
     {
       ...

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe <code>myMethod</code> method here.
+     *
+     * @param x an <code>int</code> value
+     * @param y a <code>long</code> value
+     * @exception Exception if an error occurs
+     */
     public
     void   myMethod( int  x,  long y )
       throws Exception
     {
       ...

3- Field example:
   --------------

-|-  private static final int SIZE = 10;

\\[jde-javadoc-autodoc-at-line] inserts:

+    /**
+     * Describe constant <code>SIZE</code> here.
+     */
     private static final int SIZE = 10;


`tempo' templates are used for each category of javadoc line.  The
following templates are currently defined and fully customizable (see
`tempo-define-template' for the different items that can be used in a
tempo template):

- - `jde-javadoc-author-tag-template'
- - `jde-javadoc-describe-class-template'
- - `jde-javadoc-describe-constructor-template'
- - `jde-javadoc-describe-interface-template'
- - `jde-javadoc-describe-method-template'
- - `jde-javadoc-describe-field-template'
- - `jde-javadoc-exception-tag-template'
- - `jde-javadoc-param-tag-template'
- - `jde-javadoc-return-tag-template'
- - `jde-javadoc-version-tag-template'

For example if you customize `jde-javadoc-describe-class-template'
with the following value:

'(\"* \" (P \"Class description: \"))

you will be asked to enter the class description in the minibuffer.
See also the `jde-javadoc-field-type', `jde-javadoc-a' and
`jde-javadoc-code' helper functions."
  (interactive)
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (let ((found (jde-javadoc-nonterminal-at-line)))
    (if found
	(jde-javadoc-generator found)
      (error "No tag found at point"))))

;;;###autoload
(defun jde-javadoc-remdoc-at-line (&optional noconfirm)
  "Remove javadoc comment block for declaration at current line.
Require confirmation if optional NOCONFIRM is non-nil.
Return non-nil if done.
This uses `jde-javadoc-nonterminal-at-line' to find declarations."
  (interactive "P")
  (jde-assert-source-buffer)
  (let ((found (jde-javadoc-nonterminal-at-line)))
    (if (not found)
	(error "No tag found at point")
      (jde-javadoc-delete-documentation found current-prefix-arg))))

;;;###autoload
(defun jde-javadoc-checkdoc-at-line ()
  "Check javadoc comment block of declaration at current line.

Uses the semantic bovinator parser table to find declarations (see
`jde-javadoc-nonterminal-at-line').

BEFORE EXECUTING THE COMMAND, THE POINT MUST BE LOCATED AT THE FIRST
LINE OF THE CLASS OR METHOD DECLARATION.  IF NOT RESULT IS UNCERTAIN."
  (interactive)
  (jde-assert-source-buffer)
  (let ((found (jde-javadoc-nonterminal-at-line)))
    (if (not found)
	(error "No tag found at point")
      (jde-javadoc-checkdoc-save-excursion)
      (jde-javadoc-checker found))))

;;;###autoload
(defun jde-javadoc-checkdoc ()
  "Check doc comments of tags in the current buffer.
Report the next tag with documentation errors."
  (interactive)
  (jde-assert-source-buffer)
  (semantic-fetch-tags)
  (jde-javadoc-checkdoc-save-excursion)
  (jde-javadoc-checker-next-tag (current-buffer)))

(defalias 'jde-javadoc-autodoc 'jde-javadoc-checkdoc)

;;; Compatibility
;;
(defalias 'jde-javadoc-generate-javadoc-template
  'jde-javadoc-autodoc-at-line)

;; Load the javadoc builder
(require 'jde-javadoc-gen)

;;;###autoload
(defun jde-javadoc-enable-menu-p ()
  "Return non-nil if corresponding doc menu item is enabled.
That is point is on the first line of a class, method, or field
definition."
  (let ((p (save-excursion (beginning-of-line) (point)))
	(tag-at-line (jde-javadoc-nonterminal-at-line))
	start end)
    (and tag-at-line
	 (memq (semantic-tag-class tag-at-line)
	       '(function type variable))
	 (save-excursion
	   (setq start
		 (progn
		   (goto-char (semantic-tag-start tag-at-line))
		   (beginning-of-line)
		   (point)))
	   (setq end (or (re-search-forward "[;={]")
			 (progn
			   (goto-char p)
			   (end-of-line)
			   (point))))
	   (and (>= p start) (<= p end))))))

;; Register and initialize the customization variables defined
;; by this package.
(jde-update-autoloaded-symbols)

(provide 'jde-javadoc)

;; End of jde-javadoc.el
