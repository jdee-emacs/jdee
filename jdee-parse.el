;;; jdee-parse.el

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004 Paul Kinnucan.
;; Copyright (C) 2009 Paul Landes

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

(require 'avl-tree)
(require 'cl-lib)
(require 'efc)
(require 'eieio)
(require 'etags)
(require 'jdee-imenu)
(require 'rx)
(require 'semantic/ctxt)
(require 'semantic/sb)
(require 'thingatpt)

;; FIXME: refactor
(defvar jdee-complete-private)
(defvar jdee-complete-current-list)
(declare-function jdee-jeval-r "jdee-bsh" (java-statement))
(declare-function jdee-complete-find-completion-for-pair "jdee-complete" (pair &optional exact-completion access-level))
(declare-function jdee-import-find-and-import "jdee-import" (class &optional no-errors no-exclude qualifiedp))

;; FIXME: (require 'cc-engine) doesn't work in Emacs 24.3
(declare-function c-parse-state "cc-engine" ())

(defcustom jdee-auto-parse-enable t
  "Enables automatic reparsing of a Java source buffer.
After you makes changes to the buffer, but only if the buffer is less
than `jdee-auto-parse-max-buffer-size'."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-auto-parse-buffer-interval 180
  "Time in seconds between buffer change and reparse.
That is between the time between you change a Java source buffer and
the time the JDE reparses the buffer."
  :group 'jdee-project
  :type 'number)

(defcustom jdee-auto-parse-max-buffer-size 50000
  "Maximum size in bytes of buffers automatically reparsed.
Used when `jdee-auto-parse-enable' is non-nil.  Setting the threshold
to 0 causes the JDE to parse a buffer automatically regardless of its
size."
  :group 'jdee-project
  :type 'number)

(defvar jdee-parse-buffer-needs-reparse-p nil
  "non-nil if buffer changed since last parse.")
(make-variable-buffer-local 'jdee-parse-buffer-needs-reparse-p)

(defvar jdee-auto-parse-buffer-timer nil
  "Timer used to schedule automatic reparse.")
(make-variable-buffer-local 'jdee-auto-parse-buffer-timer)

(defvar jdee-parse-casting nil
  "Variable use to determined when the variable was casted")

(defvar jdee-parse-current-beginning (make-marker)
  "The beginning of the region where the last completion was inserted.")

(defvar jdee-parse-current-end (make-marker)
  "The end of the region where the last completion was inserted.")

(defvar jdee-parse-primitive-types '("byte" "char" "double" "float"
				    "int" "long" "short" "boolean")
  "Primitive Java types.")

(defvar jdee-parse-attempted-to-import nil
  "Variable use to avoid looping in jdee-parse-eval-type-of when
the type of a class could not be found an it tried to import it")

;; (makunbound 'jdee-parse-java-symbol-re)
(defvar jdee-parse-java-symbol-re
  (rx
   (1+              ;; A Java symbol comprises one or more of the following:
    (char (?A . ?Z)   ;;   - upper case characters
	  (?a . ?z)   ;;   - lower case characters
	  (?0 . ?9)   ;;   - digits
	  "[]"        ;;   - square brackets
	  "?"         ;;   - question mark
	  "_"         ;;   - underscore
	  "."         ;;   - period
	  (160 . 255) ;;   - accented characters
	  )))
"Regular expression that matches any Java symbol.")

(defun jdee-parse-after-buffer-changed ()
  "Reparse the current buffer after any change.
Called after `jdee-auto-parse-buffer-interval' seconds following a
buffer change if `jdee-auto-parse-enable' is non-nil and buffer size
match `jdee-auto-parse-max-buffer-size' threshold."
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following
  ;; guard.
  (if (eq major-mode 'jdee-mode)
      (semantic-fetch-tags)))

(defsubst jdee-parse-should-auto-parse-buffer-p ()
  "Return non-nil if the JDE should automatically reparse the buffer."
  (and jdee-auto-parse-enable
       (or
	(<= jdee-auto-parse-max-buffer-size 0)
	(< (buffer-size) jdee-auto-parse-max-buffer-size))))

(eval-when (compile)
  (defsubst jdee-auto-parse-delay ()
    "Return the time in seconds before auto-parse triggering."
    (- (timer-until jdee-auto-parse-buffer-timer (current-time)))))

(defun jdee-parse-buffer-changed-hook (begin end length)
  "Hook run when Semantic detects a change in the current buffer.
BEGIN and END are respectively the beginning and end of the range of
changed text. LENGTH is the length in bytes of the pre-change text
replaced by that range.  See also `semantic-change-hooks'."
  ;; This function should be called only in JDE buffers
  ;; but for some reason it is called in every buffer
  ;; in some versions of XEmacs. Hence the following
  ;; guard.
  (when (eq major-mode 'jdee-mode)
    (setq jdee-parse-buffer-needs-reparse-p t)
    (when (jdee-parse-should-auto-parse-buffer-p)
      (if (timerp jdee-auto-parse-buffer-timer)
	  (let ((rem (jdee-auto-parse-delay)))
	    (cond
	     ((< rem 0)
	      ;; Timer has expired, re-schedule a new auto-parse.
	      (cancel-timer jdee-auto-parse-buffer-timer)
	      (setq jdee-auto-parse-buffer-timer nil))
	     ((< rem 2) ;; less that 2 secs. before auto-parse
	      ;; The auto-parse task is about to be triggered when
	      ;; this change occurs, so it is delayed to let finish
	      ;; typing before re-parsing.
	      (timer-inc-time jdee-auto-parse-buffer-timer
			      10) ;; wait 10 secs. more.
	      (message "Auto parse delayed...")))))
      ;; Schedule a new auto-parse task.
      (or (timerp jdee-auto-parse-buffer-timer)
	  (setq jdee-auto-parse-buffer-timer
		(run-with-timer
		 jdee-auto-parse-buffer-interval
		 nil
		 #'jdee-parse-after-buffer-changed))))))

(defun jdee-parse-buffer-contains-multiple-classes-p ()
  "Return non-nil if buffer contains multiple class definitions."
  (let* ((top-level-classes
	  (semantic-brute-find-tag-by-class
	   'type
	   (semantic-fetch-tags)))
	 (top-level-class-count (length top-level-classes)))
    (or
     (>  top-level-class-count 1)
     (and
      (= top-level-class-count 1)
      (let* ((inner-class-parts (semantic-tag-type-members (car top-level-classes)))
	     (inner-classes
	      (semantic-brute-find-tag-by-class
	       'type inner-class-parts)))
	(>= (length inner-classes) 1))))))


(defvar jdee-parse-buffer-contains-multiple-classes-p nil
  "non-nil if buffer contains more than one class definition.")
(make-variable-buffer-local 'jdee-parse-buffer-contains-multiple-classes-p)


(defun jdee-parse-update-after-parse (tokens)
  "Hook run after Semantic changed the token cache.
TOKENS is the list of tokens, new value of the cache.  It can be nil
when the cache is cleared.
See also `semantic-after-toplevel-cache-change-hook'."
  (if (jdee-parse-should-auto-parse-buffer-p)
      (setq jdee-parse-buffer-needs-reparse-p nil
	    jdee-auto-parse-buffer-timer
	    (and (timerp jdee-auto-parse-buffer-timer)
		 (cancel-timer jdee-auto-parse-buffer-timer)
		 nil)))
  (if (car tokens)
      (setq jdee-parse-buffer-contains-multiple-classes-p
	    (jdee-parse-buffer-contains-multiple-classes-p)
	    jdee-parse-the-method-map
	    (jdee-parse-method-map "Method map"))))

(defun jdee-parse-update-after-partial-parse (tokens)
  "Hook run after Semantic updated the token cache.
TOKENS is the list of updated tokens.
See also `semantic-after-partial-cache-change-hook'."
  (jdee-parse-update-after-parse (semantic-fetch-tags)))


(defun jdee-parse-get-top-of-class (&optional class-regexp no-move-point)
  "Return the position and optionally go to where the class prototype ends and its
class definition (contents) begin.

CLASS-REGEXP the class regular expression to use.  This is useful for when the
source Java files has more than one class.  If this the symbol `first' then
the first class from the top of the file is used.  If this is nil then the
class that the point it in is used but errors out if not in a class.

NO-MOVE-POINT if non-nil just the position is returned, otherwise the point is
moved also."
  (interactive "sClass name (or enter for first): ")

  (if (eq class-regexp 'first)
      (setq class-regexp ".*")
    (if (or (null class-regexp) (= 0 (length class-regexp)))
	(let* ((class-name (car (jdee-parse-get-innermost-class-at-point))))
	  (if (null class-name) (error "point is not in a class definition"))
	  (setq class-regexp (regexp-quote class-name)))))

  (let* ((tokens (semantic-fetch-tags))
	 (classes (semantic-brute-find-tag-by-class 'type tokens))
	 class-parts pos)

    (setq pos
	  (save-excursion
	    (save-match-data)
	    (block outter
	      (dolist (class classes)
		;; Commented out this form because the variable is never
		;; used, causes a compile error, and I don't know it's purpose.
		;; (setq token-class-regexp (semantic-tag-name class))
		(when (or (null class-regexp)
			  (string-match class-regexp
					(semantic-tag-name class)))
		  (let ((class-parts (semantic-tag-type-members class)))

		    (setq pos (semantic-tag-start class))

		    (if (not (numberp pos))
			(error (concat "invalid token start, probably because "
				       "not semantic bovinated top level")))

		    (goto-char pos)

		    (if (not (search-forward "{" nil t))
			(error "invalid class, can't find next `{'"))

		    (return-from outter (point))))))))
    (if (and (not no-move-point) pos) (goto-char pos))
    pos))


(defun jdee-parse-get-nth-member (&optional class-name modifiers
					   member-name-regexp
					   elt goto-start-p
					   compare-method)
  "Return the point at the Nth class member (field) with given criteria.  The
first match that is a subset of the modifiers give is choosen.

CLASS-NAME the class that has the member.
MODIFIERS a list of Java string modifiers (i.e. \"private\").
MEMBER-NAME-REGEXP regular expresion of the name of the member.
ELT integer indicating the sequential member to get or ELT from the end if
negative.
GOTO-START-P if non-nil return the position at the beginning of the line,
otherwise return the position at the end of the line.
COMPARE-METHOD is either the symbols `equal' or `subset'.  This matches the
MODIFIERS creteria as an exact match or subset.  This defaults to `subset'."
  (interactive)

  (if (null member-name-regexp) (setq member-name-regexp ".*"))

  (let* ((tokens (semantic-fetch-tags))
	 (classes (semantic-brute-find-tag-by-class 'type tokens))
	 cmp-fn vars var-parts var-name var-modifiers i)

    (setq cmp-fn
	  (cond ((or (null compare-method)
		     (eq 'subset compare-method))
		 #'(lambda (modifiers var-modifiers)
		     (cl-subsetp modifiers var-modifiers :test 'equal)
		     ))
		((eq 'equal compare-method)
		 #'(lambda (modifiers var-modifiers)
		       (let ((m (copy-tree modifiers))
			     (v (copy-tree var-modifiers)))
			 (setq m (sort m 'string<)
			       v (sort v 'string<))
			 (equal m v)
			 )))
		(t (error "compare-method `%S' not supported"
			  compare-method))))

    (save-excursion
      (save-match-data
	(block outter
	  (dolist (class classes)
	    ;; commented out this line because the variable is never bound
	    ;; nor used and I don't understand it's purpose. Paul Kinnucan.
	    ;; (setq token-class-name (semantic-tag-name class))
	    (when (or (null class-name)
		      (string-equal class-name (semantic-tag-name class)))

	      (setq var-parts (semantic-tag-type-members class)
		    vars (semantic-brute-find-tag-by-class
			  'variable var-parts)
		    i 0)

	      (setq vars
		    (mapcar
		     #'(lambda (variable)
			 (setq var-modifiers
			       (if modifiers
				   (semantic-tag-modifiers variable)))
			 (if (or (and (null var-modifiers) (null modifiers))
				 (funcall cmp-fn modifiers var-modifiers))
			     variable)) vars))

	      (setq vars (remove nil vars))

	      (setq elt (cond ((not elt) 0)
			      ((< elt 0) (+ (length vars) elt))
			      (t elt)))

	      (dolist (variable vars)

		(setq var-name (semantic-tag-name variable))

		(when (and (= elt i)
			   (string-match member-name-regexp var-name))
		  (goto-char (if goto-start-p (semantic-tag-start variable)
			       (semantic-tag-end variable)))
		  (return-from outter (point)) )

		(setq i (1+ i))))))))))



(defun jdee-parse-get-member-variables (&optional tag)
  "Get all member variables of the current class as a semantic tag list.
The optional parameter `tag' can be a semantic tag, which is
then used as the current class instead of the result of `semantic-current-tag'."
  (let ((curtag (or tag (semantic-current-tag))))
    (remove nil
	    (mapcar
	     (lambda (member)
	       (if (semantic-tag-of-class-p member 'variable)
		   member
		 nil))
	     (semantic-tag-type-members curtag)))))

(defun jdee-parse-get-member-functions (&optional tag)
  "Get all member functions of the current class as a semantic tag list.
The optional parameter `tag' can be a semantic tag, which is
then used as the current class instead of the result of `semantic-current-tag'."
  (let ((curtag (or tag (semantic-current-tag))))
    (remove nil
	    (mapcar
	     (lambda (member) (if (semantic-tag-of-class-p member 'function) member nil))
	     (semantic-tag-type-members curtag)))))

(defun jdee-parse-get-serializable-members (&optional tag)
  "Get all serializable member variables of the current class as a semantic tag list.
The optional parameter `tag' can be a semantic tag, which is
then used as the current class instead of the result of `semantic-current-tag'."
  (let ((curtag (or tag (semantic-current-tag))))
    (remove nil
	    (mapcar
	     (lambda (member)
	       (let ((is-variable (semantic-tag-of-class-p member 'variable))
		     (modifiers (semantic-tag-modifiers member)))
		 (if (and is-variable (not (or (member "static" modifiers)
					       (member "transient" modifiers))))
		     member
		      nil)))
	     (semantic-tag-type-members curtag)))))

(defun jdee-parse-member-is-scalar (tag)
  "Check if tag is of a scalar type"
  (not (or (string-match "\\[.*\\]" (semantic-tag-name tag))
	   (string-match "\\[.*\\]" (semantic-tag-type tag)))))

(defun jdee-parse-member-is-primitive (tag)
  "Check if tag is of primitive type"
  (and (or (semantic-tag-of-type-p tag "byte")
	   (semantic-tag-of-type-p tag "char")
	   (semantic-tag-of-type-p tag "short")
	   (semantic-tag-of-type-p tag "boolean")
	   (semantic-tag-of-type-p tag "int")
	   (semantic-tag-of-type-p tag "long"))
       (jdee-parse-member-is-scalar tag)))

(defun jdee-parse-member-is-float (tag)
  "Check if tag is of a floating point type"
  (and (or (semantic-tag-of-type-p tag "float")
	   (semantic-tag-of-type-p tag "double")))
  (jdee-parse-member-is-scalar tag))

(defun jdee-parse-compare-member-types (a b)
  "List sorter for a class' member list. Primitive types will be considered
lower, so that they are returned at the head of the list. This ensures they will be
processed first and the more costly complex types later."
  (or (and (jdee-parse-member-is-primitive a)
	   (not (jdee-parse-member-is-primitive b)))
      (and (jdee-parse-member-is-float a)
	   (not (or (jdee-parse-member-is-primitive b)
		    (jdee-parse-member-is-float b))))
      (and (jdee-parse-member-is-scalar a)
	   (not (or (jdee-parse-member-is-primitive b)
		    (jdee-parse-member-is-float b)
		    (jdee-parse-member-is-scalar b))))))

(defun jdee-parse-get-package-name ()
  "Gets the name of the package in which the Java source file in the
current buffer resides."
  (let ((packages (semantic-brute-find-tag-by-class 'package (current-buffer))))
    (if (and (listp packages) (eq (length packages) 1))
	(semantic-tag-name (car packages)))))

(defun jdee-parse-get-package-from-name (class-name)
  "Gets the package portion of a qualified class name."
  (substring
   class-name 0
   (let ((pos  (cl-position ?. class-name :from-end t)))
     (if pos
	 pos
       0))))

(defun jdee-parse-get-unqualified-name (name)
  "Gets the last name in a qualified name."
  (let ((unqualified-name (substring name (string-match "[^.]+$" name))))
    (if unqualified-name unqualified-name name)))

(defun jdee-parse-get-super-class-at-point ()
  (condition-case err
      (let ((superClass "Object")
	    (class-re "extends[ \t]+\\([a-zA-z]+[a-zA-Z0-9._]*\\).*[ \n]*"))
	(save-excursion
	  (let ((open-brace-pos
		 (scan-lists (point) -1 1)))
	    (when open-brace-pos
	      (goto-char open-brace-pos)
	      (when (re-search-backward class-re (point-min) t)
		(looking-at class-re)
		(setq superClass (buffer-substring-no-properties
				  (match-beginning 1)
				  (match-end 1)))))))
	superClass)
    (error)))

(defconst jdee-parse-class-mod-re
  "public\\|abstract\\|final\\|static\\|strictfp\\|protected"
  "Regular expression matching class modifiers.")

(defconst jdee-parse-java-comment-re
  "/\\*\\(?:[*][^/]\\|[^*][/]\\|[^*/]\\)*[*/]?\\*/\\|//.*$"
  "Regular expression matching java comments.")

(defconst jdee-parse-java-comment-or-ws-re
  (concat jdee-parse-java-comment-re "\\|[ \t\n]")
  "Regular expression matching java comments and whitespaces.")

(defun jdee-parse-get-class-modifiers ()
  "Get modifiers of the innermost class containing point.
Returns a list constisting of the result of
`jdee-parse-get-innermost-class-at-point' followed by
all modifieres of the class.
Returns nil, if no class could be found."
  (let ((class (jdee-parse-get-innermost-class-at-point))
	(mod-or-ws-re (concat "\\(" jdee-parse-class-mod-re
			      "\\|" jdee-parse-java-comment-or-ws-re
			      "\\)\\="))
	(case-fold-search)
	(modifiers))
    (if class
	(save-excursion
	  (goto-char (cdr class))
	  (while (re-search-backward mod-or-ws-re (point-min) t)
	    (progn
	      (if (looking-at jdee-parse-class-mod-re)
		  (setq modifiers
			(cons (match-string-no-properties 0) modifiers)))))
	  (setq modifiers (cons class modifiers))))
    modifiers))

(defconst jdee-parse-class-decl-re
  (concat "^"
	  ;; comments, string literals, keywords, identifier,
	  ;; assignment operator or open parenthese:
	  "\\(?:" jdee-parse-java-comment-or-ws-re "\\|[a-zA-Z0-9_.=(]\\)*"
	  ;; keyword before classname:
	  "\\<\\(class\\|enum\\|interface\\|new\\)"
	  "\\(?:" jdee-parse-java-comment-or-ws-re "\\)+"
	  ;; package part of superclass of anonymous class:
	  "\\(?:[a-zA-Z0-9_]+\\.\\)*"
	  ;; classname:
	  "\\([a-zA-Z0-9_]+\\)"
	  ;; everything between classname and curly brace:
	  "\\(?:" jdee-parse-java-comment-or-ws-re "\\|[a-zA-Z0-9_.,()<>]\\)*"
	  "\\=")
  "Regular expression matching class declarations before point.
Point must be at opening curly brace of class.
It matches interfaces, named and anonymous classes.")

(defun jdee-parse-get-innermost-class-at-point ()
  "Get the innermost class containing point.
If point is in a class, this function returns
\(CLASS_NAME . CLASS_POSITION). CLASS_NAME is the
name of the class. For anonymous classes it is
the unqualified name of the superclass. CLASS_POSITION
is the position of the first character of the class
or interface keyword or the first character
of the new keyword in case of anonymous classes.
Returns nil, if point is not in a class."

  (semantic-refresh-tags-safe)
  (let ((left-paren-pos (c-parse-state)))
    (if left-paren-pos
	(save-excursion
	  (catch 'class-found
	    (let ((left-paren-index 0)
		  (left-paren-count (length left-paren-pos)))
	      (while (< left-paren-index left-paren-count)
		(let ((paren-pos (nth left-paren-index left-paren-pos)))
		  (unless (consp paren-pos)
		    (goto-char paren-pos)
		    (when (looking-at "{")
		      (let* ((search-end-pos
			      (if (< left-paren-index (1- left-paren-count))
				  (let ((pos (nth (1+ left-paren-index) left-paren-pos)))
				    (if (consp pos)
					(cdr pos)
				      pos))
				(point-min)))
			     (case-fold-search nil)
			     (class-pos (re-search-backward jdee-parse-class-decl-re search-end-pos t)))
			(if class-pos
			    (throw
			     'class-found
			     (cons (match-string-no-properties 2)
				   (match-beginning 1))))))))
		(setq left-paren-index (1+ left-paren-index)))))))))

(defun jdee-parse-get-class-at-point ()
  (let ((class-info (jdee-parse-get-innermost-class-at-point))
	class-name)
    (while class-info
      (let ((name (car class-info))
	    (pos (cdr class-info)))
	(if (not class-name)
	    (setq class-name name)
	  (setq class-name (concat name "." class-name)))
	(save-excursion
	  (goto-char pos)
	  (setq class-info (jdee-parse-get-innermost-class-at-point)))))
    class-name))

(defun jdee-parse-get-classes-at-point ()
  (interactive)
  (let ((class (jdee-parse-get-innermost-class-at-point)))
    (if class (message "%s %s" (car class) (cdr class) ) (message "no class")))
  ;; (goto-char (aref (c-search-uplist-for-classkey (c-parse-state)) 0))
  )


(defun jdee-parse-select-qualified-class-name (class &optional prompt)
  "PROMPT the user to select the fully qualified name for CLASS.
Return the selection."
  (condition-case err
      (let ((names
	     (jdee-jeval-r
	      (format "jde.util.JdeUtilities.getQualifiedName(\"%s\");" class))))
	(if names
	    (if (> (length names) 1)
		(efc-query-options
		 names
		 (or prompt "Select class.")
		 "Class Name Dialog")
	      (car names))
	  (error "Cannot find class %s on the current classpath." class)))
    (error
     (message "%s" (error-message-string err)))))


(defun jdee-parse-qualified-name-at-point ()
  "Returns (cons QUALIFIER NAME) where NAME is the symbol at point and
QUALIFIER is the symbol's qualifier. For example, suppose the name at
point is

     int i = error.msg.length()
		   ^
In this case, this function returns (cons \"error.msg\" \"length\").
This function works only for qualified names that do not contain
white space. It returns null if there is no qualified name at point."
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
       (thing-at-point-looking-at "[^ \n\t();,:+<]+") ;; add < to prevent like "Map<String"
      (let ((qualified-name
	     (buffer-substring-no-properties
	      (match-beginning 0)
	      (match-end 0))))
	(string-match "\\(.+[.]\\)*\\([^.]+\\)" qualified-name)
	(let ((qualifier (if (match-beginning 1)
			     (substring qualified-name
					(match-beginning 1) (match-end 1))))
	      (name (substring qualified-name
			       (match-beginning 2) (match-end 2))))
	  (if qualifier
	      (setq qualifier (substring qualifier 0 (1- (length qualifier)))))
	  (cons qualifier name))))))


;;;###autoload
(defun jdee-parse-get-buffer-class (&optional no-package-p)
  "Get the fully qualified name of the class of this buffer.

NO-PACKAGE-P, if non-`nil', return only the class name (sans
package name), otherwise, include the package name.

If called interactively, add the name in the mini-buffer."
  (interactive (list (not current-prefix-arg)))
  (if (eq major-mode 'jdee-mode)
      (let ((package-name (jdee-parse-get-package-name))
	    (class-name (file-name-sans-extension
			 (file-name-nondirectory (buffer-file-name)))))
	(if (and (not no-package-p) package-name)
	    (setq class-name (concat package-name "." class-name)))
	(when (called-interactively-p 'interactive)
	  (kill-new class-name)
	  (message (format "Copied `%s'" class-name)))
	class-name)
    (error "Not a Java source buffer.")))


(defun jdee-parse-get-buffer-unqualified-class ()
  "Get the class name from the buffer filename"
  (file-name-sans-extension (file-name-nondirectory
			     (or (buffer-file-name) "Object.java"))))


(defun jdee-parse-double-backslashes (name)
  (mapconcat (lambda (x) (if (eq x ?\\)
			     "\\\\"
			   (string x)))
	     name ""))

(defvar jdee-parse-java-symbol-declare-re
  (rx
   (1+              ;; A Java symbol comprises one or more of the following:
    (char (?A . ?Z)   ;;   - upper case characters
	  (?a . ?z)   ;;   - lower case characters
	  (?0 . ?9)   ;;   - digits
	  "[]"        ;;   - square brackets
	  "?"         ;;   - question mark
	  "<,> \t\n\r";;   - java1.5 generic support
	  "_"         ;;   - underscore
	  "."         ;;   - period
	  (160 . 255) ;;   - accented characters
	  )))
"Regular expression that matches any Java symbol declare.")

(defun jdee-parse-valid-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (let ((case-fold-search nil)) ;; Why case-insensitive?
      (if (or
	   (looking-at (concat "\\(" jdee-parse-java-symbol-declare-re "\\)[ \t\n\r]+"
			      (jdee-parse-double-backslashes varname)
			      "[]?[ \t\n\r]*[),;=]"))

	   ;; Handle case where varname is part of a list declaration, e.g.,
	   ;;
	   ;;   String a, b, c;
	   ;;
	   (looking-at (concat "\\(" jdee-parse-java-symbol-declare-re "\\)[ \t\n\r]+"
			       "\\(" jdee-parse-java-symbol-re "[ \t\n\r]*,[ \t\n\r]*\\)*"
			      (jdee-parse-double-backslashes varname)
			      "[]?[ \t\n\r]*[,;]"))
	   ;; Parse jdk1.5 for (Type val : collection) {
	   (looking-at (concat "\\(" jdee-parse-java-symbol-declare-re "\\)[ \t\n\r]+"
			       varname "[ \t\n\r]*:")))  ;[ \t\n\r]*"
			       ;"\\(" jdee-parse-java-symbol-re "[,{} \t\n\r]*\\)+" ")")))
	  (let ((type (match-string 1))
		(type-pos (match-beginning 1)))
	    (goto-char type-pos)
	    ;;  Check for following case.
	    ;;     Object table
	    ;;    //representing objects after all updates.
	    ;;    table = new Truc();
	    ;;    table.
	    ;;  Avoid false hit on updates.
	    (if (not (or
		      (jdee-parse-comment-or-quoted-p)
		      (string= type "instanceof")
		      (string= type "return")))
		type))
	nil))))

(defun jdee-parse-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a cons of two strings.  The first is a string containing
the name of the type, or nil if it cannot be found. The second is a string
containing any qualifying text that precedes the class name, which is nil
if no text that looks like an identifier precedes the type name.  This
function does not give the fully-qualified java class name, it just returns
the type as it is declared, and a qualifier that might be the package or
the containing/outer class of the declared type."
  (save-excursion
    (let (found res foundpt qualifier)
      (setq foundpt (jdee-parse-find-declaration-of name))
      (setq found (not (null foundpt)))

      ;; now check for qualifying identifier. Note: reuses
      ;; jdee-parse-qualified-name-at-point for simplicity, not efficiency
      (if found
	  (let (qualname)
	    (goto-char foundpt)
	    (setq qualname (jdee-parse-qualified-name-at-point))
	    (setq qualifier (car qualname))
	    (setq res (cdr qualname))))

      (cons res qualifier))))

(defun jdee-parse-find-declaration-of (name)
  "Find in the current buffer the location where NAME is declared.
Returns the character position in the buffer, or nil if no declaration
could be found."
  (save-excursion
    (let ((symbol-list-entry-re
	   (concat jdee-parse-java-symbol-re "[ \t\n\r]*,[ \t\n\r]*"))
	  (orgpt (point))
	   found pos resname foundpt lastpos)

      ;; Search backward in the buffer.
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
    (setq lastpos (point))

	;; Position point at the start of the type
	;; symbol in the declaration, e.g.,
	;;
	;;  String s;
	;;  ^
	(backward-word 1)

	;; Handle case where declaration declares
	;; a list of symbols, e.g.,
	;;
	;;  String a, b, c;
	;;
	;; In this case, back over any entries in
	;; the list ahead of name.
	(while (looking-at symbol-list-entry-re)
      (setq lastpos (point))
	  (backward-word 1))
    ;;  List<String, List<String>> a;
    ;;                    ^
    ;; In this case, back over any entries between < and >
    (let ((try-count 0)
	  (max-try-count 20))
      (while (and
	      (< try-count max-try-count)
	      (not
	       (= (cl-count ?< (buffer-substring (point) lastpos))
		  (cl-count ?> (buffer-substring (point) lastpos)))))
	(setq try-count (1+ try-count))
	(backward-word 1)))


	(setq resname (jdee-parse-valid-declaration-at (point) name))
	(setq foundpt (point))
	(goto-char pos)
	(forward-char -1)
	(if (and resname
		 (not (jdee-parse-keywordp resname)))
	    (setq found t)))

      (unless found

	;; Not found backward in buffer. Try looking forward.
	(goto-char orgpt)

	(while (and (not found)
		    (search-forward name nil t))
	  (setq pos (point))
      (setq lastpos (point))
	  (backward-word 2)

	  (while (looking-at symbol-list-entry-re)
	(setq lastpos (point))
	    (backward-word 1))

    ;;  List<String, List<String>> a;
    ;;                    ^
    ;; In this case, back over any entries between < and >
      (let ((try-count 0)
	    (max-try-count 20))
	(while (and
		(< try-count max-try-count)
		(not
		 (= (cl-count ?< (buffer-substring (point) lastpos))
		    (cl-count ?> (buffer-substring (point) lastpos)))))
	  (setq try-count (1+ try-count))
	  (backward-word 1)))

	  (setq resname (jdee-parse-valid-declaration-at (point) name))
	  (setq foundpt (point))
	  (goto-char pos)
	  (forward-char 1)
	  (if (and resname
		   (not (jdee-parse-keywordp resname)))
	      (setq found t))))

      (if found foundpt nil))))


(defun jdee-display-parse-error (error)
  (let* ((parser-buffer-name "*Java Parser*")
	 (buf (get-buffer parser-buffer-name)))
    (if (not buf)
	(setq buf (get-buffer-create parser-buffer-name)))
    (set-buffer buf)
    (erase-buffer)
    (insert error)
    (pop-to-buffer buf)))

(defun jdee-parse ()
  "*Parses the Java source file displayed in the current buffer.
If the source file parses successfully, this command displays
a success message in the minibuffer. Otherwise, it displays an error
message in the Java Parser buffer. If the Java Parser buffer does
not exist, this command creates it.

Note. This command uses an external Java parser implemented in
Java to parse Java source files. This command uses the JDE's integrated
Java source interpreter, the BeanShell, to invoke the parser. If the
BeanShell is not running, this command starts the BeanShell. Thus,
the first time you invoke the parser you may notice a slight delay
before getting a response. Thereafter, the response should be very
fast."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (let ((parse-error
	 (jdee-jeval-r (concat "jde.parser.ParserMain.parseFile(\"" (buffer-file-name) "\");"))))
    (if parse-error
	(jdee-display-parse-error parse-error)
      (message "Parsed %s successfully" (buffer-name)))))

(defun jdee-parse-comment-or-quoted-p ()
  "Returns t if point is in a comment or a quoted string,
otherwise nil."
  (let* ((state (save-excursion
		  (parse-partial-sexp (point-min) (point)))))
    (if (or (nth 3 state)
	    (nth 4 state))
	t)))

(defun jdee-parse--search-class (class pos)
  ;; Define an internal function that recursively searches a class
  ;; and its subclasses for a method containing point.
  (let* ((class-name       (semantic-tag-name class))
	 (class-parts      (semantic-tag-type-members class))
	 (class-subclasses (semantic-brute-find-tag-by-class 'type class-parts))
	 (class-methods    (semantic-brute-find-tag-by-class 'function class-parts)))

    ;; Is point in a method of a subclass of this class?
    (loop for subclass in class-subclasses do
	  (jdee-parse--search-class subclass pos))

    ;; Is point in any of the methods of this class?
    (loop for method in class-methods do
	  (let* ((method-name  (semantic-tag-name method))
		 (method-start (semantic-tag-start method))
		 (method-end   (semantic-tag-end method)))
	    (when (and (>= pos method-start)
		       (<= pos method-end))
	      (throw 'found (cons (cons class-name method-name)
				  (cons method-start method-end))))))))

(defun jdee-parse-get-method-at-point (&optional position)
  "Gets the method at POSITION, if specified, otherwise at point.
Returns (CLASS_NAME . METHOD_NAME) if the specified position is
in a method; otherwise, nil."
  (let* ((pos (if position position (point)))
	 (tokens (semantic-fetch-tags))
	 (classes (semantic-brute-find-tag-by-class 'type tokens)))
    (catch 'found
      (loop for class in classes
	    do (jdee-parse--search-class class pos)))))

(defclass jdee-avl-tree ()
  ((tree        :initarg tree
		:type sequence
		:documentation
		"The tree")
   (compare-fcn :initarg compare-fcn
		:type function
		;; :initform <
		:documentation
		"Compare function."))
  "Balanced binary tree.")

(defmethod initialize-instance ((this jdee-avl-tree) &rest fields)
  "Constructor for binary balanced tree."

  ;; Call parent initializer
  (call-next-method)

  (assert (typep  (oref this compare-fcn)  'function))

  (oset this  tree (avl-tree-create (oref this compare-fcn))))

(defmethod jdee-avl-tree-add ((this jdee-avl-tree) item)
  "Inserts ITEM in this tree."
  (avl-tree-enter (oref this tree) item))

(defmethod jdee-avl-tree-delete ((this jdee-avl-tree) item)
  "Deletes ITEM from THIS tree."
  (avl-tree-delete (oref this tree) item))

(defmethod jdee-avl-tree-is-empty ((this jdee-avl-tree))
  "Return t if THIS tree is empty, otherwise return nil."
  (avl-tree-empty (oref this tree)))

(defmethod jdee-avl-tree-find ((this jdee-avl-tree) item)
  "Return the element in THIS tree that matches item."
  (avl-tree-member (oref this tree) item))

(defmethod jdee-avl-tree-map ((this jdee-avl-tree) map-function)
  "Applies MAP-FUNCTION to all elements of THIS tree."
  (avl-tree-map map-function (oref this tree)))

(defmethod jdee-avl-tree-first ((this jdee-avl-tree))
  "Return the first item in THIS tree."
  (avl-tree-first (oref this tree)))

(defmethod jdee-avl-tree-last ((this jdee-avl-tree))
  "Return the last item in THIS tree."
  (avl-tree-last (oref this tree)))

(defmethod jdee-avl-tree-copy ((this jdee-avl-tree))
  "Return a copy of THIS tree."
  (avl-tree-copy (oref this tree)))

(defmethod jdee-avl-tree-flatten ((this jdee-avl-tree))
  "Return a sorted list containing all elements of THIS tree."
  (avl-tree-flatten (oref this tree)))

(defmethod jdee-avl-tree-size ((this jdee-avl-tree))
  "Return the number of elements in THIS tree."
  (avl-tree-size (oref this tree)))

(defmethod jdee-avl-tree-clear ((this jdee-avl-tree))
  "Delete all elements of THIS tree."
  (avl-tree-clear (oref this tree)))

(defclass jdee-parse-method-map (jdee-avl-tree)
  ()
  "Map of the methods in the current buffer.")


(defun jdee-parse-method-map-compare-fcn (m1 m2)
  (and
   (< (car (cdr m1)) (car (cdr m2)))
   (< (cdr (cdr m1)) (car (cdr m2)))))

(defun jdee-parse--add-methods (method-map class)
  (let* ((class-name       (semantic-tag-name class))
	 (class-parts      (semantic-tag-type-members class))
	 (class-subclasses (semantic-brute-find-tag-by-class 'type class-parts))
	 (class-methods    (semantic-brute-find-tag-by-class 'function class-parts)))

    ;; Add methods of subclasses
    (loop for subclass in class-subclasses do
	  (jdee-parse--add-methods method-map subclass))

    ;; Add methods of this class?
    (loop for method in class-methods do
	  (let* ((method-name  (semantic-tag-name method))
		 (method-start (semantic-tag-start method))
		 (method-end   (semantic-tag-end method)))
	    (jdee-avl-tree-add
	     method-map
	     (cons
	      (cons class-name method-name)
	      (cons method-start method-end)))))))

(defmethod initialize-instance ((this jdee-parse-method-map) &rest fields)
  "Constructor for method map."

  (oset  this compare-fcn 'jdee-parse-method-map-compare-fcn)

  ;; Call parent initializer.
  (call-next-method)

  (let* ((tokens (semantic-fetch-tags))
	 (classes (semantic-brute-find-tag-by-class 'type tokens)))
    (loop for class in classes do
	  (jdee-parse--add-methods this class))))

(defmethod jdee-parse-method-map-get-method-at ((this jdee-parse-method-map) &optional pos)
  "Get the method at POS, if specified, otherwise, at point."
  (let ((p (if pos pos (point))))
    (jdee-avl-tree-find this (cons (cons "" "") (cons p p)))))

(defvar jdee-parse-the-method-map nil
  "Map of methods defined in this buffer sorted by location.")
(make-variable-buffer-local 'jdee-parse-the-method-map)


(defun jdee-current-buffer-exact-name-match-p (tag)
  (and (tag-exact-match-p tag)
       (equal (buffer-file-name (window-buffer (selected-window)))
	      (file-of-tag))))

(defun jdee-etags-recognize-tags-table () ; see etags-recognize-tags-table
  (let ((recognized (etags-recognize-tags-table)))
    (if recognized
	;; prefer exact match in current buffer to other files
	(setq find-tag-tag-order '(jdee-current-buffer-exact-name-match-p
				   tag-exact-file-name-match-p
				   tag-exact-match-p
				   ))
      recognized)))

(defun jdee-parse-java-variable-at-point ()
  "Returns a list (VAR PARTIAL) where VAR.PARTIAL is the partially completed
method or field name at point. For example, suppose obj.f1.ge were the name
at point. This function would return the list (\"obj.f1\" \"ge\")."
  (save-excursion
    (let (start middle-point varname curcar dot (dot-offset 0) found
		(original-point (point))
		intermediate-point beg-point first-part second-part
		first-paren cast-type second-paren
		args (offset 0) (bracket-count 0) (paren-count 0))

      ;; Move cursor to the beginning of the partially
      ;; completed part of the expression, e.g., move point
      ;; from
      ;;
      ;;   obj.f1.ge   to obj.f1.ge
      ;;            ^           ^
      ;;   obj.f1.get(int,int)  to obj.f1.get(int,int)
      ;;                      ^              ^
      (setq curcar (char-before))
      (if curcar
	  (progn
	    (while (null found)
	      (cond
	       ((or (and (>= curcar ?a) (<= curcar ?z)) ; a-z
		    (and (>= curcar ?A) (<= curcar ?Z)) ; A-z
		    (and (>= curcar ?0) (<= curcar ?9))
		    (>= curcar 127)
		    (member curcar '(?$ ?_ ?\\))) ;; _ \
		(forward-char -1))
	       ((eq ?. curcar)
		(setq dot-offset 1)
		(if (eq ?\) (char-before (- (point) 1)))
		    (progn
		      (forward-char -2)
		      (setq first-paren (point))
		      (setq second-paren (jdee-parse-match-paren-position))
		      (setq offset (+ (- first-paren second-paren) 1))
		      (forward-char 2)
		      (setq found (point)))
		  (setq found (point))))
	       (t
		(setq found (point))))
	      (setq curcar (char-before)))
	    (setq intermediate-point found)
	   ;; Now move point to the beginning of the expression, e.g.,
	    ;; from
	    ;;
	    ;;  obj.f1.ge
	    ;;        ^
	    ;; to
	    ;;
	    ;;  obj.f1.ge
	    ;; ^
	    ;;
	    (progn
	      (setq curcar (char-before))
	      (while (or (and (>= curcar ?a) (<= curcar ?z))
			 (and (>= curcar ?A) (<= curcar ?Z))
			 (and (>= curcar ?0) (<= curcar ?9))
			 (>= curcar 127)
			 (and (eq curcar ? ) (or (< 0 paren-count)
						 (< 0 bracket-count)))
			 (and (member curcar '(?$ ?\" ?\. ?\_ ?\\ ?\( ?\) ?\, ?\[ ?\]))
			      (if (eq curcar ?\[)
				  (> bracket-count 0)
				t)))
		(cond
		 ((eq curcar ?\))
		  (progn
		    (forward-char -1)
		    (goto-char (jdee-parse-match-paren-position))))
		 ((eq curcar ?\( )
		  (setq paren-count (1- paren-count)))
		 ((eq curcar ?\] )
		  (setq bracket-count (1+ bracket-count)))
		 ((eq curcar ?\[ )
		  (setq bracket-count (1- bracket-count))))
		(forward-char -1)
		(setq curcar (char-before)))

	      (setq beg-point (point))
	      (set-marker jdee-parse-current-beginning intermediate-point)
	      (set-marker jdee-parse-current-end original-point)
	      (setq middle-point (- intermediate-point dot-offset offset))
	      (setq first-part
		    (buffer-substring-no-properties beg-point middle-point))
	      (setq first-part (jdee-parse-isolate-to-parse first-part))

	      ;;replacing newline by empty strings new lines seems to break the
	      ;;beanshell
	      (while (string-match "\n" first-part)
		(setq first-part (replace-match "" nil nil first-part)))

	      ;;replacing extra spaces for "". This done to reduce the space
	      ;;that the completion title takes
	      (while (string-match " " first-part)
		(setq first-part (replace-match "" nil nil first-part)))

	      (setq second-part
		    (buffer-substring-no-properties
		     intermediate-point original-point))

	      ;;Checking for casting
	      ;; ((Object) obj).ge
	      ;; FIXME can't work ok for generic type, eg: ((List<String>) obj).ge
	      (if (and (not cast-type)
		       (string= first-part "")
		       (eq (char-before (+ 1 middle-point)) ?\()
		       (eq (char-before (+ 2 middle-point)) ?\())
		  (save-excursion
		    (goto-char (+ middle-point 1))
		    (setq first-paren (point))
		    (setq second-paren (jdee-parse-match-paren-position))
		    (setq cast-type (buffer-substring-no-properties
				     (+ 1 first-paren) second-paren))))

	      (if cast-type
		  (progn
		    (setq jdee-parse-casting t)
		    (list cast-type second-part))
		(progn
		  (setq jdee-parse-casting nil)
		  (list first-part second-part))))
	    )))))

(defun jdee-parse-isolate-to-parse (s)
  "Returns the right expression that needs completion in S."
  (let* ((index (length s)) stop (paren 0) curcar final-string
	 (inside-quotes nil))
    (while (and (> index 0)
		(not stop))
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar);;Checking if we are inside double quotes
	(if (not (eq ?\\ (aref s (- index 1))));;if the quote is not escape
	    (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
	(if (not inside-quotes)
	    (setq paren (1+ paren))))
       ((eq ?\( curcar)
	(if (not inside-quotes)
	    (setq paren (1- paren)))))
      (if (or (< paren 0)
	      (and (eq curcar ?\,) (<= paren 0)))
	  (setq stop t)))
    (if stop
	(setq index (1+ index)))
    (setq final-string (substring s index))
    (if (and (not (string= final-string ""))
	     (string= "(" (substring final-string 0 1)))
	(let* ((closing-paren (string-match ")" final-string))
	       (closing (string-match ")." final-string (+ 1 closing-paren))))
	  (setq final-string
		(concat (substring final-string 2 closing-paren)
			"."
			(substring final-string (+ closing 2))))))
    final-string))
(defun jdee-parse-match-paren-position ()
  "Returns the position of the parenthesis match"
  (let ((current (point))
	match)
    (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	  ((looking-at "\\s\)") (forward-char 1) (backward-list 1)))
    (setq match (point))
    (goto-char current)
    match))

(defun jdee-parse-eval-type-of (expr)
  "Get type of EXPR. This function returns a class name or builtin
Java type name, e.g., int."
  (if expr
      (let ((class-at-point (jdee-parse-get-class-at-point))
	    (super-at-point (jdee-parse-get-super-class-at-point))
	    qualified-name chop-pos temp answer)
	(setq answer
	      (cond
	       ;;If it's number returns an int
	       ((integerp expr)
		"int")
	       ;;If it's 001L or 134l return long
	       ((if (and (integerp (substring expr 0 (- (length expr) 1)))
			 (or (string= "L" (substring expr (- (length expr) 1)
						     (length expr)))
			     (string= "l" (substring expr (- (length expr) 1)
						     (length expr)))))
		    "long"))
	       ;;If it's a floating point return float
	       ((floatp expr)
		"double")
	       ;;If it's 000F or 1234f return float
	       ((if (and (floatp (substring expr 0 (- (length expr) 1)))
			 (or (string= "F" (substring expr (- (length expr) 1)
						     (length expr)))
			     (string= "f" (substring expr (- (length expr) 1)
						     (length expr)))))
		    "float"))
	       ((string-match "false\\|true" expr)
		"boolean")
	       ;;Checking if it's a character
	       ((string= "'" (substring expr 0 1))
		"char")
	       ;; If it's "this", we return the class name of the class we code in
	       ((and class-at-point (string= "this" expr))
		(jdee-parse-get-qualified-name class-at-point t))
	       ;; If it's "super", we return the super class
	       ;;name of the class we code in
	       ((and super-at-point (string= "super" expr))
		(jdee-parse-get-qualified-name super-at-point t))
	       ;;if it's a class name, done
	       ((setq qualified-name (jdee-parse-get-qualified-name expr t))
		qualified-name)

	       ;;check if it's an inner class
	       ((and expr
		     class-at-point
		     (string= "this.this" expr)
		     (setq qualified-name (jdee-parse-get-inner-class
					   class-at-point)))
		qualified-name)

	       (t
		(let (to-complete
		      (last-char
		       (aref expr (- (length expr) 1 ))))
		  ;; If it ends with a parenthesis
		  (cond
		   ((eq last-char ?\))
		    (let* ((result
			    (jdee-parse-isolate-before-matching-of-last-car
			     expr))
			   (temp (if (not (string= "this.this" result))
				     (jdee-parse-split-by-dots result)))
			   to-complete)
		      (if temp
			  (jdee-parse-find-completion-for-pair temp)
			;;we need exact completion here
			(jdee-parse-find-completion-for-pair
			 (list "this" result) nil jdee-complete-private))

		      ;;if the previous did not work try only result
		      (if (not jdee-complete-current-list)
			  (jdee-parse-find-completion-for-pair (list result "")))

		      ;;if the previous did not work try again
		      (setq qualified-name
			    (jdee-parse-get-qualified-name result t))
		      (if qualified-name
			  qualified-name
			(if jdee-complete-current-list
			    (progn
			      (setq to-complete
				    (car (car jdee-complete-current-list)))
			      (setq chop-pos (+ 3 (string-match " : " to-complete)))
			      (let* ((space-pos (string-match " " to-complete chop-pos))
				     (uqname (if space-pos (substring to-complete chop-pos space-pos)
					       (substring to-complete chop-pos))))
				uqname))))))

		   ;;if it's an array
		   ((eq last-char ?\])
		    (let ((temp (jdee-parse-eval-type-of
				 (jdee-parse-isolate-before-matching-of-last-car
				  expr))))
		      (jdee-parse-get-component-type-of-array-class temp)))

		   ;;we look for atoms if expr is splittable by dots
		   ((setq temp (if (not (string= "this.this" expr))
				   (jdee-parse-split-by-dots expr)))
		    ;;we need exact completion here
		    (jdee-parse-find-completion-for-pair temp t)
		    (if jdee-complete-current-list
			(progn
			  (setq to-complete (car (car
						  jdee-complete-current-list)))
			  (setq chop-pos (+ 3 (string-match " : " to-complete)))
			  (let* ((space-pos (string-match " " to-complete chop-pos))
				 (uqname (if space-pos (substring to-complete chop-pos space-pos)
					   (substring to-complete chop-pos))))
			    (jdee-parse-get-qualified-name uqname)))

		      nil))
		   (t
		    ;; See if it's declared somewhere in this buffer.
		    (let (parsed-type result result-qualifier)
		      (setq parsed-type (jdee-parse-declared-type-of expr))
		      (setq result (car parsed-type))
		      (setq result-qualifier (cdr parsed-type))

		      (if result
			  (let ((count 0) type)
			    (while (string-match ".*\\[\\]" result)
			      (setq result (substring result 0
						      (- (length result) 2)))
			      (setq count (1+ count)))

			    (let (work)
			      (setq type
				    (cond
				  ;; handle primitive types, e.g., int
				     ((member result jdee-parse-primitive-types)
				      result)
			     ;; quickly make sure fully qualified name
				     ;;doesn't exist
				     ((and result-qualifier
					   (jdee-parse-class-exists
					    (setq work (concat result-qualifier
							       "."
							       result))))
				      work)
				     ;; then check for inner classes
				     ((setq work
					    (jdee-parse-get-inner-class-name
					     result result-qualifier))
				      work)
			       ;; otherwise use unqualified class name
				     (t
				      (jdee-parse-get-qualified-name result
								    t)))))

			    (if type
				(progn
				  (while (> count 0)
				    (setq type (concat type "[]"))
				    (setq count (1- count)))
				  (jdee-parse-transform-array-classes-names
				   type))
			      (if (y-or-n-p
				   (format (concat "Could not find type of %s"
						   " Attempt to import %s? ")
					   expr result))
				  (progn
				    ;; import
				    (jdee-import-find-and-import result)
				    ;; recursive call of eval-type-of
				    (jdee-parse-eval-type-of expr))
				(error "Could not find type of %s" result))))
			(if (and jdee-parse-casting
				 (null jdee-parse-attempted-to-import)
				 (y-or-n-p
				  (format (concat "Could not find type of %s"
						  " Attempt to import %s? ")
					  expr expr)))
			    (progn
			      (setq jdee-parse-attempted-to-import t)
			      (setq jdee-parse-casting nil)
			      (jdee-import-find-and-import expr)
			      (jdee-parse-eval-type-of expr))
			  (progn
			    (setq jdee-parse-attempted-to-import nil)
			    nil))))))))))
	answer)))

(defun jdee-parse-convert-args-to-types (args)
  "Converts something like this (10, 10) to (int, int)"
  (let* ((answer "(")
	 (first-time t)
	 (temp (substring args 1 (- (length args) 1)));;Striping the parens
	 (lst (split-string temp ", ?"))
	 tmp)
    (while lst
      (setq tmp (car lst))
      (if (not first-time)
	  (setq answer (concat answer ", "))
	(setq first-time nil))
      (setq answer (concat answer
			   (jdee-parse-eval-type-of tmp)))
      (setq lst (cdr lst)))
    (setq answer (concat answer ")"))
    answer))

(defun jdee-parse-transform-array-classes-names (name)
  (let (result)
    (while (string-match ".*\\[\\]" name)
      (setq name (substring name 0 (- (length name) 2 )))
      (setq result (concat "[" result)))
    (if result
	(progn
	  (cond
	   ((string= name "byte")
	    (setq result (concat result "B")))
	   ((string= name "char")
	    (setq result (concat result "C")))
	   ((string= name "double")
	    (setq result (concat result "D")))
	   ((string= name "float")
	    (setq result (concat result "F")))
	   ((string= name "int")
	    (setq result (concat result "I")))
	   ((string= name "long")
	    (setq result (concat result "J")))
	   ((string= name "short")
	    (setq result (concat result "S")))
	   ((string= name "boolean")
	    (setq result (concat result "Z")))
	   (t
	    (setq result (concat result "L" name ";"))))
	  result)
      name)))

(defun jdee-parse-get-component-type-of-array-class (name)
  (if (string= "[" (substring name 0 1))
      (let (result)
	(setq result
	      (jdee-jeval
	       (concat "System.out.println( Class.forName(\""
		       name
		       "\").getComponentType().getName()) ;"))) ;;removed \n
	(substring result 0 (- (length result) 1)))
    name))

;; Modified `jdee-parse-import-list' to use semantic parser table
(defun jdee-split-import-token (token)
  "Helper function used by `jdee-parse-import-list' which return a
list (PACKAGE-DOT CLASS-OR-STAR) from given semantic 'include (that
is Java import) TOKEN.
For example:
  : (jdee-split-import-token \"java.util.Hashtable\")
  > (\"java.util.\" . \"Hashtable\")
  : (jdee-split-import-token \"java.lang.*\")
  > (\"java.lang.\" . \"*\")
  : (jdee-split-import-token \"test\")
  > (\"test.\" . \"*\")"
  (let* ((import      (semantic-tag-name token))
	 (match-point (string-match "\\." import))
	 split-point)
    (while match-point
      (setq split-point (1+ match-point)
	    match-point (string-match "\\." import split-point)))
    (if split-point
	(list (substring import 0 split-point)
	      (substring import split-point))
      (list (concat import ".")
	    "*"))))

(defun jdee-parse-import-list ()
  "Return the list of Java packages declared in the current buffer.
It uses the semantic parser table to find the 'package' and 'import'
statements. It implicitly adds the java.lang.* package. See also
`jdee-split-import-token'."
  (let* ((tokens   (semantic-fetch-tags))
	 (packages (semantic-brute-find-tag-by-class 'package tokens))
	 (imports  (semantic-brute-find-tag-by-class 'include tokens))
	 lst)
    (setq lst (append
	       (mapcar (function
			(lambda (token)
			  (list
			   (concat (semantic-tag-name token) ".")
			   "*")))
		       packages)
	      (mapcar 'jdee-split-import-token
		       imports)))
    (or (member "java.lang.*" lst)
	(setq lst (append lst '(("java.lang." "*")))))
    lst))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
;; Get the fully qualified name of the class NAME, using the import
;; list. It returns a string if the fqn was found, or null otherwise.
;; This is more capable than jdee-complete-guess-type-of because it
;; uses the beanshell to determine if an import statement with a
;; wildcard contains the unqualified class name passed to this
;; function.
(defun jdee-parse-get-qualified-name (name &optional import)
  "Guess the fully qualified name of the class NAME, using the import list. It
returns a string if the fqn was found, or null otherwise. If IMPORT is non-nil
try importing the class"
  (if (jdee-parse-class-exists name)
      name
    (let ((importlist (jdee-parse-import-list))
	  shortname fullname tmp result)
      (while (and importlist (null result))
	(setq tmp (car importlist))
	(setq shortname (car (cdr tmp)))
	(setq fullname (concat (car tmp) name))
	(cond
	 ((and (string= "*" shortname) (jdee-parse-class-exists fullname))
	  (setq result fullname))
	 ((string= name shortname)
	  (setq result fullname))
	 (t
	  (setq importlist (cdr importlist)))))
      (if (and (null result) import)
	  (progn
	    (jdee-import-find-and-import name t)
	    (setq result (jdee-parse-get-qualified-name name))))
      result)))

;; Contributed by Charles Hart <cfhart@Z-TEL.com>
(defun jdee-parse-class-exists (name)
  "Returns t if the fully qualified class name can be found in the
  classpath, nil otherwise."
  (if (stringp name)
      (progn
	;; Replace double quotes by empty strings.
	;; Double quotes seems to break the beanshell.
	(while (string-match "\"" name)
	  (setq name (replace-match "" nil nil name)))

	;; Replace back slashes by empty strings.\
	;; Back slashes causes Beeanshell problems
	(while (string-match "\\\\" name)
	  (setq name (replace-match "" nil nil name)))

	(jdee-jeval-r (concat "jde.util.JdeUtilities.classExists(\"" name "\");")))))

(defun jdee-parse-get-inner-class (expr)
  "Takes a single argument like B.A and split it up in a name
and qualifer. The name and the qualifier are then use as arguments
to the function `jdee-parse-get-inner-class-name'"
  (let (name qualifier (pos (string-match "\\." expr)))
    (if pos
	(progn
	  (setq name (substring expr (+ 1 pos)))
	  (setq qualifier (substring expr 0 pos))))
    (jdee-parse-get-inner-class-name name qualifier)))

;; Tries to divine inner class via the following algorithm:
;; if no qualifier (e.g. A), then try
;;   <this-class>$A
;; if qualifier (e.g. D.C.B.A), then try the following in order:
;;   <this-class>$D$C$B$A
;;   <super-types>$D$C$B$A (NOT YET IMPLEMENTED)
;;   D$C$B$A           i.e  D$C$B$A in default, current, or imported package
;;   D + C$B$A         i.e  D in imports or local/default package with
;;                          a C$B$A inner class
;;   D.C.B$A           i.e. B$A in package D.C
;;   D.C$B$A           i.e. C$B$A in package D
(defun jdee-parse-get-inner-class-name (name qualifier)
  "Tries to find an inner class with the class name of name.  If qualifier
is nil, the inner class is only searched on current class at point.  If
qualifier is not nil, a wider array of possible inner classes will
searched.  Returns nil if not found; a fully qualified inner class name
otherwise."
  (let (class-name this-full-class-name result)
    (setq this-full-class-name (jdee-parse-get-qualified-name
				(jdee-parse-get-class-at-point)))
    (cond
     (qualifier
      (let ((work (concat qualifier "$" name)))
	(setq work (subst-char-in-string ?. ?\$ work))

	;; check against this$D$C$B$A
	(setq class-name (concat this-full-class-name "$" work))
	(if (not (jdee-parse-class-exists class-name))
	    (let (dot-count index first-part remaining-part)
	      (setq class-name nil)

	      ;; check against D$C$B$A (default and imported)
	      (print "Zero dots" (get-buffer "*scratch*"))
	      (setq class-name (jdee-parse-get-qualified-name work))

	      ;; check remaining semi-qualified variants
	      (setq dot-count 1)
	      (while (and (null class-name)
			  (setq index (string-match "\\$" work)))

		(setq first-part (substring work 0 index))
		(setq remaining-part (if (< index (length work))
					 (substring work (+ 1 index))
				       nil))
		(cond
		 ;; check against C$B$A on class D
		 ((= 1 dot-count)
		  (setq class-name (jdee-parse-get-qualified-name
				    first-part))
		  (if (not (null class-name))
		      (progn
			(setq class-name (concat class-name
						 "$"
						 remaining-part))
			(if (null (jdee-parse-class-exists class-name))
			    (setq class-name nil)))))

		 ;; just check if it exists (ignoring imports)
		 (t
		  (setq class-name (concat first-part "." remaining-part))
		  (if (null (jdee-parse-class-exists class-name))
		      ;; lastly check if it exists in this package
		      (progn
			(setq class-name (concat (jdee-parse-get-package-name)
						 "."
						 work))
			(if (not (jdee-parse-class-exists class-name))
			    (setq class-name nil))))))

		(setq work (concat first-part "." remaining-part))
		(setq dot-count (1+ dot-count)))

	      class-name))))

     ;=; otherwise, no qualifier...check against this$D
     ((jdee-parse-class-exists (setq class-name
				    (concat this-full-class-name "$" name)))
      class-name))))

;; Can this function be reimplemented to use regular expressions?
(defun jdee-parse-isolate-before-matching-of-last-car (s)
  "Returns the right expression that needs completion in S."
  (let* (final-string (index (length s)) stop (paren 0) (bracket 0) curcar
		      (inside-quotes nil))
    (while (and (> index 0)
		(not stop))
      (setq index (- index 1))
      (setq curcar (aref s index))
      (cond
       ((eq ?\" curcar);;Checking if we are inside double quotes
	(if (not (eq ?\\ (aref s (- index 1))));;if the quote is not escape
	    (setq inside-quotes (not inside-quotes))))
       ((eq ?\) curcar)
	(if (not inside-quotes)
	    (setq paren (1+ paren))))
       ((eq ?\( curcar)
	(if (not inside-quotes)
	    (setq paren (1- paren))))
       ((eq ?\] curcar)
	(if (not inside-quotes)
	    (setq bracket (1+ bracket))))
       ((eq ?\[ curcar)
	(if (not inside-quotes)
	    (setq bracket (1- bracket)))))
      (if (and (= paren 0)
	       (= bracket 0))
	  (setq stop t)))
    (setq final-string (substring s 0 index))
    (if (and (string= "" final-string)
	     (string= "((" (substring s 0 2)))
	(let ((closing-paren (string-match ")" s)))
	  (setq final-string (substring s 2 closing-paren))))
    final-string))

(defun jdee-parse-split-by-dots (s)
  "Return a list containing the longest substring of S that ends with a dot,
 and the rest.But removes the intermediate(=last) dot."
  ;;we now isolate the last atom after a dot and the beginning
  (if (string-match "\\(.*\\)\\.\\(.*\\)" s)
      (list (match-string 1 s) (match-string 2 s))
    nil))

(defun jdee-parse-find-completion-for-pair (pair &optional exact-completion
						access-level)
  (jdee-complete-find-completion-for-pair pair exact-completion access-level))

(defun jdee-parse-keywordp (variable)
  "Checks if VARIABLE is a Java keyword, for performance reasons this only checks if the VARIABLE is an if or an else, since those are the ones that seem to be causing a problem"
  (or (string= variable "if")
      (string= variable "else")))

(defun jdee-parse-class-name (classname)
  "Parse CLASSNAME into it's package and non-fully qualified name.

If CLASSNAME is the symbol `point', then parse it from what is
at the point.

Return a list in the form:
  \(FULLY-QUALIFIED-CLASSNAME PACKAGE CLASS-NAME)
or `nil' if the passed class name doesn't look like a class (by the Sun Java
codeing standard).

The first two elements of the list are `nil' if CLASSNAME isn't fully qualifed."
  (cl-flet* ((is-first-cap
	      (str)
	      (unless (= 0 (length str))
		(let ((char (substring str 0 1)))
		  (string= char (upcase char)))))
	     (is-all-cap
	      (str)
	      (string= str (upcase str)))
	     (parse-at-point
	      (classname)
	      (let ((nopkgclass classname)
		    end-pos fq pkg)
		(when (> (length classname) 0)
		  (setq end-pos (cl-position ?. classname :from-end t))
		  (if end-pos
		      (setq fq classname
			    pkg (substring fq 0 end-pos)
			    classname (substring fq (1+ end-pos))))
		  (when (and (> (length nopkgclass) 0)
			     (is-first-cap classname))
		    ;; either a class can be all caps with a package behind it
		    ;; (i.e. com.google.gwt.core.client.GWT) or it can be class
		    ;; with a constant in front of it (i.e. Color.WHITE), currently
		    ;; support the latter instead of kludging this now
		    ;; --PL 2010-06-30
		    (if (and (> (length classname) 1)
			     (is-all-cap classname))
			;; looks like <class>.<sym> (i.e. Color.WHITE)
			nil
		      (list fq pkg classname)))))))
    (if (eq classname 'point)
	;; TODO: a fully qualified class name looks like a file name
	;; (i.e. [a-zA-Z.])  but this need to be refined to use the Sun Java
	;; standards of class name parsing
	(or (let ((thing-at-point-file-name-chars "-~/[:alnum:]_.${}#%:"))
	      (parse-at-point (thing-at-point 'filename)))
	    (parse-at-point (thing-at-point 'word))
	    (parse-at-point (thing-at-point 'symbol)))
      (parse-at-point classname))))

(provide 'jdee-parse)

;;; jdee-parse.el ends here
