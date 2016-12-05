;; jdee-import.el --- Organize Java imports

;; Copyright (C) 2000, 2001, 2002, 2003, 2004 by David Ponce
;; Copyright (C) 2004 by Philip Lord
;; Copyright (C) 2009 by Paul Landes

;; Authors:     David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; Maintainers: David Ponce <david@dponce.com>
;;              Paul Landes <landes <at> mailc dt net>
;;              Martin Schwamberger <mschw@web.de>
;; Created: 15 Nov 2000
;; Keywords: java, tools

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
;; This library adds commands to the JDE to insert and organize Java import
;; statements.

;;; Code:

(require 'cl-lib)
(require 'efc)
(require 'jdee-backend)
(require 'jdee-parse)
(require 'semantic/fw)
(require 'semantic/find)
(require 'semantic/util)

;; FIXME: refactor
(declare-function jdee-choose-class "jdee-open-source" (classes &optional prompt uq-name confirm-fq-p))
(declare-function jdee-jeval-r "jdee-bsh" (java-statement))

;;;;
;;;; Customization
;;;;

(defcustom jdee-import-excluded-classes
  '(("^bsh\\..*" . nil)
    ("^java\\.lang\\.[^.]*$" . t)
    ("^sun\\..*" . nil)
    ("^com\\.sun\\..*" . nil)
    (jdee-import-current-package-p . t))
  "*Specifies classes that should not be imported into a source file.
\"Exclude test\" should be either a regular expression or a function
whose only argument is the fully qualified class name.
Import commands excludes any classes whose fully qualified name matches
\"Regexp\" and any classes for which the \"Test function\" returns non-nil.
If \"Exclude synonyms\" is non-nil, all classes with the same unqualified name
will also be excluded. This is useful for classes of packages which will be
imported implicitly (i.e. java.lang.* and the package of the importing class).
If more than one fully qualified class names match the unqualified name that you specify,
the command prompts you to select only the classes that are not excluded."
  :group 'jdee-project
  :type '(repeat
	  (cons :tag "Exclude rule"
		(choice :tag "Exclude test"
			(regexp :tag "Regexp")
			(function :tag "Test function"))
		(boolean :tag "Exclude synonyms"))))

;; auto sorting of import statements
(defcustom jdee-import-auto-sort nil
  "*Automatically resort import statements after a `jdee-import-import'.
If non-nil, the JDE automatically resorts the import statements when a new import statement is added using `jdee-import-import' or `jdee-import-find-and-import'."
  :group 'jdee-project
  :type 'boolean
)

(defcustom jdee-import-auto-sort-function 'jdee-import-sort
  "*Function to call to automatically  sort imports statements after a `jdee-import-import'.
Usually `jdee-import-sort' or `jdee-import-organize'.  Enabled if  `jdee-import-auto-sort' is not nil."
  :group 'jdee-project
  :type 'function)

(defcustom jdee-import-reverse-sort-group nil
  "*Non-nil to sort each import group's packages in reverse alphabetic
order.  See command `jdee-import-organize'.  Note: For sorting the
groups, see variable `jdee-import-sorted-groups'."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-import-sorted-groups nil
  "*Non-nil to sort import groups in alphabetic order. Order may
be specified as alphabetic, reverse alphabetical or as implicitly
specified by `jdee-import-group-of-rules'. In the latter case the
order of groups is the same as their appearance in
`jdee-import-group-of-rules'.
See command `jdee-import-organize'. Note: For sorting the packages
within each group, see variable `jdee-import-reverse-sort-group'."
  :group 'jdee-project
  :type '(choice :tag "Order"
		 (const :tag "No sort"                  nil)
		 (const :tag "group-of-rules order"     gor)
		 (const :tag "alphabetic order"         asc)
		 (const :tag "reverse alphabetic order" desc)))

(defcustom jdee-import-group-function 'jdee-import-group-of
  "*Function used to associate an import token to a group.
It receives one argument, the import token and must return a group
name string or nil if the import does not belong to any group.  The
function `jdee-import-group-of' is the default value."
  :group 'jdee-project
  :type 'function)

(defcustom jdee-import-group-of-rules
  '(
    ("^javax?\\.")
    )
  "*Import group definitions used by `jdee-import-group-of'.
Each group definition is a pair (REGEXP . GROUP) where:
- - REGEXP is a regexp that import names of this group must match.
- - GROUP is a group name or the index of the match data returned as
    group name or nil if REGEXP is the group name."
  :group 'jdee-project
  :type '(repeat
	  (cons :tag "Group Rule"
		regexp
		(choice :tag "Group Name"
			(string  :tag "A String")
			(integer :tag "Match data at")
			(const   :tag "The Regexp" nil))))
  :set '(lambda (sym val)
	  ;; delete empty entries!
	  (set-default sym (delete '("") val))))

(defcustom jdee-import-default-group-name nil
  "*Default group name if no group name is found.
If a group name is not found in `jdee-import-group-of-rules' then this
group name is used.  If nil no default group name is used."
  :group 'jdee-project
  :type '(choice (string  :tag "A String")
		 (const :tag "none" nil)))

(defcustom jdee-import-insert-group-names nil
  "*If non-nil `jdee-import-organize' inserts group name before imports.
See also the options `jdee-import-group-of-rules' and
`jdee-import-default-group-name'."
  :group 'jdee-project
  :type 'boolean)

;; (makunbound 'jdee-import-blank-line-between-groups)
(defcustom jdee-import-blank-line-between-groups t
  "*If non-nil `jdee-import-organize' inserts a blank line between groups.
See also the options `jdee-import-group-of-rules' and
`jdee-import-default-group-name'."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-import-auto-collapse-imports nil
  "*If non-nil jde will automatically collapse imports when imports are
inserted."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-import-exclude-inner-imports t
  "Exclude imports for classes that appear to be included as inner-classes or by import some.package.*
This avoids offers for some.package.Outer.Inner when some.package.Outer is already imported.
Which is correct if your code refers to Outer.Inner, rather than just Inner;
in the latter case, supplying the no-exclude argument to `jdee-import-all' will find all the classes."
  :group 'jdee-project
  :type 'boolean)


(defun jdee-import-current-package-p (class)
  "Returns non-nil if the fully qualified classname CLASS belongs to
the same package as the class in the current buffer."
  (let ((pkg (jdee-parse-get-package-name)))
    (if pkg
	(string= pkg (jdee-parse-get-package-from-name class)))))

(defun jdee-import-get-qualified-names (unqualified-class)
  "Return a list containing all qualified name for UNQUALIFIED-CLASS."
  (jdee-backend-get-qualified-name unqualified-class))

(defun jdee-import-get-imports ()
  "Return a list containing all imported classes."
  (let* (imports
	 (tags  (semantic-fetch-tags))
	 (import-tags (semantic-brute-find-tag-by-class 'include tags)))
    (dolist (import-tag import-tags)
      (setq imports
	    (cons
	     (semantic-tag-name import-tag)
	     imports)))
    (nreverse imports)))

(defun jdee-import-get-import (unqualified-class)
  "Get imported name for unqualified name UNQUALIFIED-CLASS.
This name may have the form \"package.*\". Returns nil,
if there is no import statement for UNQUALIFIED-CLASS."
  (let (import
	(imports (jdee-import-get-imports))
	(qualified-names (jdee-import-get-qualified-names unqualified-class)))
    (catch 'found
      (dolist (class qualified-names)
	(if (setq import (jdee-import-already-imports-class class imports))
	    (throw 'found import))))))

(defun jdee-import-get-import-insertion-point ()
  "Determine where to insert an import statement.
If the buffer contains an import statement, return
the beginning of the next line; otherwise, if
the buffer contains a package statement, insert
three empty lines and return the beginning of
the second empty line; otherwise, if the buffer
contains a class definition, return the beginning
of the line before the class definition; otherwise,
return the beginning of the buffer."
  (cl-flet ((insertion-point-after
	     (tag-end)
	     (goto-char tag-end)
	     (if (eolp) (forward-char 1)(forward-line 1)) ;skip comment
	     (point)
	     ))
    (let* ((tags (semantic-fetch-tags)) ;(xx (message "tags = %s" tags))
	   (import-tag (car
			(last (semantic-brute-find-tag-by-class
			       'include tags))))
	   (package-tag (car (semantic-brute-find-tag-by-class
			      'package tags)))
	   (class-tag (car (semantic-brute-find-tag-by-class
			    'type tags)))
	   )
      (save-excursion
	(cond (import-tag
	       (insertion-point-after (semantic-tag-end import-tag)))
	      (package-tag
	       (insertion-point-after (semantic-tag-end package-tag))
	       (insert "\n")		;empty line before new imports
	       (unless (eolp)		;empty line after new imports
		 (save-excursion (insert "\n")))
	       (point))
	      (class-tag
	       (let ((comment-token (semantic-documentation-for-tag
				     class-tag 'lex)))
		 (if comment-token
		     (semantic-lex-token-start comment-token)
		   (semantic-tag-start class-tag))))
	      (t 1)))
      )))

(defun jdee-import-import (class)
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an
import statement for the specified class already exists.
The function does not exclude classes defined by `jdee-import-excluded-classes'."
  (interactive
   "sClass: ")
  (jdee-import-insert-import (list class)))

(defun jdee-import-one-class (class)
  "Insert an import into the buffer if not already there."
  (interactive "s")
  (if (not (jdee-import-already-imports-class class (jdee-import-get-imports)))
      (jdee-import-insert-imports-into-buffer (list class))))

;; Contributed by David Ponce <david_ponce@mail.schneider.fr>
(defun jdee-import-sort (&optional reverse)
  "Sort Java import statements alphabetically. In reverse order if
REVERSE is non-nil.

Usage:
  \\[jdee-import-sort] sort import statements ascending.
  \\[universal-argument] \\[jdee-import-sort] sort descending.

The the current buffer must be in `jdee-mode'. This command uses the
semantic Java parser and requires JDE 2.1.6-beta24 and above."
  (interactive "P")
  (or (eq major-mode 'jdee-mode)
      (error "Invalid major mode found. Must be 'jdee-mode'."))
  (or (and (local-variable-p 'semantic--parse-table (current-buffer))
	   (symbol-value 'semantic--parse-table))
      (error "Semantic Java parser not found."))
  (and (called-interactively-p 'interactive)
       (consp current-prefix-arg)
       (setq reverse t))
  (let* ((tags  (semantic-fetch-tags))
	 (depends (semantic-brute-find-tag-by-class 'include tags)))
    (if depends
	(let* ((first-import-tag (car depends))
	       (last-import-tag  (nth (1- (length depends)) depends))
	       (start (semantic-tag-start first-import-tag))
	       (end   (semantic-tag-end   last-import-tag)))
	  (when (and start end)
	    (require 'sort)
	    (let (sort-fold-case)
	      (sort-lines reverse start end)
	      (goto-char start)))))))

(defun jdee-import-find-and-import (class &optional no-errors no-exclude qualifiedp)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can customize
the variable `jdee-import-excluded-classes' to prevent specified classes
from being imported or considered for import. If the prefix argument NO-EXCLUDE
is non-nil, jdee-import-excluded-classes will be ignored.
This command uses the JDE's BeanShell interpreter. It starts the interpreter
if it is not already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, except jars implicitly included by the jvm, e.g.,
rt.jar. The NO-ERRORS is used to avoid showing erros to the user."
  (interactive
   (cl-flet ((vfn
	      (class)
	      (let ((existing-import (jdee-import-get-import (third class))))
		(if (null existing-import)
		    class
		  (message "Skipping: already imported %s" existing-import)
		  'pass))))
     (list (jdee-read-class nil nil nil nil nil #'vfn) nil current-prefix-arg t)))
  (if qualifiedp
      (unless (eq class 'pass)
	(jdee-parse-class-exists "java.util.List")
	(jdee-import-insert-import (list class) (not no-exclude)))
    (let (existing-import)
      (setq existing-import (jdee-import-get-import class))
      (if (not (null existing-import))
	  (message "Skipping: already imported %s" existing-import)
	(let ((imports (jdee-import-get-qualified-names class)))
	  (setq imports (cl-remove-duplicates imports :test 'equal))
	  (if imports
	      (jdee-import-insert-import imports (not no-exclude))
	    (if (not no-errors)
		(message "Error: could not find %s." class))))))))

(defun jdee-import-exclude-imports (imports)
  "Remove imports from IMPORTS according to `jdee-import-excluded-classes'."
  (if jdee-import-excluded-classes
      (let (synonym-list ; synonyms to be excluded
	    remaining-imports)
	;; Exclude all imports matching an element of jdee-import-excluded-classes
	;; and collect all synonyms to be excluded.
	(setq remaining-imports
	      (mapcar
	       (lambda (import)
		 (catch 'found
		   (dolist (rule jdee-import-excluded-classes)
		     (when (and
			    (string-match "[.]" import)
			    (if (functionp (car rule))
				(funcall (car rule) import)
			      (string-match (car rule) import)))
		       (message "Excluding %s." import)
		       (when (cdr rule)    ; exclude all classes having same name?
			 (setq synonym-list
			       (cons (jdee-parse-get-unqualified-name import) synonym-list)))
		       (throw 'found nil)))
		   import))
	       imports))
	;; Exclude all synonyms contained in synonym-list.
	(setq remaining-imports
	      (mapcar
	       (lambda (import)
		 (if import
		     (catch 'found
		       (dolist (synonym synonym-list)
			 (when (string-match (concat (regexp-quote synonym) "$") import)
			   (message "Excluding synonym %s." import)
			   (throw 'found nil)))
		       import)))
	       remaining-imports))
	;; Remove all nil inserted instead of excluded classes.
	(delq nil remaining-imports))
    imports))

(defun jdee-import-insert-import (new-imports &optional exclude)
  "Asks user, if necessary, to choose one of NEW-IMPORTS and
inserts the selected import in the buffer."
  (let* ((existing-imports (jdee-import-get-imports))
	 (candidate-imports (if exclude
				(jdee-import-exclude-imports new-imports)
			      new-imports))
	 (new-import
	  (if (> (length candidate-imports) 1)
	      (jdee-import-choose-import candidate-imports)
	    (car candidate-imports))))
    (if new-import
	(if (jdee-import-already-imports-class new-import existing-imports)
	    (message "This buffer already imports %s" new-import)
	  (jdee-import-insert-imports-into-buffer (list new-import))))))

(defun jdee-import-insert-imports-into-buffer (new-imports &optional exclude)
  "Inserts imports into the correct place in the buffer."
  (save-excursion
    (goto-char (jdee-import-get-import-insertion-point))
    (deactivate-mark)
    (if exclude
        (setq new-imports (jdee-import-exclude-imports new-imports)))
    (loop for new-import in new-imports do
	  (when (> (length new-import) 0) ;; added to avoid insert empty import statements.
	    (insert (concat "import " new-import ";\n"))
	    (message "Imported %s" new-import)))
    (if jdee-import-auto-collapse-imports
	(let (jdee-import-auto-collapse-imports) ;; setting this to avoid infinite recursion
	  (jdee-import-collapse-imports)))
    (if jdee-import-auto-sort
	(funcall jdee-import-auto-sort-function))
    (semantic-fetch-tags)
    (semantic-parse-changes)
    ))


(defun jdee-import-already-imports-class (class-name existing-imports)
  "Determine if a class is already being imported."
  (cl-find
   class-name
   existing-imports
   :test (lambda (new existing)
		   (let ((new-package (jdee-parse-get-package-from-name new))
			 (new-class (jdee-parse-get-unqualified-name new))
			 (existing-package (jdee-parse-get-package-from-name existing))
			 (existing-class (jdee-parse-get-unqualified-name existing)))
		     (and
		      (string= new-package existing-package)
		      (or
		       (string= new-class existing-class)
		       (string= existing-class "*")))))))

(defun jdee-import-strip-existing-imports (new-imports existing-imports)
  "Exclude classes that have already been imported."
  (delq
   nil
   (mapcar
    (lambda (new-import)
      (unless  (jdee-import-already-imports-class new-import existing-imports)
	new-import))
    new-imports)))

(defun jdee-import-choose-import (new-imports)
  "Prompts the user to select a class to import from a list of similarly
 named candidates."
  (jdee-choose-class new-imports "Import class"))

(defun jdee-import-kill-extra-imports (&optional comment)
  "Delete extra Java import statements.
An import statement is considered extra if it is a duplicate,
imports a class from the package to which this file belongs,
it is not referenced in the file,
or imports a class belonging to an already imported package, i.e.,
a package already imported by an import statement ending in .*.
If optional argument COMMENT is non-nil, the extra import statements
are commented out instead of deleted.

Usage:
  \\[jdee-import-kill-extra-imports]
  to kills extra imports.
  \\[universal-argument] \\[jdee-import-kill-extra-imports]
  to comment out extra imports.

The current buffer must be in `jdee-mode'."
  (interactive "P")
  (or (eq major-mode 'jdee-mode)
      (error "Major mode must be 'jdee-mode'"))
  (and (called-interactively-p 'interactive)
       (consp current-prefix-arg)
       (setq comment t))
  (let* ((tags    (semantic-fetch-tags))
	 (imports (semantic-brute-find-tag-by-class 'include tags)))
    (if (not imports)
	(message "No import found")
      (let* ((packages (semantic-brute-find-tag-by-class 'package tags))
	     (package-imports
	      (append
	       (mapcar
		(lambda (package)
		  ;; Return a global import name from PACKAGE tag.
		  ;; That is add ".*" at end of tag name.
		  (concat (semantic-tag-name package) ".*"))
		packages)
	       (delq nil
		     (mapcar
		      (lambda (import)
			;; Return tag name if IMPORT is global or nil if not.
			;; IMPORT is global if its name ends with ".*".
			(let ((name (semantic-tag-name import)))
			  (and (string-match "[.][*]\\'" name)
			       name)))
		      imports))))
	     (first-import (car imports))
	     extra-imports
	     required-imports)
	(save-excursion
	  ;; Get the list of extra imports
	  ;; Going to character zero so the the count-matches method work.
	  (goto-char 0)
	  (while imports
	    (let* ((import (car imports))
		   (name (semantic-tag-name import))
		   (classname (jdee-import-get-classname name))
		   (case-fold-search nil)
		   (number-of-matches
		    (count-matches
		     (concat "\\b" classname "\\b"))))
	      (if (or
		   ;; If name is already listed in the set
		   ;; of required imports...
		   (member name required-imports)
		   ;;or the class is not reference in the file
		   ;;and is not an import of the whole package i.e. .*
		   (and (< number-of-matches 2)
			(not (string= classname "*")))
		   ;; or imports a class in the current package...
		   (and
		    ;; make sure name is not a package import, e.g., foo.bar.*
		    (not (string-match "[.][*]\\'" name))
		    (member
		     ;; convert class import to equivalent package import
		     ;; e.g., foo.barClass to foo.*
		     (concat
		      (substring
		       name
		       0
		       (or (string-match "[.][^.]+\\'" name)
			   (length name)))
		      ".*")
		     package-imports)))
		  ;; add name to the list of extra imports...
		  (setq extra-imports (cons import extra-imports))
		;; otherwise add to the list or required  imports
		(setq required-imports (cons name required-imports))))
	    (setq imports (cdr imports)))
	  (if (not extra-imports)
	      (message "No extra imports found")
	    (let ((count 0))
	      ;; Move the point at the beginning of the first import
	      (goto-char (semantic-tag-start first-import))
	      ;; Kill or comment out extra imports
	      (while extra-imports
		(let* ((extra-import (car extra-imports))
		       (start (semantic-tag-start extra-import))
		       (end (semantic-tag-end extra-import)))
		  (setq count  (1+ count))
		  (if comment
		      (comment-region start end)
		    ;; The following assumes that there is only one import
		    ;; statement on the same line. Line end comments are deleted
		    ;; too.
		    (kill-region start
				 (progn
				   (goto-char end)
				   (forward-line)
				   (point))))
		  (setq extra-imports (cdr extra-imports))))
	      (message "%d extra import%s removed"
		       count (if (= count 1) "" "s")))))))))

;;;;
;;;; Helper functions
;;;;

(defun jdee-import-get-classname(import)
  "Takes as an argument an import i.e. java.util.Vector.
And returns the class name. In the above example it will
return Vector"
  (let ((pieces (split-string import "\\.")) class)
    (setq class (car (last pieces)))
    (setq pieces (split-string class "\\$"))
    (setq class (car (last pieces)))
    class))

(defun jdee-import-group-of (import-tag)
  "Return the group IMPORT-TAG belongs to or nil if not found.
A group is found as soon as the import name matches a regexp in
`jdee-import-group-of-rules'.  The returned group name depends on the
corresponding group definition in `jdee-import-group-of-rules'."
  (let ((import-name (semantic-tag-name import-tag))
	(groups      jdee-import-group-of-rules)
	match rule regexp group)
    (while (and groups (not match))
      (setq rule    (car groups)
	    groups  (cdr groups)
	    regexp  (car rule)
	    group   (cdr rule)
	    match   (and (string-match regexp import-name)
			 (cond ((stringp  group)
				group)
			       ((integerp group)
				(match-string group import-name))
			       (t
				regexp)))))
    match))

(defun jdee-import-bucketize (imports)
  "Bucketize IMPORTS tags.
Return a vector of buckets.  Each bucket is sorted alphabetically by
import name or in reverse order if `jdee-import-reverse-sort-group' is
non-nil.  There is a bucket for each different group the function
specified by `jdee-import-group-function' returns.  The last extra
bucket contains imports that do not belong to any group."
  (let (import group others bins bin i n)
    ;; Sort imports into an alist of groups.  Build a separate list
    ;; for imports not in any group.
    (while imports
      (setq import  (car imports)
	    imports (cdr imports)
	    group   (funcall (or jdee-import-group-function
				 #'jdee-import-group-of)
			     import))
      (if (not group)
	  (setq others (cons import others))
	(setq bin (assoc group bins))
	(if bin
	    (setcdr bin (cons import (cdr bin)))
	  (setq bins (cons (cons group (list import)) bins)))))
    ;; If required sort the bins by group name
    ;; Remember that bins are in reverse order at this point.
    (cond ((eq jdee-import-sorted-groups 'asc)
	   (setq bins (sort bins
			    (function
			     (lambda (bin1 bin2)
			       (string-lessp (car bin2)
					     (car bin1)))))))
	  ((eq jdee-import-sorted-groups 'desc)
	   (setq bins (sort bins
			    (function
			     (lambda (bin1 bin2)
			       (string-lessp (car bin1)
					     (car bin2)))))))
	  ((eq jdee-import-sorted-groups 'gor)
	   (let* ((group-list (mapcar (function
				       (lambda (item) (cdr item)))
				      jdee-import-group-of-rules)))
	     (setq bins
		   (sort bins
			 (function
			  (lambda (bin1 bin2)
			    (let* ((name1 (car bin1))
				   (name2 (car bin2))
				   (idx1 (length (member name1 group-list)))
				   (idx2 (length (member name2 group-list))))
			      (< idx1 idx2)))))))))
    ;; Build the vector of buckets.
    (setq bins (vconcat
		(delq nil
		      (nreverse
		       (cons (cons jdee-import-default-group-name
				   others)
			     bins))))
	  n    (length bins)
	  i    0)
    ;; Sort each bucket.
    (while (< i n)
      (setq bin (aref bins i))
      (setcdr bin (if jdee-import-reverse-sort-group
		      (semantic-sort-tags-by-name-decreasing (cdr bin))
		    (semantic-sort-tags-by-name-increasing (cdr bin))))
      (setq i (1+ i)))
    bins))

(defun jdee-import-insert-group (group &optional skip-line name)
  "Insert a GROUP of import texts in the current buffer.
If optional SKIP-LINE is non-nil skip a line before the group.
If optional NAME is non-nil add it as comment just before the group."
  (when group
    (when skip-line
      (newline)
      (if jdee-import-blank-line-between-groups
	  (newline)))
    (when (and jdee-import-insert-group-names name)
      (insert comment-start name)
      (newline))
    (insert (car group))
    (setq group (cdr group))
    (while group
      (newline)
      (insert (car group))
      (setq group (cdr group)))))

;;;;
;;;; Commands
;;;;

;;;###autoload
(defun jdee-import-organize (&optional force)
  "Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jdee-import-group-function'.  Groups are inserted in the order they
are found unless `jdee-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jdee-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jdee-import-organize] group and sort import statements.
  \\[universal-argument] \\[jdee-import-organize] to force reordering.

The current buffer must be in `jdee-mode'.  This command requires a
version of the JDE with the semantic parser."
  (interactive "P")
  (or (eq major-mode 'jdee-mode)
      (error "Major mode must be 'jdee-mode'"))
  (and (called-interactively-p 'interactive)
       (consp current-prefix-arg)
       (setq force t))
  (save-excursion
    (let* ((tags  (semantic-fetch-tags))
	   (imports (semantic-brute-find-tag-by-class 'include tags)))
      (if imports
	  (let* ((bins (jdee-import-bucketize imports))
		 (n    (length bins))
		 i l sl changed group bin)
	    (if force
		(setq changed t)
	      ;; Check if imports already ordered
	      (setq sl (apply #'append (mapcar #'cdr bins))
		    l  imports)
	      (while (and (not changed) l)
		(setq changed (not (string-equal
				    (semantic-tag-name (car l))
				    (semantic-tag-name (car sl))))
		      l  (cdr l)
		      sl (cdr sl))))
	    (if (not changed)
		(message "Import statements already ordered")
	      ;; Imports need to be reordered.
	      ;; 1- Get ordered import texts
	      (setq i 0)
	      (while (< i n)
		(setq bin (aref bins i))
		(setcdr bin (mapcar (function
				     (lambda (import)
				       (buffer-substring-no-properties
					(semantic-tag-start import)
					(progn
					  (goto-char (semantic-tag-end import))
					  (end-of-line)	; keep any line comment
					  (point)))))
				    (cdr bin)))
		(setq i (1+ i)))
	      ;; 2- Keep the point at the beginning of the first import
	      (goto-char (semantic-tag-start (car imports)))
	      ;; 2b- But check if the previous line already contains the
	      ;; group name for the first group
	      (when jdee-import-insert-group-names
		(setq i 0)
		(while (and (< i n) (not group))
		  (setq group (aref bins i)
			i     (1+ i)))
		(when (car group)
		  (forward-line -1)
		  (if (not (string< (concat comment-start (car group))
				    (thing-at-point 'line)))
		      (forward-line 1)))
		(setq group nil))
	      ;; 3- Kill current imports
	      (kill-region (point)
			   (progn
			     (goto-char (semantic-tag-end
					 (car (reverse imports))))
			     (end-of-line)
			     (point)))
	      ;; 4- Insert ordered imports
	      ;; Insert the first group found
	      (setq i 0)
	      (while (and (< i n) (not group))
		(setq group (aref bins i)
		      i     (1+ i)))
	      (jdee-import-insert-group (cdr group) nil (car group))
	      ;; Insert the others with a blank line before each group
	      (while (< i n)
		(setq group (aref bins i)
		      i (1+ i))
		(jdee-import-insert-group (cdr group) 'skip-line (car group)))))))))

(defcustom jdee-import-collapse-imports-threshold 2
  "Threshold level used by `jdee-import-collapse-imports' to decide when a
package star import is used instead of single imports. If N is the number of
classes imported by the current buffer from a package and N is >= to the
threshhold, the JDEE replaces the class imports with a package import.
Setting the threshold to 0 causes the JDE to not collapse anything at
all."
  :group 'jdee-project
  :type 'number)

(defun jdee-import-collapse-imports (&optional comments)
"Function that collapse multiple class imports from the same package
into a single .* package import. Uses
`jdee-import-collapse-imports-threshold' to decide when a .* statement
is generated. Implemented by adding the package statements and then
invoking `jdee-import-kill-extra-imports' to clean up."
  (interactive "P")
  (or (eq major-mode 'jdee-mode)
      (error "Major mode must be 'jdee-mode'"))
  (let* ((tags    (semantic-fetch-tags))
	 (imports (semantic-brute-find-tag-by-class 'include tags)))
    (if (<= jdee-import-collapse-imports-threshold 0)
	(message "Collapse threshold set to zero. No collapsing will occur.")
    (if (not imports)
	(message "No import found")
      (let* ((package-buckets (jdee-import-collapse-imports-bucketize imports))
	     (extra-imports   nil)
	     (required-imports nil)
	     (new-imports nil))
	(while package-buckets
	  (let*
	      ((bucket (car package-buckets)))
	    (if (>= (length bucket) jdee-import-collapse-imports-threshold)
		(progn
		  (add-to-list 'extra-imports (cdr bucket))
		  ;; Add the collapsing package statement
		  (add-to-list 'new-imports (concat (car bucket) ".*")))
	      (add-to-list 'required-imports (cdr bucket))))
	  (setq package-buckets (cdr package-buckets)))
	(jdee-import-insert-imports-into-buffer new-imports)
	(jdee-import-kill-extra-imports comments))))))


;; Contributed by Martin Schwamberger.
(defun jdee-import-expand-imports (&optional no-exclude)
   "Delete all package imports and replace them by their respective
class imports. The replacement is done by `jdee-import-all'.
`jdee-import-auto-collapse-imports' is temporarily disabled during the
execution of `jdee-import-all'. The optional prefix argumet NO-EXCLUDE
is used by `jdee-import-all'. This function is roughly the opposite of
`jdee-import-collapse-imports'."
   (interactive "P")
   (let* ((tags  (semantic-fetch-tags))
	  (imports (semantic-brute-find-tag-by-class 'include tags))
	  import-all
	  package-import-start
	  package-import-end
	  jdee-import-auto-collapse-imports) ; disable auto collapse
     (dolist (import imports)
       (when package-import-start
	 ;; kill from start of package-import to beginning of following import
	 (kill-region package-import-start (semantic-tag-start import))
	 (setq import-all t)
	 (setq package-import-start nil))
       (when (string-match "\\.\\*" (semantic-tag-name import)) ; package-import?
	 (setq package-import-start (semantic-tag-start import))
	 (setq package-import-end (semantic-tag-end import))))
     ;; kill last import?
     (when package-import-start
       ;; kill from start of package-import to end of line
       (kill-region package-import-start
		    (save-excursion
		      (goto-char package-import-end)
		      (end-of-line)
		      (or (eobp) (forward-char))
		      (point)))
       (setq import-all t))
     (if import-all
	 (jdee-import-all no-exclude))))


(defun jdee-import-collapse-imports-bucketize (imports)
  "Put all imports into a bucket named as the package they belong to."
  (let ((package-buckets))
    (while imports
      (let* ((import (car imports))
	     (name (semantic-tag-name import))
	     (packagename (jdee-parse-get-package-from-name name))
	     (packagebin))
	(setq packagebin (assoc packagename package-buckets))
	(if packagebin
	    (setcdr packagebin (cons import (cdr packagebin)))
	  (setq package-buckets (cons (cons packagename (list import)) package-buckets)))
	(setq imports (cdr imports))))
  package-buckets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                   ;;
;;  Import All                                                                       ;;
;;                                                                                   ;;
;;  Thanks to Philip Lord for the original idea for this command and for             ;;
;;  contributing the initial implementation.                                         ;;
;;                                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jdee-import-find-declared-classes (class-tag declared-classes)
  (let ((members (semantic-tag-get-attribute class-tag :members)))
    (dolist (member members)
      (if (eq (semantic-tag-class member) 'type)
          (progn
            (setq declared-classes
                  (append declared-classes (list (semantic-tag-name member))))
            (jdee-import-find-declared-classes member declared-classes))))))

(defun jdee-import-all-find-classes-to-import ()
  "Returns a list of unqualified class names to import into this
buffer. This function returns all the identifiers in the current
buffer that start with an uppercase character, have at least one lower
case character, and that are not included in an import statement and
are not the names of inner or outer classes declared in this buffer."
  (let (declared-classes
	imported-classes
	classes-to-import)

    ;; Get the names of classes that are already imported into this buffer.
    (let ((import-tags (semantic-brute-find-tag-by-class 'include (current-buffer))))
      (setq
	imported-classes
	(mapcar
	 (lambda (import-tag)
	   (jdee-parse-get-unqualified-name (semantic-tag-name import-tag)))
	 import-tags)))

    ;; Get the names of classes declared in this buffer.
    (let ((buffer-class-tags (semantic-brute-find-tag-by-class 'type (current-buffer))))
      (dolist (class-tag buffer-class-tags)
	(setq declared-classes (append declared-classes  (list (semantic-tag-name class-tag))))
	(jdee-import-find-declared-classes class-tag declared-classes)))

    ;; Sort through the Java tokens in this buffer, looking
    ;; for identifiers that start with an uppercase character and
    ;; do not appear in an import statement or a toplevel
    ;; class declaration.
    (let ((tokens (semantic-lex-buffer 1000)))
	     (dolist (token tokens classes-to-import)
      (let ((type (car token))
	    (start (cadr token))
	    (end (cddr token)))
	(if (eq type 'IDENTIFIER)
	    (let (case-fold-search
		  (name (buffer-substring-no-properties start end)))
	      (unless (or
		       (string-match "^[a-z]" name)
		       (not (string-match "[a-z]" name))
		       (member name declared-classes)
		       (member name imported-classes))
		(add-to-list 'classes-to-import  name t)))))))))

(defun jdee-import-is-included0 (name import0)
  "check single qualified name against a single qualified class name."
  (and import0
       (let* ((len0 (length import0))
	      (dotstar (eq t (compare-strings import0 (- len0 2) len0 ".*" nil nil nil)))
	      (import (if dotstar (substring import0 0 (- len0 2)) import0))
	      (len (length import)))
	 (or
	  (string-equal import name)	; name.equals(import)
	  (and
	   (eq t (compare-strings name 0 len import nil nil nil))  ; name.startsWith(import)
	   (eq t (compare-strings name len (1+ len) "." nil nil )) ; name[len] == "."
	   ))
	 )))

(defun jdee-import-is-included1 (name classes)
  "check single qualified name against list of qualified classes"
  (and name
       (do* ((imports classes (cdr imports))
	     (import (car imports) (car imports))
	     (incl (jdee-import-is-included0 name import) (jdee-import-is-included0 name import)))
	   ((or (null import) incl) incl)
	 )))

(defun jdee-import-is-included (names classes)
  "check single or list of qualified names against qualified classes"
  (if (listp names)
      (do* ((nlist names (cdr nlist))
	    (name (car nlist) (car nlist))
	    (incl (jdee-import-is-included1 name classes) (jdee-import-is-included1 name classes))
	    )
	  ((or (null name) incl) incl))
    (jdee-import-is-included1 names classes)
    ))

(defun jdee-import-filter-inner-imports (qualified-names)
  "remove names that are imported by outer classes or some.package.*"
  (let* ((import-tags (semantic-brute-find-tag-by-class 'include (current-buffer)))
	 (imported-classes (mapcar (lambda (import-tag) (semantic-tag-name import-tag)) import-tags))
	 (imports nil))
    (dolist (qnames qualified-names imports)
      (if (not (jdee-import-is-included qnames imported-classes))
	  (setq imports (cons qnames imports))))
    ))

(defun jdee-import-all-show ()
  "Display a list of the class names referenced in this
buffer that are not declared or explicitly imported into this
buffer and hence may need to be imported."
  (interactive)
  (let ((candidate-imports
	 (jdee-import-all-find-classes-to-import)))
    (with-output-to-temp-buffer "*jde import*"
      (mapcar
       (lambda(match)
	 (princ match)
	 (princ "\n"))
       candidate-imports))))

(defun jdee-import-all-filter (unqualified-imports &optional no-exclude)
  "Generate a list of fully qualified names of classes to
import from UNQUALIFIED-IMPORTS, excluding classes specified
by `jdee-import-exclude-imports' if NO-EXCLUDE is nil.
If `jdee-import-exclude-inner-imports' is non-nil, then also remove
any classes that appear to be included by outer-class imports."
  (let ((imports
  (mapcar
   (lambda (unqualified-class)
     (let ((qualified-imports (jdee-import-get-qualified-names unqualified-class)))
       (if no-exclude
	   qualified-imports
	 (jdee-import-exclude-imports qualified-imports))))
   unqualified-imports))
	)
    (if (or no-exclude (not jdee-import-exclude-inner-imports))
	imports
      (jdee-import-filter-inner-imports imports))))

(defun jdee-import-all-unique ()
  "Import all classes uniquely referenced by unqualified class
names in the current buffer, i.e., all referenced classes for
which there is only one fully qualified name on the current
classpath."
  (interactive)
  (let ((list
	 (jdee-import-all-filter (jdee-import-all-find-classes-to-import)))
	(retn))
    (delq nil
	  ;; take single length sublists, and return item..
	  (mapcar
	   (lambda(item)
	     (if (= 1 (length item))
		 (add-to-list 'retn
			      (car item))))
	   list))
    (if (< 0 (length retn))
	(jdee-import-insert-imports-into-buffer retn))))

(defclass jdee-import-all-dialog(efc-multi-option-dialog) nil)

(defmethod initialize-instance ((this jdee-import-all-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-multi-option-dialog-sort ((this jdee-import-all-dialog) list)
  "Sort the options."
  ;; sort the ones with the most options first...
  (sort list
	(lambda(a b)
	  ;; sort lexically
	  (if (= (length a)
		 (length b))
	      (string< (car a)
		       (car b))
	    ;; or by length
	    (> (length a)
	       (length b))))))

(defun jdee-import-all (&optional no-exclude)
  "Imports all classes that need to be imported into the current buffer.
If any of the required imports are ambiguous, this command displays a dialog
box that allows you to disambiguate the references.
Classes specified by `jdee-import-excluded-classes' will be excluded,
unless the prefix argument NO-EXCLUDE is non-nil."
  (interactive "P")
  (let* ((imports
	  (jdee-import-all-filter
	   (jdee-import-all-find-classes-to-import) no-exclude))
	 (unique-imports
	  (delq
	   nil
	   (mapcar
	   (lambda (import) (if (= (length import) 1) (car import)))
	   imports)))
	 (ambiguous-imports
	  (delq
	   nil
	   (mapcar
	   (lambda (import) (if (> (length import) 1) import))
	   imports)))
	 (dialog
	  (if ambiguous-imports
	      (jdee-import-all-dialog
	       "Multi Classes Option"
	       :options ambiguous-imports
	       :text "Select imports to insert."))))
    (if dialog
	(progn
	  (efc-dialog-show dialog)
	  (setq unique-imports
		(append unique-imports (oref dialog selection)))))
    (jdee-import-insert-imports-into-buffer unique-imports)))

;;;###autoload
(defun jdee-import-at-point (class)
  "Import a class at the current point.
The fully qualified class is received from user input."
  (interactive (list (jdee-read-class)))
  (insert (format "import %s;" class))
  (indent-according-to-mode))

(provide 'jdee-import)

;;; jdee-import.el ends here
