;; jde-import.el --- Organize Java imports
;; $Id$

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

(require 'efc)
(require 'semantic-fw)
(require 'semantic-find)
(require 'semantic-util)
(jde-require 'sregex)

;;;;
;;;; Customization
;;;;

(defcustom jde-import-excluded-classes
  '(("^bsh\\..*" . nil)
    ("^java\\.lang\\.[^.]*$" . t)
    ("^sun\\..*" . nil)
    ("^com\\.sun\\..*" . nil)
    (jde-import-current-package-p . t))
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
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Exclude rule"
		(choice :tag "Exclude test"
			(regexp :tag "Regexp")
			(function :tag "Test function"))
		(boolean :tag "Exclude synonyms"))))

;; auto sorting of import statements
(defcustom jde-import-auto-sort nil
  "*Automatically resort import statements after a `jde-import-import'.
If non-nil, the JDE automatically resorts the import statements when a new import statement is added using `jde-import-import' or `jde-import-find-and-import'."
  :group 'jde-project
  :type 'boolean
)

(defcustom jde-import-auto-sort-function 'jde-import-sort
  "*Function to call to automatically  sort imports statements after a `jde-import-import'.
Usually `jde-import-sort' or `jde-import-organize'.  Enabled if  `jde-import-auto-sort' is not nil."
  :group 'jde-project
  :type 'function)

(defcustom jde-import-reverse-sort-group nil
  "*Non-nil to sort each import group's packages in reverse alphabetic
order.  See command `jde-import-organize'.  Note: For sorting the
groups, see variable `jde-import-sorted-groups'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-import-sorted-groups nil
  "*Non-nil to sort import groups in alphabetic order. Order may
be specified as alphabetic, reverse alphabetical or as implicitly
specified by `jde-import-group-of-rules'. In the latter case the
order of groups is the same as their appearance in
`jde-import-group-of-rules'.
See command `jde-import-organize'. Note: For sorting the packages
within each group, see variable `jde-import-reverse-sort-group'."
  :group 'jde-project
  :type '(choice :tag "Order"
		 (const :tag "No sort"                  nil)
		 (const :tag "group-of-rules order"     gor)
		 (const :tag "alphabetic order"         asc)
		 (const :tag "reverse alphabetic order" desc)))

(defcustom jde-import-group-function 'jde-import-group-of
  "*Function used to associate an import token to a group.
It receives one argument, the import token and must return a group
name string or nil if the import does not belong to any group.  The
function `jde-import-group-of' is the default value."
  :group 'jde-project
  :type 'function)

(defcustom jde-import-group-of-rules
  '(
    ("^javax?\\.")
    )
  "*Import group definitions used by `jde-import-group-of'.
Each group definition is a pair (REGEXP . GROUP) where:
- - REGEXP is a regexp that import names of this group must match.
- - GROUP is a group name or the index of the match data returned as
    group name or nil if REGEXP is the group name."
  :group 'jde-project
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

(defcustom jde-import-default-group-name nil
  "*Default group name if no group name is found.
If a group name is not found in `jde-import-group-of-rules' then this
group name is used.  If nil no default group name is used."
  :group 'jde-project
  :type '(choice (string  :tag "A String")
		 (const :tag "none" nil)))

(defcustom jde-import-insert-group-names nil
  "*If non-nil `jde-import-organize' inserts group name before imports.
See also the options `jde-import-group-of-rules' and
`jde-import-default-group-name'."
  :group 'jde-project
  :type 'boolean)

;; (makunbound 'jde-import-blank-line-between-groups)
(defcustom jde-import-blank-line-between-groups t
  "*If non-nil `jde-import-organize' inserts a blank line between groups.
See also the options `jde-import-group-of-rules' and
`jde-import-default-group-name'."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-import-auto-collapse-imports nil
  "*If non-nil jde will automatically collapse imports when imports are
inserted."
  :group 'jde-project
  :type 'boolean)

(defun jde-import-current-package-p (class)
  "Returns non-nil if the fully qualified classname CLASS belongs to
the same package as the class in the current buffer."
  (let ((pkg (jde-parse-get-package-name)))
    (if pkg
	(string= pkg (jde-parse-get-package-from-name class)))))

(defun jde-import-get-qualified-names (unqualified-class)
  "Returns a list containing all qualified name for UNQUALIFIED-CLASS."
  (jde-jeval-r
   (concat
    "jde.util.JdeUtilities.getQualifiedName(\""
    unqualified-class "\");")))

(defun jde-import-get-imports ()
  "Returns a list containing all imported classes."
  (let* (imports
	 (tags  (semantic-fetch-tags))
	 (import-tags (semantic-brute-find-tag-by-class 'include tags)))
    (dolist (import-tag import-tags)
      (setq imports
	    (cons
	     (semantic-tag-name import-tag)
	     imports)))
    (nreverse imports)))

(defun jde-import-get-import (unqualified-class)
  "Get imported name for unqualified name UNQUALIFIED-CLASS.
This name may have the form \"package.*\". Returns nil,
if there is no import statement for UNQUALIFIED-CLASS."
  (let (import
	(imports (jde-import-get-imports))
	(qualified-names (jde-import-get-qualified-names unqualified-class)))
    (catch 'found
      (dolist (class qualified-names)
	(if (setq import (jde-import-already-imports-class class imports))
	    (throw 'found import))))))

(defun jde-import-get-import-insertion-point ()
   "Determine where to insert an import statement.
If the buffer contains an import statement, return
the beginning of the next line; otherwise, if
the buffer contains a package statement, insert
three empty lines and return the beginning of
the second empty line; otherwise, if the buffer
contains a class definition, return the beginning
of the line before the class definition; otherwise,
return the beginning of the buffer."
   (let* ((tags (semantic-fetch-tags))
	  (import-tag
	   (car (last (semantic-brute-find-tag-by-class
		       'include tags))))
	  (package-tag (car (semantic-brute-find-tag-by-class
			     'package tags)))
	  (class-tag (car (semantic-brute-find-tag-by-class
			   'type tags)))
	  insertion-point)
     (cond (import-tag
	    (setq insertion-point (+ (semantic-tag-end import-tag) 1)))
	   (package-tag
	    (save-excursion
	      (goto-char (semantic-tag-end package-tag))
	      (forward-line)
	      (insert "\n")
	      (setq insertion-point (point))))
	   (class-tag
	    (setq insertion-point
		  (let ((comment-token (semantic-documentation-for-tag
					class-tag 'lex)))
		    (if comment-token
			(semantic-lex-token-start comment-token)
		      (semantic-tag-start class-tag)))))
	   (t
	    (setq insertion-point 1)))
     (save-excursion
       (goto-char insertion-point)
       (unless (and (bolp) (eolp)) (insert "\n")))
     insertion-point))

(defun jde-import-import (class)
  "*Insert an import statement for a class in the current buffer.
CLASS is the fully qualified name of the class to be imported. This
function allows you to enter an import at the head of your buffer
from any point in the buffer. The function does nothing if an
import statement for the specified class already exists.
The function does not exclude classes defined by `jde-import-excluded-classes'."
  (interactive
   "sClass: ")
  (jde-import-insert-import (list class)))

(defun jde-import-one-class (class)
  "Insert an import into the buffer if not already there."
  (interactive "s")
  (if (not (jde-import-already-imports-class class (jde-import-get-imports)))
      (jde-import-insert-imports-into-buffer (list class))))

;; Contributed by David Ponce <david_ponce@mail.schneider.fr>
(defun jde-import-sort (&optional reverse)
  "Sort Java import statements alphabetically. In reverse order if
REVERSE is non-nil.

Usage:
  \\[jde-import-sort] sort import statements ascending.
  \\[universal-argument] \\[jde-import-sort] sort descending.

The the current buffer must be in `jde-mode'. This command uses the
semantic Java parser and requires JDE 2.1.6-beta24 and above."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Invalid major mode found. Must be 'jde-mode'."))
  (or (and (local-variable-p 'semantic--parse-table (current-buffer))
	   (symbol-value 'semantic--parse-table))
      (error "Semantic Java parser not found."))
  (and (interactive-p)
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

(defun jde-import-find-and-import (class &optional no-errors no-exclude)
  "*Insert an import statement for a class in the current buffer.
CLASS is an unqualified class name. This function searches
the classpath for a class (or classes) that match CLASS. If it
finds only one, it inserts an import statements for the class at the
head of the current buffer. If it finds more than one class that matches
CLASS, it prompts you to select which class to import. You can customize
the variable `jde-import-excluded-classes' to prevent specified classes
from being imported or considered for import. If the prefix argument NO-EXCLUDE
is non-nil, jde-import-excluded-classes will be ignored.
This command uses the JDE's BeanShell interpreter. It starts the interpreter
if it is not already running so there may be a short delay generating the first
import statement in the session. Note that you must explicitly include
any directories or jars that you want the command to search in your
classpath, except jars implicitly included by the jvm, e.g.,
rt.jar. The NO-ERRORS is used to avoid showing erros to the user."
  (interactive
   (flet ((vfn
	   (class)
	   (let ((existing-import (jde-import-get-import (third class))))
	     (if (null existing-import)
		 class
	       (message "Skipping: already imported %s" existing-import)
	       'pass))))
     (list (jde-read-class nil nil nil nil nil 'vfn) nil current-prefix-arg)))
  (if (not (eq class 'pass))
      (jde-import-insert-import (list class) (not no-exclude))))

(defun jde-import-exclude-imports (imports)
  "Remove imports from IMPORTS according to `jde-import-excluded-classes'."
  (if jde-import-excluded-classes
      (let (synonym-list ; synonyms to be excluded
	    remaining-imports)
	;; Exclude all imports matching an element of jde-import-excluded-classes
	;; and collect all synonyms to be excluded.
	(setq remaining-imports
	      (mapcar
	       (lambda (import)
		 (catch 'found
		   (dolist (rule jde-import-excluded-classes)
		     (when (and
			    (string-match "[.]" import)
			    (if (functionp (car rule))
				(funcall (car rule) import)
			      (string-match (car rule) import)))
		       (message "Excluding %s." import)
		       (when (cdr rule)    ; exclude all classes having same name?
			 (setq synonym-list
			       (cons (jde-parse-get-unqualified-name import) synonym-list)))
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

(defun jde-import-insert-import (new-imports &optional exclude)
  "Asks user, if necessary, to choose one of NEW-IMPORTS and
inserts the selected import in the buffer."
  (let* ((existing-imports (jde-import-get-imports))
	 (candidate-imports (if exclude
				(jde-import-exclude-imports new-imports)
			      new-imports))
	 (new-import
	  (if (> (length candidate-imports) 1)
	      (jde-import-choose-import candidate-imports)
	    (car candidate-imports))))
    (if new-import
	(if (jde-import-already-imports-class new-import existing-imports)
	    (message "This buffer already imports %s" new-import)
	  (jde-import-insert-imports-into-buffer (list new-import))))))

(defun jde-import-insert-imports-into-buffer (new-imports &optional exclude)
  "Inserts imports into the correct place in the buffer."
  (save-excursion
    (goto-char (jde-import-get-import-insertion-point))
    (if (not jde-xemacsp) (deactivate-mark))
    (if exclude
      (setq new-imports (jde-import-exclude-imports new-imports)))
    (loop for new-import in new-imports do
	  (when (> (length new-import) 0) ;; added to avoid insert empty import statements.
	    (insert
	     (concat "import " new-import ";\n"))
	    (message "Imported %s" new-import)))
    (if jde-import-auto-collapse-imports
	(let (jde-import-auto-collapse-imports) ;; setting this to avoid infinite recursion
	  (jde-import-collapse-imports)))
    (if jde-import-auto-sort
	(funcall jde-import-auto-sort-function))))


(defun jde-import-already-imports-class (class-name existing-imports)
  "Determine if a class is already being imported."
  (find
   class-name
   existing-imports
   :test (lambda (new existing)
		   (let ((new-package (jde-parse-get-package-from-name new))
			 (new-class (jde-parse-get-unqualified-name new))
			 (existing-package (jde-parse-get-package-from-name existing))
			 (existing-class (jde-parse-get-unqualified-name existing)))
		     (and
		      (string= new-package existing-package)
		      (or
		       (string= new-class existing-class)
		       (string= existing-class "*")))))))

(defun jde-import-strip-existing-imports (new-imports existing-imports)
  "Exclude classes that have already been imported."
  (delq
   nil
   (mapcar
    (lambda (new-import)
      (unless  (jde-import-already-imports-class new-import existing-imports)
	new-import)
      new-imports))))

(defun jde-import-choose-import (new-imports)
  "Prompts the user to select a class to import from a list of similarly
 named candidates."
  (jde-choose-class new-imports "Import class"))

(defun jde-import-kill-extra-imports (&optional comment)
  "Delete extra Java import statements.
An import statement is considered extra if it is a duplicate,
imports a class from the package to which this file belongs,
it is not referenced in the file,
or imports a class belonging to an already imported package, i.e.,
a package already imported by an import statement ending in .*.
If optional argument COMMENT is non-nil, the extra import statements
are commented out instead of deleted.

Usage:
  \\[jde-import-kill-extra-imports]
  to kills extra imports.
  \\[universal-argument] \\[jde-import-kill-extra-imports]
  to comment out extra imports.

The current buffer must be in `jde-mode'."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (and (interactive-p)
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
		   (classname (jde-import-get-classname name))
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

(defun jde-import-get-classname(import)
  "Takes as an argument an import i.e. java.util.Vector.
And returns the class name. In the above example it will
return Vector"
  (let ((pieces (split-string import "\\.")) class)
    (setq class (car (last pieces)))
    (setq pieces (split-string class "\\$"))
    (setq class (car (last pieces)))
    class))

(defun jde-import-group-of (import-tag)
  "Return the group IMPORT-TAG belongs to or nil if not found.
A group is found as soon as the import name matches a regexp in
`jde-import-group-of-rules'.  The returned group name depends on the
corresponding group definition in `jde-import-group-of-rules'."
  (let ((import-name (semantic-tag-name import-tag))
	(groups      jde-import-group-of-rules)
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

(defun jde-import-bucketize (imports)
  "Bucketize IMPORTS tags.
Return a vector of buckets.  Each bucket is sorted alphabetically by
import name or in reverse order if `jde-import-reverse-sort-group' is
non-nil.  There is a bucket for each different group the function
specified by `jde-import-group-function' returns.  The last extra
bucket contains imports that do not belong to any group."
  (let (import group others bins bin i n)
    ;; Sort imports into an alist of groups.  Build a separate list
    ;; for imports not in any group.
    (while imports
      (setq import  (car imports)
	    imports (cdr imports)
	    group   (funcall (or jde-import-group-function
				 #'jde-import-group-of)
			     import))
      (if (not group)
	  (setq others (cons import others))
	(setq bin (assoc group bins))
	(if bin
	    (setcdr bin (cons import (cdr bin)))
	  (setq bins (cons (cons group (list import)) bins)))))
    ;; If required sort the bins by group name
    ;; Remember that bins are in reverse order at this point.
    (cond ((eq jde-import-sorted-groups 'asc)
	   (setq bins (sort bins
			    (function
			     (lambda (bin1 bin2)
			       (string-lessp (car bin2)
					     (car bin1)))))))
	  ((eq jde-import-sorted-groups 'desc)
	   (setq bins (sort bins
			    (function
			     (lambda (bin1 bin2)
			       (string-lessp (car bin1)
					     (car bin2)))))))
	  ((eq jde-import-sorted-groups 'gor)
	   (let* ((group-list (mapcar (function
				       (lambda (item) (cdr item)))
				      jde-import-group-of-rules)))
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
		       (cons (cons jde-import-default-group-name
				   others)
			     bins))))
	  n    (length bins)
	  i    0)
    ;; Sort each bucket.
    (while (< i n)
      (setq bin (aref bins i))
      (setcdr bin (if jde-import-reverse-sort-group
		      (semantic-sort-tags-by-name-decreasing (cdr bin))
		    (semantic-sort-tags-by-name-increasing (cdr bin))))
      (setq i (1+ i)))
    bins))

(defun jde-import-insert-group (group &optional skip-line name)
  "Insert a GROUP of import texts in the current buffer.
If optional SKIP-LINE is non-nil skip a line before the group.
If optional NAME is non-nil add it as comment just before the group."
  (when group
    (when skip-line
      (newline)
      (if jde-import-blank-line-between-groups
	  (newline)))
    (when (and jde-import-insert-group-names name)
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
(defun jde-import-organize (&optional force)
  "Organize import statements of the current Java source buffer.
If optional FORCE is non-nil force reordering even if imports are
already organized.

Imports are organized into groups returned by the function specified
by `jde-import-group-function'.  Groups are inserted in the order they
are found unless `jde-import-sorted-groups' requires that they must be
alphabetically sorted.  In each group imports are sorted by name
alphabetically or in reverse order if `jde-import-reverse-sort-group'
is non-nil.  A blank line is inserted between groups.

Usage:
  \\[jde-import-organize] group and sort import statements.
  \\[universal-argument] \\[jde-import-organize] to force reordering.

The current buffer must be in `jde-mode'.  This command requires a
version of the JDE with the semantic parser."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq force t))
  (save-excursion
    (let* ((tags  (semantic-fetch-tags))
	   (imports (semantic-brute-find-tag-by-class 'include tags)))
      (if imports
	  (let* ((bins (jde-import-bucketize imports))
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
	      (when jde-import-insert-group-names
		(setq i 0)
		(while (and (< i n) (not group))
		  (setq group (aref bins i)
			i     (1+ i)))
		(when (car group)
		  (previous-line 1)
		  (if (not (string< (concat comment-start (car group))
				    (thing-at-point 'line)))
		      (next-line 1)))
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
	      (jde-import-insert-group (cdr group) nil (car group))
	      ;; Insert the others with a blank line before each group
	      (while (< i n)
		(setq group (aref bins i)
		      i (1+ i))
		(jde-import-insert-group (cdr group) 'skip-line (car group)))))))))

(defcustom jde-import-collapse-imports-threshold 2
  "Threshold level used by `jde-import-collapse-imports' to decide when a
package star import is used instead of single imports. If N is the number of
classes imported by the current buffer from a package and N is >= to the
threshhold, the JDEE replaces the class imports with a package import.
Setting the threshold to 0 causes the JDE to not collapse anything at
all."
  :group 'jde-project
  :type 'number)

(defun jde-import-collapse-imports (&optional comments)
"Function that collapse multiple class imports from the same package
into a single .* package import. Uses
`jde-import-collapse-imports-threshold' to decide when a .* statement
is generated. Implemented by adding the package statements and then
invoking `jde-import-kill-extra-imports' to clean up."
  (interactive "P")
  (or (eq major-mode 'jde-mode)
      (error "Major mode must be 'jde-mode'"))
  (let* ((tags    (semantic-fetch-tags))
	 (imports (semantic-brute-find-tag-by-class 'include tags)))
    (if (<= jde-import-collapse-imports-threshold 0)
	(message "Collapse threshold set to zero. No collapsing will occur.")
    (if (not imports)
	(message "No import found")
      (let* ((package-buckets (jde-import-collapse-imports-bucketize imports))
	     (extra-imports   nil)
	     (required-imports nil)
	     (new-imports nil))
	(while package-buckets
	  (let*
	      ((bucket (car package-buckets)))
	    (if (>= (length bucket) jde-import-collapse-imports-threshold)
		(progn
		  (add-to-list 'extra-imports (cdr bucket))
		  ;; Add the collapsing package statement
		  (add-to-list 'new-imports (concat (car bucket) ".*")))
	      (add-to-list 'required-imports (cdr bucket))))
	  (setq package-buckets (cdr package-buckets)))
	(jde-import-insert-imports-into-buffer new-imports)
	(jde-import-kill-extra-imports comments))))))


;; Contributed by Martin Schwamberger.
(defun jde-import-expand-imports (&optional no-exclude)
   "Delete all package imports and replace them by their respective
class imports. The replacement is done by `jde-import-all'.
`jde-import-auto-collapse-imports' is temporarily disabled during the
execution of `jde-import-all'. The optional prefix argumet NO-EXCLUDE
is used by `jde-import-all'. This function is roughly the opposite of
`jde-import-collapse-imports'."
   (interactive "P")
   (let* ((tags  (semantic-fetch-tags))
	  (imports (semantic-brute-find-tag-by-class 'include tags))
	  import-all
	  package-import-start
	  package-import-end
	  jde-import-auto-collapse-imports) ; disable auto collapse
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
	 (jde-import-all no-exclude))))


(defun jde-import-collapse-imports-bucketize (imports)
  "Put all imports into a bucket named as the package they belong to."
  (let ((package-buckets))
    (while imports
      (let* ((import (car imports))
	     (name (semantic-tag-name import))
	     (packagename (jde-parse-get-package-from-name name))
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

(defun jde-import-all-find-classes-to-import ()
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
	   (jde-parse-get-unqualified-name (semantic-tag-name import-tag)))
	 import-tags)))

    ;; Get the names of classes declared in this buffer.
    (let ((buffer-class-tags (semantic-brute-find-tag-by-class 'type (current-buffer))))
      (flet ((find-declared-classes
	      (class-tag)
	      (let ((members (semantic-tag-get-attribute class-tag :members)))
		(dolist (member members)
		  (if (eq (semantic-tag-class member) 'type)
		      (progn
			(setq declared-classes
			      (append declared-classes (list (semantic-tag-name member))))
			(find-declared-classes member)))))))
      (dolist (class-tag buffer-class-tags)
	 (setq declared-classes (append declared-classes  (list (semantic-tag-name class-tag))))
	 (find-declared-classes class-tag))))

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

(defun jde-import-all-show ()
  "Display a list of the class names referenced in this
buffer that are not declared or explicitly imported into this
buffer and hence may need to be imported."
  (interactive)
  (let ((candidate-imports
	 (jde-import-all-find-classes-to-import)))
    (with-output-to-temp-buffer "*jde import*"
      (mapcar
       (lambda(match)
	 (princ match)
	 (princ "\n"))
       candidate-imports))))

(defun jde-import-all-filter (unqualified-imports &optional no-exclude)
  "Generate a list of fully qualified names of classes to
import from UNQUALIFIED-IMPORTS, excluding classes specified
by `jde-import-exclude-imports' if NO-EXCLUDE is nil."
  (mapcar
   (lambda (unqualified-class)
     (let ((qualified-imports (jde-import-get-qualified-names unqualified-class)))
       (if no-exclude
	   qualified-imports
	 (jde-import-exclude-imports qualified-imports))))
   unqualified-imports))

(defun jde-import-all-unique ()
  "Import all classes uniquely referenced by unqualified class
names in the current buffer, i.e., all referenced classes for
which there is only one fully qualified name on the current
classpath."
  (interactive)
  (let ((list
	 (jde-import-all-filter (jde-import-all-find-classes-to-import)))
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
	(jde-import-insert-imports-into-buffer retn))))

(defclass jde-import-all-dialog(efc-multi-option-dialog) nil)

(defmethod initialize-instance ((this jde-import-all-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-multi-option-dialog-sort ((this jde-import-all-dialog) list)
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

(defun jde-import-all (&optional no-exclude)
  "Imports all classes that need to be imported into the current buffer.
If any of the required imports are ambiguous, this command displays a dialog
box that allows you to disambiguate the references.
Classes specified by `jde-import-excluded-classes' will be excluded,
unless the prefix argument NO-EXCLUDE is non-nil."
  (interactive "P")
  (let* ((imports
	  (jde-import-all-filter
	   (jde-import-all-find-classes-to-import) no-exclude))
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
	      (jde-import-all-dialog
	       "Multi Classes Option"
	       :options ambiguous-imports
	       :text "Select imports to insert."))))
    (if dialog
	(progn
	  (efc-dialog-show dialog)
	  (setq unique-imports
		(append unique-imports (oref dialog selection)))))
    (jde-import-insert-imports-into-buffer unique-imports)))

;;;###autoload
(defun jde-import-at-point (class)
  "Import a class at the current point.
The fully qualified class is received from user input."
  (interactive (list (jde-read-class)))
  (insert (format "import %s;" class))
  (indent-according-to-mode))

(provide 'jde-import)

;; End of jde-import.el
