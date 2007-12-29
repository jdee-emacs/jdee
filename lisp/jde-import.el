;; jde-import.el --- Organize Java imports

;; Copyright (C) 2000, 2001, 2002, 2003, 2004 by David Ponce
;; Copyright (C) 2004 by Philip Lord

;; Authors:     David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;; Maintainers: David Ponce <david@dponce.com>
;;              Paul Kinnucan <paulk@mathworks.com>
;;              Martin Schwamberger <mschw@web.de>
;; Created: 15 Nov 2000
;; Version: $Revision: 1.46 $
;; Keywords: java, tools
;; VC: $Id: jde-import.el,v 1.46 2004/12/12 14:27:02 paulk Exp $

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

;; (makunbound 'jde-import-excluded-classes)
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
  (if (jde-parse-get-package-name)
      (string-match
       (concat (regexp-quote (jde-parse-get-package-name)) "\\.[^.]*$")
       class)))

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
   (list (read-from-minibuffer "Class: "
                               (thing-at-point 'symbol))
         nil
         current-prefix-arg))
  (let (existing-import)
    (setq existing-import (jde-import-get-import class))
    (if (not (null existing-import))
	(message "Skipping: already imported %s" existing-import)
      (let ((imports (jde-import-get-qualified-names class)))
        (setq imports (remove-duplicates imports :test 'equal))
        (if imports
            (jde-import-insert-import imports (not no-exclude))
          (if (not no-errors)
              (message "Error: could not find %s." class)))))))

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
  (efc-query-options
	 new-imports
	 "Select import to insert."
	 "Classes name dialog"))

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
                    (substring (count-matches 
                                (concat "\\b" classname "\\b")) 0 2)))
              (if (or 
                   ;; If name is already listed in the set
                   ;; of required imports...
                   (member name required-imports)
                   ;;or the class is not reference in the file
                   ;;and is not an import of the whole package i.e. .*
                   (and (< (string-to-number number-of-matches) 2)
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


(provide 'jde-import)


;;; History:
;;
;; $Log: jde-import.el,v $
;; Revision 1.46  2004/12/12 14:27:02  paulk
;; Add templates provided by Ole Arndt.
;;
;; Revision 1.45  2004/09/30 04:25:46  paulk
;; Change the default threshhold for collapsing imports to two classes. This causes the JDEE to collapse imports for any package from which the current buffer imports at least two classes.
;;
;; Revision 1.44  2004/09/30 04:18:21  paulk
;; Expand and fix documentation for jde-import-collapse-import command.
;;
;; Revision 1.43  2004/09/21 04:50:36  paulk
;; Include Martin Schwamberger's jde-import-expand-imports command.
;;
;; Revision 1.42  2004/08/11 04:24:00  paulk
;; Includes enhancements contributed by Martin Schwamberger.
;;
;; Revision 1.41  2004/05/19 04:17:45  paulk
;; Wrap all of jde-import-organize in a save-excursion form.
;;
;; Revision 1.40  2004/05/17 14:09:21  paulk
;; Cedet compatibility fix: change reference to semantic-toplevel-bovine-table to semantic--parse-table.
;;
;; Revision 1.39  2004/05/04 04:40:36  paulk
;; Force reparsing with semantic 1.4.
;;
;; Revision 1.38  2004/05/03 17:55:12  paulk
;; (jde-require 'sregex) to avoid shadowing standard version. Thanks to David Ponce.
;;
;; Revision 1.37  2004/03/22 06:23:10  paulk
;; jde-import-get-import-insertion-point takes class documention into account.
;; It no longer destroys class documentation. Also improved insertion of newlines.
;; Thanks to Martin Schwarmberger.
;;
;; Revision 1.36  2004/03/21 03:26:50  paulk
;; Removed Philip Lord's prototype code.
;;
;; Revision 1.35  2004/03/16 07:49:34  paulk
;; Add a jde-import-all command contributed by Philip Lord.
;;
;; Revision 1.34  2004/03/02 06:34:50  paulk
;; Update jde-import-excluded-packages to exclude packages that start with sun and com.sun. This classes contain classes named i and packages named e, confusing the JDEE when it tries to complete Iterator instances named i and Exception instances named e.
;;
;; Revision 1.33  2003/09/22 02:56:24  paulk
;; Cosmetic changes.
;;
;; Revision 1.32  2003/09/07 05:19:28  paulk
;; Extend jde-import-excluded-classes to exclude classes that
;; belong to the same package as the importing class. Thanks to
;; Martin Schwamberger.
;;
;; Revision 1.31  2003/04/09 01:25:19  jslopez
;; Updates method call from jde-import-insert-imports to jde-import-insert-import.
;;
;; Revision 1.30  2003/04/06 08:17:12  paulk
;; Fixed regression caused by Andy's efc XEmacs compatibility
;; changes. The regression was caused by the plural in the name
;; jde-import-choose-imports which implies that it should return a list
;; of imports selected by a user when in fact it should return only
;; one. The plural misled Andy to enclose the import selected by the user
;; in a list. I removed the enclosing list and I renamed the function and
;; a set of other similarly misnamed functions to the singular, e.g.,
;; jde-import-choose-import, to reflect the fact that they actually
;; insert only a single import.
;;
;; Revision 1.29  2003/03/28 05:33:29  andyp
;; XEmacs optimizations for JDEbug and efc.
;;
;; Revision 1.28  2003/02/28 14:53:08  jslopez
;; Fixes bug in jde-import-insert-imports.
;; If new-imports contain only one import and this import is excluded the
;; code was importing a nil value.
;;
;; Revision 1.27  2003/02/26 02:59:38  jslopez
;; Supresses more error messages when completing.
;;
;; Revision 1.26  2003/02/25 05:20:25  paulk
;; Fix bug that causes jde-import-organize to insert the group name
;; multiple times when called repeatedly.
;; Thanks to Joshua Spiewak <JSpiewak@axeda.com>
;;
;; Revision 1.25  2003/02/18 02:09:40  jslopez
;; Fixes regression bugs.
;;
;; Revision 1.24  2003/01/17 20:58:06  jslopez
;; Fixes regression bug causing excluded imports to show in the
;; import list.
;; Fixes typo in jde-import-insert-imports. It was using new-imports instead
;; of candidate-imports.
;;
;; Revision 1.23  2003/01/12 20:05:26  paulk
;; - Check whether import already exists AFTER the user selects the
;;   import. This is to prevent the JDE from importing a class that
;;   is already imported but from a different package.
;; - Display message when the buffer already imports the specified class.
;; - Clean up the code.
;;
;; Revision 1.22  2003/01/10 13:27:01  paulk
;; Fix jde-import-strip-existing-imports so that it handles package import statements.
;;
;; Revision 1.21  2002/09/06 13:07:12  jslopez
;; Fixes jde-import-get-classname to handle inner classes.
;;
;; Revision 1.20  2002/08/07 06:36:17  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.19  2002/05/31 18:55:08  mnl
;; Added sorting of import statements according to their occurance in the
;; group-of-rules.
;;
;; Revision 1.18  2002/05/30 04:43:31  paulk
;; Fixes bug in jde-import-get-import-insertion-point that
;; caused it to mishandle insertion of import statements
;; after package statements. Thanks to Phillip Lord <p.lord@russet.org.uk>.
;;
;; Revision 1.17  2002/02/03 07:01:39  paulk
;; Adds support for inserting group names before groups. Thanks to
;; Brian Paulsmeyer and David Ponce.
;;
;; Revision 1.16  2002/01/28 07:07:50  paulk
;; * Adds jde-import-auto-collapse-imports variable. Customizing this variable to a nonnil value
;;   causes the JDE to run jde-import-collapse-imports after importing a class. Thanks to
;;   "Max Rydahl Andersen" <max@eos.dk>.
;;
;; * Also cleaned up the code to jde-import-insert-imports-into-buffer.
;;
;; Revision 1.15  2002/01/27 04:09:57  paulk
;; Remove duplicates from import list. Thanks to Stephen Molitar.
;;
;; Revision 1.14  2002/01/06 06:56:09  paulk
;; jde-import-choose-imports now checks for the case where the user cancels
;; the class selection dialog.
;;
;; Revision 1.13  2001/12/08 13:55:29  jslopez
;; Updates the function jde-import-choose-imports to use
;; efc-option-dialog.
;;
;; Revision 1.12  2001/11/30 11:13:43  jslopez
;; Rolling back my previous changes to
;; jde-import-choose-imports, to fix regression bug.
;;
;; Revision 1.11  2001/11/28 22:50:24  jslopez
;; Fixes compilation messages.
;;
;; Revision 1.10  2001/10/19 08:57:27  paulk
;; Adds the customization variable jde-import-auto-sort-function.
;; This variable allows you to specify the function used by
;; the JDE to sort imports. Thanks to Hugh Emberson.
;;
;; Revision 1.9  2001/07/31 05:22:48  paulk
;; Adds jde-import-collapse-imports command. Thanks to Max Rydahl Andersen.
;;
;; Revision 1.8  2001/07/06 02:10:43  paulk
;; Bug fix in removing unneeded import statements.
;;
;; Revision 1.7  2001/06/07 03:35:01  paulk
;; Further fine-tuned import insertion point function.
;;
;; Revision 1.6  2001/06/06 05:19:28  paulk
;; Improved calculation of import insertion point.
;;
;; Revision 1.5  2001/04/27 01:33:42  paulk
;; jde-import-sort now refreshes parse cache. Thanks to Robert Mecklenburg <mecklen@cimsoft.com> for tthis fix.
;;
;; Revision 1.4  2001/04/26 09:06:07  paulk
;; -- jde-import-kill-extra-imports now refreshes the buffer's parse cache. This fixes a bug where successive calls to the function would incorrectly remove imports.
;;
;; -- jde-import-kill-extra-imports now removes fully qualified imports that are not referenced in the code.
;;
;; Thanks to "Javier Lopez" <jlopez@cellexchange.com>.
;;
;; Revision 1.3  2001/03/13 04:19:45  paulk
;; Cosmetic changes.
;;
;; Revision 1.2  2000/11/27 06:18:40  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.1  2000/11/20 05:15:15  paulk
;; Added jde-import-organize command. Moved all import-related code from
;; jde-wiz.el to a new package named jde-import.el.
;;
;; Revision 1.2  2000/11/17 11:52:54  david_ponce
;; - New `jde-import-group-function' option to specify the function used
;;   to associate import token to group. The default one is
;;   `jde-import-group-of'. This let the user to completely handle the
;;   way imports are grouped.
;;
;; - New `jde-import-sorted-groups' option to specify if groups will be
;;   sorted. Notice that the *default* group (the one that contains
;;   imports not belonging to any specific group) is allways the last
;;   group.
;;
;; - Improvement of the function `jde-import-group-of'. For consistency
;;   `jde-import-group-rules' is now `jde-import-group-of-rules' and it
;;   is now possible to associate a group regexp to a particular name.
;;
;; Revision 1.1  2000/11/17 11:48:31  david_ponce
;; Initial Revision.
;;

;;; jde-import.el ends here
