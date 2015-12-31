;;; jdee-imenu.el --- imenu setup for the JDEE -*-coding: utf-8;-*-

;; Author: Paul Kinnucan <paulk@mathworks.com>,
;;         David Ponce <david@dponce.com>
;; Maintainer: David Ponce, Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2000, 2001, 2002, 2004 Paul Kinnucan.
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

;;; Code:

(require 'semantic/java)
(require 'semantic/imenu)
(require 'regexp-opt)

;;;;
;;;; Global options
;;;;

(defcustom jdee-imenu-enable t
  "*Enables creation of a classes index menu in the Emacs menubar."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-imenu-create-index-function 'semantic-create-imenu-index
  "*Function used to create the \"Classes\" imenu.
Files must be reopened to update the imenu when this option is
changed. The default is the generic `semantic-create-imenu-index'."
  :group 'jdee-project
  :type 'function)

(defcustom jdee-imenu-include-signature t
  "*If non-nil imenu displays full method signatures and field types.
Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-imenu-include-modifiers nil
  "*If non-nil imenu shows abbreviations for Java modifiers.
Use *Rescan* to rebuild the imenu when you have changed this option.
See also `jdee-imenu-modifier-abbrev-alist'."
  :group 'jdee-project
  :type 'boolean)

(defconst jdee-imenu-default-modifier-abbrev-alist
  '(
    ("public"        . ?+)              ; +
    ("protected"     . 177)             ; ±
    ("private"       . 172)             ; ¬

    ("static"        . ?§)              ; §
    ("transient"     . ?#)              ; #
    ("volatile"      . ?~)              ; ~

    ("abstract"      . 170)             ; ª
    ("final"         . 182)             ; ¶
    ("native"        . ?$)              ; $

    ("synchronized"  . ?@)              ; @
    ("strictfp"      . ?%)              ; %
    )
  "Default value of `jdee-imenu-modifier-abbrev-alist'.")

(defconst jdee-imenu-valid-modifiers-regexp
  (concat "\\b"
	  (regexp-opt
	   (mapcar #'car jdee-imenu-default-modifier-abbrev-alist) t)
	  "\\b")
  "Regexp of valid Java modifiers used by
`jdee-imenu-modifier-field-validate'.")

(defun jdee-imenu-modifier-field-validate (widget)
  "Validate a Java modifier value.
Used by `jdee-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (and (stringp value)
	       (string-match jdee-imenu-valid-modifiers-regexp value))
	  nil
	(widget-put widget :error (format "Invalid modifier: %S" value))
	widget))))

(defun jdee-imenu-abbrev-field-validate (widget)
  "Validate a character abbreviation.
 Used by `jdee-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (characterp value)
	  nil
	(widget-put widget :error
		    (format "Invalid character value: %S" value))
	widget))))

(defcustom jdee-imenu-modifier-abbrev-alist
  jdee-imenu-default-modifier-abbrev-alist
  "*Alist of character abbreviations for Java modifiers.
Each association has the form (MODIFIER . CHARACTER) where MODIFIER is
a valid Java modifier string (see `jdee-imenu-valid-modifiers-regexp')
and CHARACTER any valid character. Modifiers without any valid
association are not displayed (see also `jdee-imenu-include-modifiers')."
  :group 'jdee-project
  :type '(repeat
	  (cons :tag "Abbrev"
		(string :tag "Modifier"
			:validate
			(lambda (widget)
			  (jdee-imenu-modifier-field-validate widget))
			"")
		(choice :tag "Abbreviation"
			(const     :tag "None" nil)
			(character :tag "Character")
			(integer   :tag "Character value"
				   :validate
				   (lambda (widget)
				     (jdee-imenu-abbrev-field-validate widget))
				   ))
		)))

(defun jdee--imenu-setup-sorting (sym val)
  ;; setup sorting for `semantic-create-imenu-index'
  ;; buffer local
  (setq semantic-imenu-sort-bucket-function
        (cond ((eq val 'asc)
               'semantic-sort-tags-by-name-increasing-ci)
              ((eq val 'desc)
               'semantic-sort-tags-by-name-decreasing-ci)
              (t
               nil)))
  ;; global
  (set-default 'semantic-imenu-sort-bucket-function
               semantic-imenu-sort-bucket-function)
  (set-default sym val))

(defcustom jdee-imenu-sort nil
  "*If non-nil sorts items in the index menu.
You can choose:

- - 'asc   to sort by tag name ascending (ignoring case).
- - 'desc  to sort by tag name descending (ignoring case).

Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'jdee-project
  :type '(choice (const :tag "No sort"    nil )
		 (const :tag "Ascending"  asc )
		 (const :tag "Descending" desc))
  :set #'jdee--imenu-setup-sorting)

;;;;
;;;; Helper functions
;;;;

(defun jdee-imenu-abbreviate-modifiers (modifiers)
  "Return a string of character abbreviations for MODIFIERS or \"\" if
not found. This string is prepended to each type, function and
variable prototype, giving a synthetic view of their modifiers (See
also `jdee-imenu-include-modifiers')."
  (if (not jdee-imenu-include-modifiers)
      ""
    (let ((alist jdee-imenu-modifier-abbrev-alist)
	  (abbrevs "")
	  entry modifier)
      (while alist
	(setq entry (car alist)
	      alist (cdr alist))
	(if (member (car entry) modifiers)
	    (setq abbrevs
		  (concat abbrevs (if (characterp (cdr entry))
				      (char-to-string (cdr entry))
				    "")))))
      (if (> (length abbrevs) 0)
	  (concat abbrevs " ")         ; trailing whitespace separator
	abbrevs))))

;;;;
;;;; Universal `semantic-imenu' stuff adapted to JDE's needs
;;;;

(defun jdee-imenu-prototype-nonterminal (tag &optional parent color)
  "Return a prototype for TAG.
See also `semantic-prototype-nonterminal'."
  (let* ((tag-cat  (semantic-tag-class tag))
	 (prototyper (intern-soft (format "jdee-imenu-prototype-%s"
					  tag-cat))))
    (if (fboundp prototyper)
	(funcall prototyper tag parent color)
      (if (fboundp 'semantic-format-prototype-tag-java-mode)
	  (semantic-format-prototype-tag-java-mode tag parent color) ;; cedet-1.0pre6 and earlier
	(semantic-format-tag-prototype-java-mode tag parent color)) ;; cedet-1.0pre7
      )))

(defun jdee-imenu-prototype-function (tag &optional parent color)
  "Return a function (method) prototype for TAG.
See also `semantic-java-prototype-function'."
  (let ((sign (if jdee-imenu-include-signature
		  (semantic-java-prototype-function tag parent color)
		(concat (if color
			    (semantic--format-colorize-text
			     (semantic-tag-name tag) 'function)
			  (semantic-tag-name tag))
			"()"))))
    (concat (jdee-imenu-abbreviate-modifiers
	     (semantic-tag-modifiers tag))
	    sign)))

(defun jdee-imenu-prototype-variable (tag &optional parent color)
  "Return a variable (field) prototype for TAG.
See also `semantic-java-prototype-variable'."
  (let ((sign (if jdee-imenu-include-signature
		  (semantic-java-prototype-variable tag parent color)
		(if color
		    (semantic--format-colorize-text
		     (semantic-tag-name tag) 'variable)
		  (semantic-tag-name tag)))))
    (concat (jdee-imenu-abbreviate-modifiers
	     (semantic-tag-modifiers tag))
	    sign)))

(defun jdee-imenu-prototype-type (tag &optional parent color)
  "Return a type (class/interface) prototype for TAG.
See also `semantic-prototype-nonterminal'."
  (let ((sign (semantic-java-prototype-type tag parent color)))
    (concat (jdee-imenu-abbreviate-modifiers
	     (semantic-tag-modifiers tag))
	    sign)))

;;;;
;;;; Specific JDE's imenu (to be replaced by semantic-imenu stuff)
;;;;

(defcustom jdee-imenu-include-classdef t
  "*If non-nil `jdee-imenu-index-class' adds *class def* items in imenu
index to go to class definition."
  :group 'jdee-project
  :type 'boolean)

(defun jdee-imenu-sort-tags (tags)
  "Sorts the tag list TAGS depending on `jdee-imenu-sort' value."
  (cond ((eq jdee-imenu-sort 'asc)
	 (sort tags
	       (function
		(lambda (tag1 tag2)
		  (string-lessp (upcase (semantic-tag-name tag1))
				(upcase (semantic-tag-name tag2)))))))
	((eq jdee-imenu-sort 'desc)
	 (sort tags
	       (function
		(lambda (tag1 tag2)
		  (string-lessp (upcase (semantic-tag-name tag2))
				(upcase (semantic-tag-name tag1)))))))
	(t
	 tags)))

(defun jdee-imenu-index-class  (class-tag)
  "Creates an imenu index for a class in CLASS-TAG."
  (let* ((class-name  (semantic-tag-name       class-tag))
	 (class-type  (semantic-tag-type       class-tag))
	 (class-start (semantic-tag-start      class-tag))
	 (class-parts (semantic-tag-type-members class-tag))
	 (class-index (jdee-imenu-index-class-parts class-parts)))

    (if jdee-imenu-include-classdef
	;; If requested adds a *class def* item to go to the class def.
	(setq class-index (cons (cons "*class def*" class-start)
				class-index))
      ;; Else adds an *empty* item to go to the class def. only
      ;; when there is not parts
      (or class-index
	  (setq class-index
		(list (cons "*empty*"
			    class-start)))))

    (list (cons (format "%s %s" class-type class-name)
		class-index))))

(defun jdee-imenu-index-class-parts (tags)
  "Creates an imenu index for class parts in TAGS.
When`jdee-imenu-include-signature' is non-nil the
index menu displays full method signatures and field types."
  (let ((methods (semantic-brute-find-tag-by-class 'function tags))
	(fields  (semantic-brute-find-tag-by-class 'variable tags))
	(classes (semantic-brute-find-tag-by-class 'type     tags))
	index)

    (setq methods (jdee-imenu-sort-tags methods))
    (while methods
      (let* ((method-tag (car methods))
	     (method-name  (semantic-tag-name method-tag))
	     (method-pos   (semantic-tag-start method-tag))
	     method-sig)
	(if jdee-imenu-include-signature
	    (let ((method-type  (semantic-tag-type method-tag))
		  (method-args  (semantic-tag-function-arguments method-tag)))
	      (setq method-sig (if method-type
				   (format "%s %s(" method-type method-name)
				 (format "%s(" method-name)))
	      (while method-args
		(let ((method-arg-tag (car method-args))
		      method-arg-type)
		  (when (semantic-tag-p method-arg-tag)
		    (setq method-arg-type (semantic-tag-type method-arg-tag))
		    (setq method-sig (concat method-sig method-arg-type ",")))
		  (setq method-args (cdr method-args))))
	      ;; remove the extra comma at end
	      (if (char-equal ?, (aref method-sig (1- (length method-sig))))
		  (setq method-sig (substring method-sig 0 -1)))
	      (setq method-sig (concat method-sig ")")))
	  (setq method-sig (format "%s()" method-name)))
	(setq index
	      (append
	       index (list (cons method-sig method-pos)))))
      (setq methods (cdr methods)))

    ;; Add a separator between method and field index
    (if fields
	(setq index (append index '(("-")))))

    (setq fields (jdee-imenu-sort-tags fields))
    (while fields
      (let* ((field-tag (car fields))
	     (field-name  (semantic-tag-name  field-tag))
	     (field-pos   (semantic-tag-start field-tag)))
	(if jdee-imenu-include-signature
	    (setq field-name (concat (semantic-tag-type field-tag)
				     " " field-name)))
	(setq index
	      (append
	       index (list (cons field-name field-pos)))))
      (setq fields (cdr fields)))

    (setq classes (jdee-imenu-sort-tags classes))
    (while classes
      (let* ((class-tag  (car classes))
	     (class-index  (jdee-imenu-index-class class-tag)))
	(setq index (append index class-index)))
      (setq classes (cdr classes)))
    index))

(defun jdee-create-imenu-index ()
  "Creates an imenu index for a Java source buffer.
This function uses the semantic bovinator to index the buffer."

  (semantic-fetch-tags)

  (let* ((tags   (semantic-fetch-tags))
         (packages (semantic-brute-find-tag-by-class 'package tags))
         (depends  (semantic-brute-find-tag-by-class 'include tags))
         (classes  (semantic-brute-find-tag-by-class 'type tags))
         depend-index
         index)


    (setq classes (jdee-imenu-sort-tags classes))
    (while classes
      (let* ((class-tag  (car classes))
             (class-index  (jdee-imenu-index-class class-tag)))
        (setq index (append index class-index)))
      (setq classes (cdr classes)))

    (setq depends (jdee-imenu-sort-tags depends))
    (while depends
      (let* ((depend-tag (car depends))
             (depend-name  (semantic-tag-name  depend-tag))
             (depend-pos   (semantic-tag-start depend-tag)))
        (setq depend-index (append depend-index (list (cons depend-name depend-pos)))))
      (setq depends (cdr depends)))
    (if depend-index
        (setq index (append index (list (cons "imports" depend-index)))))

    (setq packages (jdee-imenu-sort-tags packages))
    (while packages
      (let* ((package-tag (car packages))
             (package-name  (semantic-tag-name  package-tag))
             (package-pos   (semantic-tag-start package-tag)))
        (setq index
              (append
               index
               (list (cons (concat "package " package-name) package-pos)))))
      (setq packages (cdr packages)))
    index))

;;;;
;;;; JDEE's imenu setup
;;;;

(defun jdee-imenu-setup ()
  "Setup the JDE's \"Classes\" imenu when entering jdee-mode."
  (when jdee-imenu-enable

    ;; semantic overloaded functions
    (semantic-install-function-overrides
     (if (fboundp 'semantic-format-tag-prototype)
	 '((format-tag-prototype . jdee-imenu-prototype-nonterminal))
       '((prototype-nonterminal . jdee-imenu-prototype-nonterminal))
       ))

    ;; function to use for creating the imenu
    (setq imenu-create-index-function
	  (if (fboundp jdee-imenu-create-index-function)
	      jdee-imenu-create-index-function
	    'semantic-create-imenu-index))

    ;; add the imenu to the menu bar for the current buffer
    (imenu-add-to-menubar "Classes")

    ))

(provide 'jdee-imenu)

;;; jdee-imenu.el ends here
