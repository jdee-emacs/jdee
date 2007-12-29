;;; jde-imenu.el --- imenu setup for the JDE
;; $Revision: 1.7 $ 

;; Author: Paul Kinnucan <paulk@mathworks.com>,
;;         David Ponce <david@dponce.com>
;; Maintainer: Paul Kinnucan, David Ponce
;; Keywords: java, tools

;; Copyright (C) 2000, 2001, 2002, 2004 Paul Kinnucan.

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

(require 'semantic-java)
(require 'semantic-imenu)
(require 'regexp-opt)

;;; Compatibility
(cond
 ((fboundp 'char-valid-p)
  (defalias 'jde-imenu-char-valid-p 'char-valid-p))
 ((fboundp 'char-int-p)
  (defalias 'jde-imenu-char-valid-p 'char-int-p))
 (t
  (defun jde-imenu-char-valid-p (object)
    "Return t if OBJECT is a valid normal character."
    (condition-case nil
        (progn
          (char-to-string object)
          t)
      (error nil))))
 )

;;;;
;;;; Global options
;;;;

(defcustom jde-imenu-enable t
  "*Enables creation of a classes index menu in the Emacs menubar."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-imenu-create-index-function 'semantic-create-imenu-index
  "*Function used to create the \"Classes\" imenu.
Files must be reopened to update the imenu when this option is
changed. The default is the generic `semantic-create-imenu-index'."
  :group 'jde-project
  :type 'function)

(defcustom jde-imenu-include-signature t
  "*If non-nil imenu displays full method signatures and field types.
Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-imenu-include-modifiers nil
  "*If non-nil imenu shows abbreviations for Java modifiers.
Use *Rescan* to rebuild the imenu when you have changed this option.
See also `jde-imenu-modifier-abbrev-alist'."
  :group 'jde-project
  :type 'boolean)

(defconst jde-imenu-default-modifier-abbrev-alist
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
  "Default value of `jde-imenu-modifier-abbrev-alist'.")

(defconst jde-imenu-valid-modifiers-regexp
  (concat "\\b"
          (regexp-opt
           (mapcar #'car jde-imenu-default-modifier-abbrev-alist) t)
          "\\b")
  "Regexp of valid Java modifiers used by
`jde-imenu-modifier-field-validate'.")

(defun jde-imenu-modifier-field-validate (widget)
  "Validate a Java modifier value.
Used by `jde-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (and (stringp value)
               (string-match jde-imenu-valid-modifiers-regexp value))
          nil
        (widget-put widget :error (format "Invalid modifier: %S" value))
        widget))))

(defun jde-imenu-abbrev-field-validate (widget)
  "Validate a character abbreviation.
 Used by `jde-imenu-modifier-abbrev-alist' customization."
  (save-excursion
    (let ((value (widget-value widget)))
      (if (jde-imenu-char-valid-p value)
          nil
        (widget-put widget :error
                    (format "Invalid character value: %S" value))
        widget))))
      
(defcustom jde-imenu-modifier-abbrev-alist
  jde-imenu-default-modifier-abbrev-alist
  "*Alist of character abbreviations for Java modifiers.
Each association has the form (MODIFIER . CHARACTER) where MODIFIER is
a valid Java modifier string (see `jde-imenu-valid-modifiers-regexp')
and CHARACTER any valid character. Modifiers without any valid
association are not displayed (see also `jde-imenu-include-modifiers')."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Abbrev"
		(string :tag "Modifier"
                        :validate
                        (lambda (widget)
                          (jde-imenu-modifier-field-validate widget))
                        "")
                (choice :tag "Abbreviation"
                        (const     :tag "None" nil)
                        (character :tag "Character")
                        (integer   :tag "Character value"
                                   :validate
                                   (lambda (widget)
                                     (jde-imenu-abbrev-field-validate widget))
                                   ))
                )))

(defcustom jde-imenu-sort nil
  "*If non-nil sorts items in the index menu.
You can choose:

- - 'asc   to sort by tag name ascending (ignoring case).
- - 'desc  to sort by tag name descending (ignoring case).

Use *Rescan* to rebuild the imenu when you have changed this option."
  :group 'jde-project
  :type '(choice (const :tag "No sort"    nil )
                 (const :tag "Ascending"  asc )
                 (const :tag "Descending" desc))
  :set '(lambda (sym val)
          ;; setup sorting for `semantic-create-imenu-index'
          ;; buffer local
          (setq semantic-imenu-sort-bucket-function
                (cond ((eq val 'asc)
                       'semantic-sort-tokens-by-name-increasing-ci)
                      ((eq val 'desc)
                       'semantic-sort-tokens-by-name-decreasing-ci)
                      (t
                       nil)))
          ;; global
          (set-default 'semantic-imenu-sort-bucket-function
                       semantic-imenu-sort-bucket-function)
          (set-default sym val)))

;;;;
;;;; Helper functions
;;;;

(defun jde-imenu-abbreviate-modifiers (modifiers)
  "Return a string of character abbreviations for MODIFIERS or \"\" if
not found. This string is prepended to each type, function and
variable prototype, giving a synthetic view of their modifiers (See
also `jde-imenu-include-modifiers')."
  (if (not jde-imenu-include-modifiers)
      ""
    (let ((alist jde-imenu-modifier-abbrev-alist)
          (abbrevs "")
          entry modifier)
      (while alist
        (setq entry (car alist)
              alist (cdr alist))
        (if (member (car entry) modifiers)
            (setq abbrevs
                  (concat abbrevs (if (jde-imenu-char-valid-p (cdr entry))
                                      (char-to-string (cdr entry))
                                    "")))))
      (if (> (length abbrevs) 0)
          (concat abbrevs " ")         ; trailing whitespace separator
        abbrevs))))

;;;;
;;;; Universal `semantic-imenu' stuff adapted to JDE's needs
;;;;

(defun jde-imenu-prototype-nonterminal (tag &optional parent color)
  "Return a prototype for TAG.
See also `semantic-prototype-nonterminal'."
  (let* ((tag-cat  (semantic-tag-class tag))
         (prototyper (intern-soft (format "jde-imenu-prototype-%s"
                                          tag-cat))))
    (if (fboundp prototyper)
        (funcall prototyper tag parent color)
      (semantic-format-prototype-tag-java-mode tag parent color))))

(defun jde-imenu-prototype-function (tag &optional parent color)
  "Return a function (method) prototype for TAG.
See also `semantic-java-prototype-function'."
  (let ((sign (if jde-imenu-include-signature
                  (semantic-java-prototype-function tag parent color)
                (concat (if color
                            (semantic--format-colorize-text
                             (semantic-tag-name tag) 'function)
                          (semantic-tag-name tag))
                        "()"))))
    (concat (jde-imenu-abbreviate-modifiers
             (semantic-tag-modifiers tag))
            sign)))

(defun jde-imenu-prototype-variable (tag &optional parent color)
  "Return a variable (field) prototype for TAG.
See also `semantic-java-prototype-variable'."
  (let ((sign (if jde-imenu-include-signature
                  (semantic-java-prototype-variable tag parent color)
                (if color
                    (semantic--format-colorize-text
                     (semantic-tag-name tag) 'variable)
                  (semantic-tag-name tag)))))
    (concat (jde-imenu-abbreviate-modifiers
             (semantic-tag-modifiers tag))
            sign)))

(defun jde-imenu-prototype-type (tag &optional parent color)
  "Return a type (class/interface) prototype for TAG.
See also `semantic-prototype-nonterminal'."
  (let ((sign (semantic-java-prototype-type tag parent color)))
    (concat (jde-imenu-abbreviate-modifiers
             (semantic-tag-modifiers tag))
            sign)))

;;;;
;;;; Specific JDE's imenu (to be replaced by semantic-imenu stuff)
;;;;

(defcustom jde-imenu-include-classdef t
  "*If non-nil `jde-imenu-index-class' adds *class def* items in imenu
index to go to class definition."
  :group 'jde-project
  :type 'boolean)

(defun jde-imenu-sort-tags (tags)
  "Sorts the tag list TAGS depending on `jde-imenu-sort' value."
  (cond ((eq jde-imenu-sort 'asc)
         (sort tags
               (function
                (lambda (tag1 tag2)
                  (string-lessp (upcase (semantic-tag-name tag1))
                                (upcase (semantic-tag-name tag2)))))))
        ((eq jde-imenu-sort 'desc)
         (sort tags
               (function
                (lambda (tag1 tag2)
                  (string-lessp (upcase (semantic-tag-name tag2))
                                (upcase (semantic-tag-name tag1)))))))
        (t
         tags)))

(defun jde-imenu-index-class  (class-tag)
  "Creates an imenu index for a class in CLASS-TAG."
  (let* ((class-name  (semantic-tag-name       class-tag))
         (class-type  (semantic-tag-type       class-tag))
         (class-start (semantic-tag-start      class-tag))
         (class-parts (semantic-tag-type-members class-tag))
         (class-index (jde-imenu-index-class-parts class-parts)))

    (if jde-imenu-include-classdef
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

(defun jde-imenu-index-class-parts (tags)
  "Creates an imenu index for class parts in TAGS.
When`jde-imenu-include-signature' is non-nil the
index menu displays full method signatures and field types."
  (let ((methods (semantic-find-nonterminal-by-token 'function tags))
        (fields  (semantic-find-nonterminal-by-token 'variable tags))
        (classes (semantic-find-nonterminal-by-token 'type     tags))
        index)

    (setq methods (jde-imenu-sort-tags methods))
    (while methods
      (let* ((method-tag (car methods))
             (method-name  (semantic-tag-name method-tag))
             (method-pos   (semantic-tag-start method-tag))
             method-sig)
        (if jde-imenu-include-signature
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
    
    (setq fields (jde-imenu-sort-tags fields))
    (while fields
      (let* ((field-tag (car fields))
             (field-name  (semantic-tag-name  field-tag))
             (field-pos   (semantic-tag-start field-tag)))
        (if jde-imenu-include-signature
            (setq field-name (concat (semantic-tag-type field-tag)
                                     " " field-name)))
        (setq index 
              (append 
               index (list (cons field-name field-pos)))))
      (setq fields (cdr fields)))

    (setq classes (jde-imenu-sort-tags classes))
    (while classes
      (let* ((class-tag  (car classes))
             (class-index  (jde-imenu-index-class class-tag)))
        (setq index (append index class-index)))
      (setq classes (cdr classes)))
    index))

(defun jde-create-imenu-index ()
  "Creates an imenu index for a Java source buffer.
This function uses the semantic bovinator to index the buffer."

    (semantic-fetch-tags)
 
    (let* ((tags   (semantic-fetch-tags))
	   (packages (semantic-find-nonterminal-by-token 'package tags))
	   (depends  (semantic-find-nonterminal-by-token 'include tags))
	   (classes  (semantic-find-nonterminal-by-token 'type tags))
	   depend-index
	   index)


      (setq classes (jde-imenu-sort-tags classes))
      (while classes
	(let* ((class-tag  (car classes))
	       (class-index  (jde-imenu-index-class class-tag)))
	  (setq index (append index class-index)))
	(setq classes (cdr classes)))

      (setq depends (jde-imenu-sort-tags depends))
      (while depends
	(let* ((depend-tag (car depends))
	       (depend-name  (semantic-tag-name  depend-tag))
	       (depend-pos   (semantic-tag-start depend-tag)))
	  (setq depend-index (append depend-index (list (cons depend-name depend-pos)))))
	(setq depends (cdr depends)))
      (if depend-index
	  (setq index (append index (list (cons "imports" depend-index)))))

      (setq packages (jde-imenu-sort-tags packages))
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
;;;; JDE's imenu setup
;;;;

(defun jde-imenu-setup ()
  "Setup the JDE's \"Classes\" imenu when entering jde-mode."
  (when jde-imenu-enable

    ;; semantic overloaded functions
    (semantic-install-function-overrides
     (if (fboundp 'semantic-format-tag-prototype)
         '((format-tag-prototype . jde-imenu-prototype-nonterminal))
       '((prototype-nonterminal . jde-imenu-prototype-nonterminal))
       ))

    ;; function to use for creating the imenu
    (setq imenu-create-index-function
          (if (fboundp jde-imenu-create-index-function)
              jde-imenu-create-index-function
            'semantic-create-imenu-index))

    ;; add the imenu to the menu bar for the current buffer
    (imenu-add-to-menubar "Classes")

    ))

(provide 'jde-imenu)

;; $Log: jde-imenu.el,v $
;; Revision 1.7  2004/07/06 05:25:51  paulk
;; Convert to new tag nomenclature used by semantic 2.0 to avoid
;; byte-compilation errors caused by references to obsolete names.
;;
;; Revision 1.6  2003/09/21 03:35:37  paulk
;; (jde-imenu-setup): Install correct override for Semantic 2.0. Submitted by David Ponce.
;;
;; Revision 1.5  2002/09/16 04:42:55  paulk
;; XEmacs compatibility fix: added require statement for regexp-opt package.
;;
;; Revision 1.4  2001/05/23 03:51:45  paulk
;; Removed which-func support as jde-which-method is better.
;;
;; Revision 1.3  2001/05/19 02:35:59  paulk
;; Updated to support semantic 1.4. Thanks to David Ponce.
;;
;; Revision 1.2  2000/11/27 06:18:39  paulk
;; Miscellaneous bug fixes and minor enhancements.
;;
;; Revision 1.1  2000/10/20 04:04:20  paulk
;; Initial version.
;;

;;; jde-imenu.el ends here
