;;; jde-java-font-lock.el -- Extra level font locking for java
;; $Id$

;; Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005 by David Ponce
;; Copyright (C) 2009 by Paul Landes

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;;             Paul Landes <landes <at> mailc dt net>
;; Created: September 28 1998
;; Keywords: java, tools

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Adds some extra level font locking for java in `jde-mode'.
;;
;; - Numbers are fontified with `jde-java-font-lock-number-face'.
;;
;; - Packages in package and import statements are fontified with
;;   `jde-java-font-lock-package-face'.  Last '*' of imported packages
;;   are fontified with `jde-java-font-lock-number-face'.  Last type
;;   identifiers of imported packages are fontified with
;;   `font-lock-type-face'.
;;
;; - Modifiers are fontified with `jde-java-font-lock-modifier-face'.
;;
;; - Keywords const and goto are fontified with
;;   `font-lock-warning-face'.  These keywords are reserved, even
;;   though they are not currently used.
;;
;; - Keywords super, this and default are fontified with
;;   `font-lock-keyword-face'.
;;
;; - User's defined identifiers (see variable
;;   `jde-java-font-lock-api-file') are fontified with
;;   `jde-java-font-lock-api-face'.
;;
;; - Capitalized identifiers and special constants null, true and
;;   false are fontified with `jde-java-font-lock-constant-face'.
;;
;; - Text between `' in comments and javadoc tags (including non
;;   official javadoc tags) are fontified with
;;   `jde-java-font-lock-doc-tag-face'.
;;
;; - Javadoc links (following @link tags or enclosed in HTML <a> tags)
;;   are fontified with `jde-java-font-lock-link-face'
;;
;; - Javadoc code samples (enclosed in HTML <code> tags or following
;;   @see tags) are fontified with `jde-java-font-lock-code-face'.
;;
;; - Javadoc HTML bold and strong styles are fontified with
;;   `jde-java-font-lock-bold-face'.
;;
;; - Javadoc HTML italic and emphasized styles are fontified with
;;   `jde-java-font-lock-italic-face'.
;;
;; - Javadoc HTML underlined style is fontified with
;;   `jde-java-font-lock-underline-face'.
;;
;; - Javadoc HTML preformatted style is fontified with
;;   `jde-java-font-lock-pre-face'.
;;
;; All font-lock and jde-java-font-lock faces are individually
;; customizable.  jde-java-font-lock faces are in the customization
;; group `jde-java-font-lock-faces' which is a sub group of
;; `font-lock-highlighting-faces' (Emacs) or `font-lock-faces'
;; (XEmacs).

;; This code has been tested with GNU Emacs 20.7, 21.0 and XEmacs
;; 21.1.  Any comments, suggestions, bug reports or upgrade requests
;; are welcome.  Please send them to the maintainers.

;; WARNING: It seems there is byte-code compatibility issues between
;; Emacs and XEmacs.  When using Emacs byte-code on XEmacs font
;; locking don't work correctly for some complex matchers like those
;; used to highlight imported package name :-)

;;; History:
;;
;; See at end of this file.

;;; Code:
(require 'font-lock)
(require 'regexp-opt)
(condition-case nil
    (require 'cc-fonts)
  (error nil))

(defcustom jde-use-font-lock t
  "*Turn on font-locking if non-nil.
Set to nil to disable the use of font-locking."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-java-font-lock-max-names-by-regexp
  100
  "*Maximum number of user defined names that one regexp can match.
No limit if less than 1.  For speed, the default value of 100 seems to
be a good compromize between the number of font lock keyword regexps
to match and the complexity of each regexp.

WARNING: It seems XEmacs search fails with a very long regexp.  So if
you have a lot of user's defined names don't use a value less than 1!"
  :group 'jde-project
  :type 'integer)

;;;;
;;;; Define the faces
;;;;

(defgroup jde-java-font-lock-faces nil
  "Specific JDE faces for highlighting Java sources."
  :prefix "jde-java-font-lock-"
  :group (if (featurep 'xemacs)
	     'font-lock-faces
	   'font-lock-highlighting-faces))

(defface jde-java-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-operator-face
  '((((class grayscale)) (:foreground "grey"))
    (((class color)) (:foreground "medium blue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight operators."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-constant-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight constants."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-api-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight user's defined names."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-link-face
  '((t (:foreground "blue" :italic nil :underline t)))
  "Font Lock mode face used to highlight links."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-package-face
  '((((class color) (background dark)) (:foreground "steelblue1"))
    (((class color) (background light)) (:foreground "blue3"))
    (t (:underline t)))
  "Font Lock Mode face used to highlight packages."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-doc-tag-face
  '((((class color) (background dark)) (:foreground "light coral"))
    (((class color) (background light)) (:foreground "green4"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight doc tags."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-modifier-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight modifiers."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-private-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight private access."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-protected-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight protected access."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-public-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight public access."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-bold-face
  '((t (:bold t)))
  "Font Lock Mode face used to highlight HTML bold text style."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-italic-face
  '((t (:italic t)))
  "Font Lock Mode face used to highlight HTML italic text style."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-underline-face
  '((t (:underline t)))
  "Font Lock Mode face used to highlight HTML underlined text style."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-pre-face
  '((t nil))
  "Font Lock Mode face used to highlight HTML preformatted text style."
  :group 'jde-java-font-lock-faces)

(defface jde-java-font-lock-code-face
  '((t nil))
  "Font Lock Mode face used to highlight HTML program code style."
  :group 'jde-java-font-lock-faces)

;; Define the extra font lock faces
(defvar jde-java-font-lock-number-face    'jde-java-font-lock-number-face
  "Face name to use for numbers.")
(defvar jde-java-font-lock-operator-face  'jde-java-font-lock-operator-face
  "Face name to use for operators.")
(defvar jde-java-font-lock-constant-face  'jde-java-font-lock-constant-face
  "Face name to use for constants.")
(defvar jde-java-font-lock-package-face   'jde-java-font-lock-package-face
  "Face name to use for packages.")
(defvar jde-java-font-lock-modifier-face  'jde-java-font-lock-modifier-face
  "Face name to use for modifiers.")
(defvar jde-java-font-lock-private-face	  'jde-java-font-lock-private-face
  "Face name to use for private modifiers.")
(defvar jde-java-font-lock-protected-face 'jde-java-font-lock-protected-face
  "Face name to use for protected modifiers.")
(defvar jde-java-font-lock-public-face    'jde-java-font-lock-public-face
  "Face name to use for public modifiers.")
(defvar jde-java-font-lock-api-face       'jde-java-font-lock-api-face
  "Face name to use for user's defined names.")
(defvar jde-java-font-lock-doc-tag-face   'jde-java-font-lock-doc-tag-face
  "Face name to use for doc tags.")
(defvar jde-java-font-lock-link-face      'jde-java-font-lock-link-face
  "Face name to use for links.")
(defvar jde-java-font-lock-bold-face      'jde-java-font-lock-bold-face
  "Face name to use for HTML bold text style.")
(defvar jde-java-font-lock-italic-face    'jde-java-font-lock-italic-face
  "Face name to use for HTML italic text style.")
(defvar jde-java-font-lock-underline-face 'jde-java-font-lock-underline-face
  "Face name to use for HTML underlined text style.")
(defvar jde-java-font-lock-pre-face       'jde-java-font-lock-pre-face
  "Face name to use for HTML preformatted text style.")
(defvar jde-java-font-lock-code-face      'jde-java-font-lock-code-face
  "Face name to use for HTML program code style.")

;;;;
;;;; Useful constants
;;;;

(eval-and-compile
  (defconst jde-java-font-lock-capital-letter
    "A-Z\300-\326\330-\337_$"
    "Java identifier capital letter.")

  (defconst jde-java-font-lock-letter
    (eval-when-compile
      (concat jde-java-font-lock-capital-letter "a-z"))
    "Java identifier letter.")

  (defconst jde-java-font-lock-capital-letter-or-digit
    (eval-when-compile
      (concat jde-java-font-lock-capital-letter "0-9"))
    "Java identifier capital letter or digit.")

  (defconst jde-java-font-lock-letter-or-digit
    (eval-when-compile
      (concat jde-java-font-lock-letter "0-9"))
    "Java identifier letter or digit.")
  )

(defconst jde-java-font-lock-modifier-regexp
  (eval-when-compile
    (concat "\\<\\("
	    (regexp-opt '("abstract"
			  "const"
			  "final"
			  "native"
			  ;;; removed after making each its own font -PaulL
			  ;;"private"
			  ;;"protected"
			  ;;"public"
			  "static"
			  "strictfp"
			  "synchronized"
			  "transient"
			  "volatile"
			  ))
	    "\\)\\>"))
  "Regular expression to match Java modifiers.")

(defconst jde-java-font-lock-number-regexp
  (eval-when-compile
    (concat "\\("
	    "\\<[0-9]+[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[.][eE][-+]?[0-9]+[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[.][fFdD]\\>"
	    "\\|"
	    "\\<[0-9]+[.]"
	    "\\|"
	    "[.][0-9]+\\([eE][-+]?[0-9]+\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]+[eE][-+]?[0-9]+[fFdD]?\\>"
	    "\\|"
	    "\\<0[xX][0-9a-fA-F]+[lL]?\\>"
	    "\\|"
	    "\\<[0-9]+[lLfFdD]?\\>"
	    "\\)"
	    ))
  "Regular expression to match Java numbers.")

(defconst jde-java-font-lock-operator-regexp
  "[<>=|/*&!%:?~^]+"
  "Regular expression to match Java operators.")

(defconst jde-java-font-lock-capital-id-regexp
  (eval-when-compile
    (concat "\\(\\b[" jde-java-font-lock-capital-letter
	    "]+[" jde-java-font-lock-capital-letter-or-digit
	    "]*\\b\\)"))
  "Regular expression to match capitalised identifiers.")

;;;;
;;;; Support for fontification inside javadocs and comments.
;;;;

(defun jde-java-font-lock-remove-javadoc-keywords (keywords)
  "Remove existing javadoc font lock keywords from KEYWORDS.
That is those with \"@\" in their matcher regexp."
  (let (kw matcher)
    (while keywords
      (setq matcher  (car keywords)
	    keywords (cdr keywords))
      (if (not (and (consp matcher)
		    (stringp (car matcher))
		    (string-match "@" (car matcher))))
	  (setq kw (cons matcher kw))))
    (nreverse kw)))

(defconst jde-java-font-lock-comment-faces
  '(font-lock-comment-face font-lock-doc-face)
  "List of faces font-lock uses for comments.")

(defmacro jde-java-font-lock-at-comment (pos)
  "Return non-nil if POS is in a comment."
  `(memq (get-text-property ,pos 'face)
	 jde-java-font-lock-comment-faces))

(defsubst jde-java-font-lock-search-in-comment (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a java comment.  Buffer position END bounds
the search.  The match found must not extend after that position."
  (let ((here (point))
	ok b p)
    (while (and (not ok)
		(setq p (re-search-forward regexp end t)))
      (setq b (match-beginning 0))
      (setq ok (and (jde-java-font-lock-at-comment b)
		    (< p (next-single-property-change
			  b 'face nil (point-max))))))
    (if ok
	(point)
      (goto-char here)
      nil)))

(defsubst jde-java-font-lock-search-in-javadoc (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a javadoc comment.  Buffer position END
bounds the search.  The match found must not extend after that
position."
  (let ((here (point))
	b c ok md p)
    (while (and (not ok) (setq p (re-search-forward regexp end t)))
      (setq b  (match-beginning 0)
	    md (match-data)
	    ok (and (re-search-backward "^\\s-*\\(/[*][*]\\)" nil t)
		    (jde-java-font-lock-at-comment
		     (goto-char (setq c (match-beginning 1))))
		    (forward-comment 1)
		    (< p (point))
		    (>= b c)
		    (setq here p)))
      (goto-char p))
    (set-match-data md)
    (goto-char here)
    ok))

(defun jde-java-font-lock-quote-matcher (end)
  "Font lock matcher for comment enclosed in \`\'.
Limit search to END position."
  (jde-java-font-lock-search-in-comment
   "`\\([^']*\\)'"
   end))

(defconst jde-java-font-lock-quote-keyword
  '(jde-java-font-lock-quote-matcher
    1 jde-java-font-lock-doc-tag-face t)
  "Font lock keyword for comment enclosed in \`\'.")

(defun jde-java-font-lock-html-ahref-matcher (end)
  "Font lock matcher for HTML A HREF anchor in javadoc comments.
Limit search to END position."
  (jde-java-font-lock-search-in-javadoc
   "<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
   end))

(defconst jde-java-font-lock-html-ahref-keyword
  '(jde-java-font-lock-html-ahref-matcher
    1 jde-java-font-lock-link-face t)
  "Font lock keyword for javadoc HTML A HREF anchor.")

(eval-and-compile
  (defun jde-java-font-lock-html-tag-regexp (tag &rest aliases)
    "Return a regexp that matches HTML tag TAG.
The string between <TAG> and </TAG> is the second parenthesized
expression in the returned regexp.  ALIASES are other names for TAG."
    (let* ((tag-re (mapconcat
		    #'(lambda (tag)
			(mapconcat #'(lambda (c)
				       (format "[%c%c]"
					       (upcase c)
					       (downcase c)))
				   tag
				   ""))
		    (cons tag aliases)
		    "\\|"))
	   (hit-re (if (or (and (featurep 'xemacs)
				(or (< emacs-major-version 21)
				    (and (= emacs-major-version 21)
					 (< emacs-minor-version 4))))
			   (< emacs-major-version 21))
		       ".*"
		     ;; In GNU Emacs 21 use the "non-greedy" variant of
		     ;; the operator `*' to match the smallest possible
		     ;; substring.
		     "\\(.\\|[\r\n]\\)*?")))
      (format "<\\(%s\\)>\\(%s\\)</\\(%s\\)>" tag-re hit-re tag-re)
      )))

(eval-and-compile
  (if (featurep 'cc-fonts)

      ;; Highlight javadoc comments through cc-fonts.
      (defconst jde-java-font-lock-doc-comments
	`(
	  ;; Fontify javadoc tags (including non official ones)
	  (,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
		    "\\(@[" jde-java-font-lock-letter-or-digit "]+\\)")
	   2 jde-java-font-lock-doc-tag-face t)
	  ;; Fontify @param variable name
	  ("^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@param\\>[ \t]*\\(\\sw+\\)?"
	   2 font-lock-variable-name-face prepend t)
	  ;; Fontify @exception or @throws exception type
	  ("^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*\
@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?"
	   3 font-lock-type-face prepend t)
	  ;; Fontify @docRoot
	  ("{\\(@docRoot\\)}"
	   1 jde-java-font-lock-doc-tag-face t)
	  ;; Fontify @link
	  ("{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
	   (1 jde-java-font-lock-doc-tag-face t)
	   (2 jde-java-font-lock-link-face t))
	  ;; Fontify @see reference
	  (,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
		    "@see\\>[ \t]*"
		    "\\([.#" jde-java-font-lock-letter-or-digit "]+\\)")
	   2 jde-java-font-lock-code-face t)
	  ;; Fontify the text of a HREF anchor
	  ("<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
	   1 jde-java-font-lock-link-face t)
	  ;; Fontify other HTML tags
	  (,(jde-java-font-lock-html-tag-regexp "b")
	   2 jde-java-font-lock-bold-face t)
	  (,(jde-java-font-lock-html-tag-regexp "strong")
	   2 jde-java-font-lock-bold-face t)
	  (,(jde-java-font-lock-html-tag-regexp "i")
	   2 jde-java-font-lock-italic-face t)
	  (,(jde-java-font-lock-html-tag-regexp "em")
	   2 jde-java-font-lock-italic-face t)
	  (,(jde-java-font-lock-html-tag-regexp "u")
	   2 jde-java-font-lock-underline-face t)
	  (,(jde-java-font-lock-html-tag-regexp "code")
	   2 jde-java-font-lock-code-face t)
	  (,(jde-java-font-lock-html-tag-regexp "pre")
	   2 jde-java-font-lock-pre-face t)
	  )
	"Keywords highlighted in javadoc comments.")

    ;; Highlight javadoc comments through JDEE's own support
    (defun jde-java-font-lock-remove-javadoc-keywords (keywords)
      "Remove existing javadoc font lock keywords from KEYWORDS.
That is those with \"@\" in their matcher regexp."
      (let (kw matcher)
	(while keywords
	  (setq matcher  (car keywords)
		keywords (cdr keywords))
	  (if (not (and (consp matcher)
			(stringp (car matcher))
			(string-match "@" (car matcher))))
	      (setq kw (cons matcher kw))))
	(nreverse kw)))

    (defsubst jde-java-font-lock-search-in-javadoc (regexp end)
      "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a javadoc comment.  Buffer position END
bounds the search.  The match found must not extend after that
position."
      (let ((here (point))
	    b c ok md p)
	(while (and (not ok) (setq p (re-search-forward regexp end t)))
	  (setq b  (match-beginning 0)
		md (match-data)
		ok (and (re-search-backward "^\\s-*\\(/[*][*]\\)" nil t)
			(jde-java-font-lock-at-comment
			 (goto-char (setq c (match-beginning 1))))
			(forward-comment 1)
			(< p (point))
			(>= b c)
			(setq here p)))
	  (goto-char p))
	(set-match-data md)
	(goto-char here)
	ok))

    (defun jde-java-font-lock-html-ahref-matcher (end)
      "Font lock matcher for HTML A HREF anchor in javadoc comments.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       "<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
       end))

    (defconst jde-java-font-lock-html-ahref-keyword
      '(jde-java-font-lock-html-ahref-matcher
	1 jde-java-font-lock-link-face t)
      "Font lock keyword for javadoc HTML A HREF anchor.")

    (defvar jde-java-font-lock-html-keywords nil
      "List of HTML keywords defined so far.")

    (defmacro jde-java-font-lock-def-html-keyword (tag face)
      "Define a font-lock keyword for HTML TAG.
Data inside TAG will be highlighted with FACE.
A new keyword is pushed into `jde-java-font-lock-html-keywords'."
      (let* ((matcher (intern (format "jde-java-font-lock-html-%s-matcher" tag))))
	`(progn
	   (add-to-list 'jde-java-font-lock-html-keywords '(,matcher 2 ,face t))
	   (defun ,matcher (end)
	     (jde-java-font-lock-search-in-javadoc
	      ,(jde-java-font-lock-html-tag-regexp tag) end)))))

    (jde-java-font-lock-def-html-keyword "b" jde-java-font-lock-bold-face)
    (jde-java-font-lock-def-html-keyword "strong" jde-java-font-lock-bold-face)
    (jde-java-font-lock-def-html-keyword "i" jde-java-font-lock-italic-face)
    (jde-java-font-lock-def-html-keyword "em" jde-java-font-lock-italic-face)
    (jde-java-font-lock-def-html-keyword "u" jde-java-font-lock-underline-face)
    (jde-java-font-lock-def-html-keyword "code" jde-java-font-lock-code-face)
    (jde-java-font-lock-def-html-keyword "pre" jde-java-font-lock-pre-face)

    (defun jde-java-font-lock-javadoc-tag-matcher (end)
      "Font lock matcher for javadoc tags.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       (eval-when-compile
	 (concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
		 "\\(@[" jde-java-font-lock-letter-or-digit "]+\\)"))
       end))

    (defconst jde-java-font-lock-javadoc-tag-keyword
      '(jde-java-font-lock-javadoc-tag-matcher
	2 jde-java-font-lock-doc-tag-face t)
      "Font lock keyword for javadoc tags.")

    (defun jde-java-font-lock-javadoc-docroot-matcher (end)
      "Font lock matcher for javadoc @docRoot tags.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       "{\\(@docRoot\\)}"
       end))

    (defconst jde-java-font-lock-javadoc-docroot-keyword
      '(jde-java-font-lock-javadoc-docroot-matcher
	1 jde-java-font-lock-doc-tag-face t)
      "Font lock keyword for javadoc @docRoot tags.")

    (defun jde-java-font-lock-javadoc-link-matcher (end)
      "Font lock matcher for javadoc @link tags.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       "{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
       end))

    (defconst jde-java-font-lock-javadoc-link-keyword
      '(jde-java-font-lock-javadoc-link-matcher
	(1 jde-java-font-lock-doc-tag-face t)
	(2 jde-java-font-lock-link-face t))
      "Font lock keyword for javadoc @link tags.")

    (defun jde-java-font-lock-javadoc-see-ref-matcher (end)
      "Font lock matcher for javadoc @see references.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       (eval-when-compile
	 (concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
		 "@see\\>[ \t]*"
		 "\\([.#" jde-java-font-lock-letter-or-digit "]+\\)"))
       end))

    (defconst jde-java-font-lock-javadoc-see-ref-keyword
      '(jde-java-font-lock-javadoc-see-ref-matcher
	2 jde-java-font-lock-code-face t)
      "Font lock keyword for javadoc @see references.")

    (defun jde-java-font-lock-javadoc-param-name-matcher (end)
      "Font lock matcher for javadoc @param names.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@param\\>[ \t]*\\(\\sw+\\)?"
       end))

    (defconst jde-java-font-lock-javadoc-param-name-keyword
      '(jde-java-font-lock-javadoc-param-name-matcher
	2 font-lock-variable-name-face prepend t)
      "Font lock keyword for javadoc @param names.")

    (defun jde-java-font-lock-javadoc-exception-type-matcher (end)
      "Font lock matcher for javadoc exception types.
Limit search to END position."
      (jde-java-font-lock-search-in-javadoc
       "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*\
@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?"
       end))

    (defconst jde-java-font-lock-javadoc-exception-type-keyword
      '(jde-java-font-lock-javadoc-exception-type-matcher
	3 font-lock-type-face prepend t)
      "Font lock keyword for javadoc exception types.")

    ))

;;;;
;;;; Support for fontification of user's defined names.
;;;;

(defcustom jde-java-font-lock-api-file
  (expand-file-name "~/jde-java-font-lock.api")
  "*File which contains a list of user's defined names to fontify.
If nil no name fontification occurs.  Otherwise the specified file must
contain one name by line.  Lines not beginning with a letter are
ignored.  When you change this file or modify its content a new cache
of font lock regular expressions will be rebuilt when restarting
Emacs.  Also, you can manually rebuild the cache and update font lock
keywords by entering the command:

\\[universal-argument] \\[jde-java-font-lock-setup-keywords]."
  :group 'jde-project
  :type '(choice :tag "Names"
		 (const :tag "No" nil)
		 (file  :tag "In file" :format "%t\n%v")))

(defcustom jde-java-font-lock-api-name-filter nil
  "*Function used to filter a name."
  :group 'jde-project
  :type 'function)

(defconst jde-java-font-lock-api-entry-regexp
  (eval-when-compile
    (concat "^[" jde-java-font-lock-letter "]"
	    "[" jde-java-font-lock-letter-or-digit "]+$"))
  "Regexp to match a valid entry in `jde-java-font-lock-api-file'.")

(defconst jde-java-font-lock-api-entry-match 0
  "Index of the match data in `jde-java-font-lock-api-entry-regexp'.")

(defun jde-java-font-lock-api-names (&optional filter)
  "Return the list of names in `jde-java-font-lock-api-file'.
If optional FILTER function is non-nil it is called for each name
found and must return non-nil to include it in the result list."
  (let (k kl)
    (if (and jde-java-font-lock-api-file
	     (file-readable-p jde-java-font-lock-api-file))
	(with-temp-buffer
	  (erase-buffer)
	  (insert-file-contents jde-java-font-lock-api-file)
	  (goto-char (point-min))
	  (while (re-search-forward jde-java-font-lock-api-entry-regexp nil t)
	    (setq k (match-string jde-java-font-lock-api-entry-match))
	    ;; Allow filtering of names
	    (if (or (null filter) (funcall filter k))
		(setq kl (cons k kl))))))
    kl))

(defun jde-java-font-lock-api-split-list (l n)
  "Split list L in sub listes of N elements.
If L is nil return nil.  If N is less than 1 all elements will be in
one sub list."
  (if l
      (if (<= n 0)
	  (list l)
	(let (split-list sub-list i)
	  (while l
	    (setq i 0 sub-list nil)
	    (while (and l (< i n))
	      (setq sub-list (cons (car l) sub-list)
		    i        (1+ i)
		    l        (cdr l)))
	    (if sub-list
		(setq split-list (cons sub-list split-list))))
	  split-list))))

(defun jde-java-font-lock-api-build-regexps (max-matches)
  "Build regular expressions matching names to fontify.
MAX-MATCHES is the maximum number of names that one regular expression
will match.  If MAX-MATCHES is less than 1 one regular expression will
match all the names."
  (let ((max-specpdl-size 2000)) ;; Prevent errors in `regexp-opt'
				 ;; when processing long string listes
    (mapcar (function
	     (lambda (k)
	       (concat "\\<" (regexp-opt k t) "\\>")))
	    (jde-java-font-lock-api-split-list
	     (jde-java-font-lock-api-names
	      jde-java-font-lock-api-name-filter)
	     max-matches))))

(defvar jde-java-font-lock-api-cache nil
  "Cache of regular expressions matching names to fontify..")

(defun jde-java-font-lock-api-cache-file ()
  "Return the filename of the regular expressions cache.
There is a different cache file for each major version of (X)Emacs
because of incompatible regular expressions returned by `regexp-opt'."
  (and jde-java-font-lock-api-file
       (format "%s.%semacs-%d.apicache"
	       jde-java-font-lock-api-file
	       (if (featurep 'xemacs) "x" "")
	       emacs-major-version)))

(defconst jde-java-font-lock-api-cache-file-header
  ";;; Regular expressions matching names to fontify.
;;; Automatically generated by `jde-java-font-lock' on %s.
"
  "Header to be written into the cache file.")

(defun jde-java-font-lock-api-regexps (&optional rebuild)
  "Return regular expressions matching names to fontify.
The list is cached in variable `jde-java-font-lock-api-cache'.  If it
is nil try to initialize it from the cache file (see function
`jde-java-font-lock-api-cache-file').  If optional REBUILD flag is
non-nil or there is no cache file or the cache file is older than the
names file (see variable `jde-java-font-lock-api-file'), a new cache
is created."
  (let ((cache (jde-java-font-lock-api-cache-file)))
    (cond

     ;; Inconditionnal rebuild
     (rebuild
      ;; Clear the cache to rebuild
      (setq jde-java-font-lock-api-cache nil))

     ;; No names file exists
     ((null cache)
      ;; Clear the cache (no fontification)
      (setq jde-java-font-lock-api-cache nil))

     ;; A cache file exists
     ((file-readable-p cache)
      (if (file-newer-than-file-p jde-java-font-lock-api-file cache)
	  (progn
	    (message
	     "jde-java-font-lock: names file %s newer than cache file %s"
	     jde-java-font-lock-api-file cache)
	    ;; The api file has been modified since the cache was
	    ;; created, so clear the cache to rebuild
	    (setq jde-java-font-lock-api-cache nil))
	;; Try to load the existing cache if needed
	(or jde-java-font-lock-api-cache
	    (condition-case nil
		(load-file cache)
	      ;; If load fails clear the cache to rebuild
	      (error
	       (setq jde-java-font-lock-api-cache nil)))))))

    (or jde-java-font-lock-api-cache
	(not cache)
	;; Build a new cache if it is empty and available
	(progn
	  (message "jde-java-font-lock: building names cache...")
	  (when (setq jde-java-font-lock-api-cache
		      (jde-java-font-lock-api-build-regexps
		       jde-java-font-lock-max-names-by-regexp))
	    ;; Save regexps in cache
	    (with-current-buffer (find-file-noselect cache)
	      (erase-buffer)
	      (insert
	       (format jde-java-font-lock-api-cache-file-header
		       (current-time-string))
	       (format "(setq jde-java-font-lock-api-cache '%S)"
		       jde-java-font-lock-api-cache))
	      (save-buffer)
	      (kill-buffer (current-buffer))))
	  (message "jde-java-font-lock: building names cache...%s"
		   (if jde-java-font-lock-api-cache "done" "empty"))))
	  jde-java-font-lock-api-cache))

(defun jde-java-font-lock-api-keywords (&optional rebuild)
  "Return a list of font lock keywords for user's defined names.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (mapcar (function
	   (lambda (k)
	     (cons k 'jde-java-font-lock-api-face)))
	  (jde-java-font-lock-api-regexps rebuild)))

;;;;
;;;; Font lock setup.
;;;;

(defvar java-font-lock-keywords-4 nil
  "Extra level fontification keywords for JDE mode.")

(defun jde-java-font-lock-refontify ()
  "Re-fontify buffers in `java-mode' and `jde-mode'."
  (dolist (b (buffer-list))
    (when (buffer-live-p b)
      (with-current-buffer b
	(when (and font-lock-mode
		   (memq major-mode '(java-mode jde-mode)))
	  (message "JDEE refontify buffer %s..." b)
	  (font-lock-mode -1)
	  (font-lock-mode 1)
	  (message "JDEE refontify  buffer %s...done" b))))))

(defun jde-java-font-lock-keywords (&optional rebuild)
  "JDEE's extra level font lock keywords.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (if (featurep 'cc-fonts)

      ;; Through cc-fonts support.
      (append
       java-font-lock-keywords-3
       ;; Fontify user's defined names
       (mapcar #'(lambda (e)
		   (list
		    (c-make-font-lock-search-function
		     (car e)
		     (list 0 (cdr e) t))))
	       (jde-java-font-lock-api-keywords rebuild))
       (list
	;; Fontify modifiers.
	`(,(c-make-font-lock-search-function
	    jde-java-font-lock-modifier-regexp
	    '(0 jde-java-font-lock-modifier-face t)))
	`(,(c-make-font-lock-search-function
	    "\\<\\(false\\|null\\|true\\)\\>"
	    '(1 jde-java-font-lock-constant-face t)))
	;; Fontify default and assert as keywords
	`(,(c-make-font-lock-search-function
	    "\\<\\(default\\|assert\\)\\>"
	    '(1 'font-lock-keyword-face t)))
	;; Fontify const and goto with warning face. These keywords are
	;; reserved, even though they are not currently used.
	`(,(c-make-font-lock-search-function
	    "\\<\\(const\\|goto\\)\\>"
	    '(1 'font-lock-warning-face t)))
	;; Fontify numbers
	`(,(c-make-font-lock-search-function
	    jde-java-font-lock-number-regexp
	    '(0 jde-java-font-lock-number-face t)))
	;; Fontify operators
;;;     `(,(c-make-font-lock-search-function
;;;         jde-java-font-lock-operator-regexp
;;;         '(0 jde-java-font-lock-operator-face t)))
	;; Fontify capitalised identifiers as constant
	`(,jde-java-font-lock-capital-id-regexp
	  1 jde-java-font-lock-constant-face)
	;; Fontify text between `' in comments
	jde-java-font-lock-quote-keyword
	;; Fontify javadoc comments
	'((lambda (limit)
	    (c-font-lock-doc-comments
		"/\\*\\*" limit
	      jde-java-font-lock-doc-comments)))
	)
       )

    ;; Through JDEE's own support
    (append
     ;; Feature scoping: These must come first or the Special
     ;; constants, Modifiers and Packages from keywords-1 will catch
     ;; them.
;;; Compatibility
     (if (featurep 'xemacs)
	 (list

	  ;; Special keywords and constants
	  '("\\<\\(this\\|super\\)\\>"
	    (1 font-lock-keyword-face))
	  '("\\<\\(false\\|null\\|true\\)\\>"
	    (1 jde-java-font-lock-constant-face))
	  ))

     (list
      ;; Fontify default as keyword
      '("\\<\\(default\\)\\>" (1 font-lock-keyword-face))
      ;; Fontify assert as keyword
      '("\\<\\(assert\\)\\>" (1 font-lock-keyword-face))
      ;; Fontify const and goto with warning face. These keywords are
      ;; reserved, even though they are not currently used.
      '("\\<\\(const\\|goto\\)\\>" (1 font-lock-warning-face))
      ;; Fontify modifiers.
      (cons jde-java-font-lock-modifier-regexp
	    'jde-java-font-lock-modifier-face)
      ;; Fontify package directives
      '("\\<\\(package\\)\\>\\s-+\\(\\sw+\\)"
	(1 font-lock-keyword-face)
	(2 jde-java-font-lock-package-face nil t)
	("\\=\\.\\(\\sw+\\)" nil nil
	 (1 jde-java-font-lock-package-face nil t)))
      ;; Fontify import directives
      '("\\<\\(import\\)\\>\\s-+\\(\\sw+\\)"
	(1 font-lock-keyword-face)
	(2 (if (equal (char-after (match-end 0)) ?\.)
	       'jde-java-font-lock-package-face
	     'font-lock-type-face))
	("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
	 (1 (if (equal (char-after (match-end 0)) ?\.)
		'jde-java-font-lock-package-face
	      (if (equal (char-before (match-end 0)) ?\*)
		  'jde-java-font-lock-number-face
		'font-lock-type-face)))))
      ;; modifier protections
      '("\\<\\(private\\)\\>" (1 jde-java-font-lock-private-face))
      '("\\<\\(protected\\)\\>" (1 jde-java-font-lock-protected-face))
      '("\\<\\(public\\)\\>" (1 jde-java-font-lock-public-face))
      )
     ;; Fontify user's defined names
     (jde-java-font-lock-api-keywords rebuild)

;;; Compatibility
     (if (featurep 'xemacs)
	 java-font-lock-keywords-2
       ;; Remove existing javadoc font lock keywords from GNU Emacs
       ;; `java-font-lock-keywords-3'
       (jde-java-font-lock-remove-javadoc-keywords
	java-font-lock-keywords-3))

;;; Compatibility
     (unless (featurep 'xemacs)
       ;; GNU Emacs don't fontify capitalized types so do it
       (list
	(list
	 (concat "\\<\\([" jde-java-font-lock-capital-letter "]\\sw*\\)\\>"
		 "\\([ \t]*\\[[ \t]*\\]\\)*"
		 "\\([ \t]*\\sw\\)")
	 '(font-lock-match-c-style-declaration-item-and-skip-to-next
	   (goto-char (match-beginning 3))
	   (goto-char (match-beginning 3))
	   (1 (if (match-beginning 2)
		  font-lock-function-name-face
		font-lock-variable-name-face))))
	(cons
	 (concat "\\<\\([" jde-java-font-lock-capital-letter "]\\sw*\\)\\>"
		 "\\([ \t]*\\[[ \t]*\\]\\)*"
		 "\\([ \t]*\\sw\\)")
	 '(1 font-lock-type-face))
	'("\\<\\(new\\|instanceof\\)\\>[ \t]+\\(\\sw+\\)"
	  2 font-lock-type-face)))

     ;; Some extra fontification
     (list
      ;; Fontify numbers
      (cons jde-java-font-lock-number-regexp
	    'jde-java-font-lock-number-face)
      ;; Fontify operators
;;;      (cons jde-java-font-lock-operator-regexp
;;;            'jde-java-font-lock-operator-face)
      ;; Fontify capitalised identifiers as constant
      (cons jde-java-font-lock-capital-id-regexp
	    '(1 jde-java-font-lock-constant-face))
      ;; Fontify text between `' in comments
      jde-java-font-lock-quote-keyword
      ;; Fontify javadoc tags (including non official ones)
      jde-java-font-lock-javadoc-tag-keyword
      ;; Fontify @param variable name
      jde-java-font-lock-javadoc-param-name-keyword
      ;; Fontify @exception or @throws exception type
      jde-java-font-lock-javadoc-exception-type-keyword
      ;; Fontify @docRoot
      jde-java-font-lock-javadoc-docroot-keyword
      ;; Fontify @link
      jde-java-font-lock-javadoc-link-keyword
      ;; Fontify @see reference
      jde-java-font-lock-javadoc-see-ref-keyword
      ;; Fontify the text of a HREF anchor
      jde-java-font-lock-html-ahref-keyword
      )
     ;; Fontify other HTML tags
     jde-java-font-lock-html-keywords
     )
    ))

;;;###autoload
(defun jde-java-font-lock-setup-keywords (&optional rebuild)
  "Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (interactive "P")
  (and (interactive-p)
       (consp current-prefix-arg)
       (setq rebuild t))

  ;; Setup the JDEE's extra font lock keywords.
  (setq java-font-lock-keywords-4 (jde-java-font-lock-keywords rebuild))

  ;; Update fontification of buffers in `java-mode' and `jde-mode'.
  (when (interactive-p)
    (jde-java-font-lock-refontify)))

;; Setup `java-font-lock-keywords-4'
(jde-java-font-lock-setup-keywords)

;; Define new defaults for Font Lock mode
(defconst jde-java-font-lock-defaults
  (let ((java-defaults
	 (if (featurep 'xemacs)
	     (get 'java-mode 'font-lock-defaults)
	   ;;; To avoid compilation warning, replace obsolete variable,
	   ;;; font-lock-defaults-alist, with its value for java-mode.
	   ;;; (cdr (assq 'java-mode font-lock-defaults-alist)))))
	   ;;; Paul Kinnucan.
	   '((java-font-lock-keywords
	      java-font-lock-keywords-1
	      java-font-lock-keywords-2
	      java-font-lock-keywords-3)
	     nil nil ((?_ . "w") (?$ . "w")) nil
	     (font-lock-mark-block-function . mark-defun)))))
    (cons (append (car java-defaults) '(java-font-lock-keywords-4))
	  (cdr java-defaults)))
  "Defaults for coloring Java keywords in jde-mode. The defaults
consist of the java-mode defaults plus `java-font-lock-keywords-4'.")

;;;;
;;;; Mode hook to setup syntax coloring.
;;;;

(defun jde-setup-syntax-coloring ()
  "Mode hook to setup syntax coloring in `java-mode' and `jde-mode'.
When `jde-use-font-lock' is non-nil syntax coloring is always turned
on and uses `java-font-lock-keywords-4' extra level of fontification.
If `jde-use-font-lock' is nil syntax coloring rules are those of
standard `java-mode'."
  (when (or (featurep 'xemacs)          ; XEmacs and Emacs 21 support
	    (> emacs-major-version 20)  ; colors on ordinary terminal.
	    window-system)              ; Others only on `window-system'.
    ;; If `jde-use-font-lock' is non-nil setup
    ;; `java-font-lock-keywords-4' extra level of fontification.
    (when jde-use-font-lock
      ;; Setup `font-lock-defaults'
      (set (make-local-variable 'font-lock-defaults)
	   jde-java-font-lock-defaults)
      ;; Use the maximum decoration available
      (set (make-local-variable 'font-lock-maximum-decoration) t)
      ;; Handle multiline
      (set (make-local-variable 'font-lock-multiline) t)
      )
    ;; Turn on Font Lock Mode as needed (based on parent `java-mode'
    ;; if `jde-use-font-lock' is nil).
    (let ((major-mode (if jde-use-font-lock major-mode 'java-mode)))
      (if (featurep 'xemacs)
	  ;; The following is intended to avoid a byte-compilation warning
	  ;; when compiling this file under Emacs. The Emacs version of
	  ;; font-lock-set-defaults accepts no arguments.
	  (apply 'font-lock-set-defaults (list t))
	(if global-font-lock-mode
	    (if (fboundp 'turn-on-font-lock-if-enabled)
		(turn-on-font-lock-if-enabled)
	      (turn-on-font-lock-if-desired)))))  ;; Newer emacs renamed -if-enabled to -if-desired
    ;; Always turn on font locking if `jde-use-font-lock' is non-nil.
    (if jde-use-font-lock
	(turn-on-font-lock))))

;; By default, enable extra fontification in `jde-mode'.
(add-hook 'jde-mode-hook #'jde-setup-syntax-coloring)

(provide 'jde-java-font-lock)

;;; Change History:

;;
;; $Log: jde-java-font-lock.el,v $
;; Revision 1.24  2005/04/07 05:54:29  paulk
;; Defines faces for public, private, and protected keywords. Thanks to Paul Landes.
;;
;; Revision 1.23  2004/07/07 04:35:20  paulk
;; Made changes to eliminate byte-compilation warnings.
;;
;; Revision 1.22  2004/05/28 04:34:18  paulk
;; Remove DOS line endings.
;;
;; Revision 1.21  2004/05/14 04:03:27  paulk
;; Update to support c-doc-face-name if defined. Thanks to David Ponce.
;;
;; Revision 1.20  2004/03/03 03:14:28  paulk
;; Fix bug that causes a (wrong-type-argument integerp nil) error when entering an import statement in a new Java file. Thanks to David Ponce.
;;
;; Revision 1.19  2003/07/15 11:58:45  paulk
;; Remove CRs.
;;
;; Revision 1.18  2003/07/06 13:42:01  paulk
;; As of version 5.30, cc-mode provides its own set of font lock keywords and faces for
;; Java: cc-fonts.  The JDEE now uses cc-fonts if defined. Thanks to David Ponce.
;;
;; Revision 1.17  2003/06/07 05:07:42  paulk
;; Fix a small bug in the JDEE font lock on XEmacs 21.4.11. Thanks to Andrew Kensler.
;;
;; Revision 1.16  2003/02/07 00:38:10  jslopez
;; Adds assert as a keyword to be fortified.
;; Ideally we want the assert keyword fortified only when the user
;; is using jdk 1.4 but I don't know of a good way of enabling/disabling
;; fortifying a keyword based on another parameter. Since fortifying does
;; not cause any harm, I rather have it fortified always. This might prevent
;; users from using the assert keyword and causing them problem if they
;; want to upgrade to 1.4 and use assertions.
;;
;; Revision 1.15  2002/09/16 04:42:54  paulk
;; XEmacs compatibility fix: added require statement for regexp-opt package.
;;
;; Revision 1.14  2002/09/16 04:06:14  paulk
;; XEmacs compatibility fixes. Thanks to Andy Piper.
;;
;; Revision 1.13  2002/06/23 17:38:28  jslopez
;; Removes carriage returns.
;;
;; Revision 1.12  2002/06/22 06:29:32  paulk
;; Improves highlighting of multiline HTML tags in javadoc comments. Thanks to David Ponce.
;;
;; Revision 1.11  2001/09/18 15:03:52  jslopez
;; Adding david's fixed for the make error.
;;
;; Revision 1.10  2001/09/18 14:58:02  jslopez
;; reverted to 1.8 code
;;
;; Revision 1.8  2001/09/16 17:55:37  paulk
;; This new version should be a little bit faster when byte compiled and
;; fixes the `font-lock-add-keywords' problem reported by Klaus Berndl
;; <klaus.berndl@sdm.de>. Thanks to David Ponce.
;;
;; Revision 1.7  2001/08/06 05:32:20  paulk
;; New implementation of `jde-setup-syntax-coloring' as a
;; `java-mode-hook'.
;;
;; - If `jde-use-font-lock' is non-nil the JDE completely handles font
;;   lock setup (it turns on font locking in `java-mode' if needed) and
;;   installs the extra level of fontification in
;;   `java-font-lock-keywords-4'.
;;
;; - If `jde-use-font-lock' is nil the JDE does nothing by itself (it
;;   does not turn on font locking in `java-mode') and just delegates
;;   syntax coloring setup to standard `java-mode'.
;;
;; Revision 1.6  2001/06/05 04:54:49  paulk
;; Minor updates.
;;
;; Revision 1.5  2001/04/14 05:28:11  paulk
;; - All extra fontification added by jde-java-font-lock have a specific
;;   face.  So there is less font-lock implementation dependencies,
;;   greater flexibility to setup a color theme, and fontification can be
;;   kept consistent accross Emacs flavors :-) Notice that this change
;;   may require to redo some face customization :(
;;
;;
;; - For convenience all specific jde-java-font-lock faces are in the
;;   customization group `jde-java-font-lock-faces' which is a sub group
;;   of `font-lock-highlighting-faces' (Emacs) or `font-lock-faces'
;;   (XEmacs).
;;
;;
;; - Better fontification of numbers and imported package names.
;;
;;
;; - When used interactively the command
;;   `jde-java-font-lock-setup-keywords' automatically update
;;   fontification of all Java buffers.  This is very useful when you
;;   change your API file for example!
;;
;;
;; - The `jde-setup-syntax-coloring' is greatly enhanced.  On Emacs, it
;;   takes care of the current `global-font-lock-mode' setting.  Also, it
;;   no more use `font-lock-defaults-alist' (which is an obsolete
;;   variable on Emacs 21) but the preferred `font-lock-defaults' way to
;;   setup fontification.
;;
;; - Finally, jde-java-font-lock works in java-mode too and it can be
;;   used standalone if needed.
;;
;; Contributed by David Ponce.
;;
;; Revision 1.4  2001/01/17 18:59:03  paulk
;; Font-locking improvements from David Ponce
;;
;;    - You can now fontify user-defined identifiers with the new
;;     jde-java-font-lock-api-face.  These identifiers are read in the
;;     file specified by the `jde-java-font-lock-api-file' option.
;;
;;     The JDE provides a default file "jde-java-font-lock.api" to fontify class
;;     names of the core 'java' and 'javax' packages (JDK 1.3) and servlet
;;     API (JSDK 2.0).  To enable this fontification just put this file on
;;     your home directory, maybe modify it to suit your needs and execute
;;     the command:
;;
;;        M-x jde-java-font-lock-setup-keywords (or restart Emacs).
;;
;;     To improve reloading a cache file of regular expressions matching
;;     these names is created in the same directory (see the source for
;;     more details).
;;
;;   - Because the 'const' and 'goto' keywords are reserved, but not
;;     currently used they are now fontified with `font-lock-warning-face'.
;;
;;   - The 'default' keyword is now fontified with
;;     `font-lock-keyword-face'.  This was suggested by Stephane Nicolas
;;     s.nicolas@videotron.ca>.
;;
;; Revision 1.3  2000/12/18 05:22:45  paulk
;; *** empty log message ***
;;
;; Revision 1.2  2000/10/10 06:41:47  paulk
;; Fixed some XEmacs compatibility problems.
;;
;; Revision 1.1  2000/10/08 12:53:22  paulk
;; Initial revision.
;;

;;; jde-java-font-lock.el ends here