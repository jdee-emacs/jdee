;;; jdee-font-lock.el -- Extra level font locking for java

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
;; Adds some extra level font locking for java in `jdee-mode'.
;;
;; - Numbers are fontified with `jdee-font-lock-number-face'.
;;
;; - Packages in package and import statements are fontified with
;;   `jdee-font-lock-package-face'.  Last '*' of imported packages
;;   are fontified with `jdee-font-lock-number-face'.  Last type
;;   identifiers of imported packages are fontified with
;;   `font-lock-type-face'.
;;
;; - Modifiers are fontified with `jdee-font-lock-modifier-face'.
;;
;; - Keywords const and goto are fontified with
;;   `font-lock-warning-face'.  These keywords are reserved, even
;;   though they are not currently used.
;;
;; - Keywords super, this and default are fontified with
;;   `font-lock-keyword-face'.
;;
;; - User's defined identifiers (see variable
;;   `jdee-font-lock-api-file') are fontified with
;;   `jdee-font-lock-api-face'.
;;
;; - Capitalized identifiers and special constants null, true and
;;   false are fontified with `jdee-font-lock-constant-face'.
;;
;; - Text between `' in comments and javadoc tags (including non
;;   official javadoc tags) are fontified with
;;   `jdee-font-lock-doc-tag-face'.
;;
;; - Javadoc links (following @link tags or enclosed in HTML <a> tags)
;;   are fontified with `jdee-font-lock-link-face'
;;
;; - Javadoc code samples (enclosed in HTML <code> tags or following
;;   @see tags) are fontified with `jdee-font-lock-code-face'.
;;
;; - Javadoc HTML bold and strong styles are fontified with
;;   `jdee-font-lock-bold-face'.
;;
;; - Javadoc HTML italic and emphasized styles are fontified with
;;   `jdee-font-lock-italic-face'.
;;
;; - Javadoc HTML underlined style is fontified with
;;   `jdee-font-lock-underline-face'.
;;
;; - Javadoc HTML preformatted style is fontified with
;;   `jdee-font-lock-pre-face'.
;;
;; All font-lock and jdee-font-lock faces are individually
;; customizable.  jdee-font-lock faces are in the customization
;; group `jdee-font-lock-faces' which is a sub group of
;; `font-lock-highlighting-faces' (Emacs).

;; Any comments, suggestions, bug reports or upgrade requests
;; are welcome.  Please send them to the maintainers.

;;; Code:
(require 'font-lock)
(require 'regexp-opt)

;; FIXME: refactor
(defvar jdee-font-lock-javadoc-tag-keyword)
(defvar jdee-font-lock-javadoc-param-name-keyword)
(defvar jdee-font-lock-javadoc-exception-type-keyword)
(defvar jdee-font-lock-javadoc-docroot-keyword)
(defvar jdee-font-lock-javadoc-link-keyword)
(defvar jdee-font-lock-javadoc-see-ref-keyword)
(defvar jdee-font-lock-html-keywords)


;; FIXME: (require 'cc-fonts)) doesn't work for this
(declare-function c-make-font-lock-search-function "cc-fonts" (regexp &rest highlights))

(defcustom jdee-use-font-lock t
  "*Turn on font-locking if non-nil.
Set to nil to disable the use of font-locking."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-font-lock-max-names-by-regexp
  100
  "*Maximum number of user defined names that one regexp can match.
No limit if less than 1.  For speed, the default value of 100 seems to
be a good compromize between the number of font lock keyword regexps
to match and the complexity of each regexp."
  :group 'jdee-project
  :type 'integer)

;;;;
;;;; Define the faces
;;;;

(defgroup jdee-font-lock-faces nil
  "Specific JDE faces for highlighting Java sources."
  :prefix "jdee-font-lock-"
  :group 'font-lock-highlighting-faces)

(defface jdee-font-lock-number-face
  '((((class grayscale) (background light)) (:foreground "DimGray" :italic t))
    (((class grayscale) (background dark)) (:foreground "LightGray" :italic t))
    (((class color) (background light)) (:foreground "RosyBrown"))
    (((class color) (background dark)) (:foreground "LightSalmon"))
    (t (:italic t)))
  "Font Lock mode face used to highlight numbers."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-operator-face
  '((((class grayscale)) (:foreground "grey"))
    (((class color)) (:foreground "medium blue"))
    (t (:bold t)))
  "Font Lock mode face used to highlight operators."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-constant-face
  '((((type tty) (class color)) (:foreground "magenta"))
    (((class grayscale) (background light))
     (:foreground "LightGray" :bold t :underline t))
    (((class grayscale) (background dark))
     (:foreground "Gray50" :bold t :underline t))
    (((class color) (background light)) (:foreground "CadetBlue"))
    (((class color) (background dark)) (:foreground "Aquamarine"))
    (t (:bold t :underline t)))
  "Font Lock mode face used to highlight constants."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-api-face
  '((((class grayscale) (background light)) (:foreground "DimGray"))
    (((class grayscale) (background dark)) (:foreground "LightGray"))
    (((class color) (background light)) (:foreground "dark goldenrod"))
    (((class color) (background dark)) (:foreground "light goldenrod")))
  "Font Lock mode face used to highlight user's defined names."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-link-face
  '((t (:foreground "blue" :italic nil :underline t)))
  "Font Lock mode face used to highlight links."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-package-face
  '((((class color) (background dark)) (:foreground "steelblue1"))
    (((class color) (background light)) (:foreground "blue3"))
    (t (:underline t)))
  "Font Lock Mode face used to highlight packages."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-javadoc-face
  '((t :inherit font-lock-doc-face))
  "Font Lock Mode face used to highlight javadoc (sans tags etc)."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-doc-tag-face
  '((((class color) (background dark)) (:foreground "light coral"))
    (((class color) (background light)) (:foreground "green4"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight doc tags."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-modifier-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight modifiers."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-private-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight private access."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-protected-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight protected access."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-public-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "Orchid"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight public access."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-constructor-face
  '((((type tty) (class color)) (:foreground "blue" :weight light))
    (((class grayscale) (background light)) (:foreground "LightGray" :bold t))
    (((class grayscale) (background dark)) (:foreground "DimGray" :bold t))
    (((class color) (background light)) (:foreground "DarkBlue"))
    (((class color) (background dark)) (:foreground "LightSteelBlue"))
    (t (:bold t)))
  "Font Lock Mode face used to highlight protected access."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-bold-face
  '((t (:bold t)))
  "Font Lock Mode face used to highlight HTML bold text style."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-italic-face
  '((t (:italic t)))
  "Font Lock Mode face used to highlight HTML italic text style."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-underline-face
  '((t (:underline t)))
  "Font Lock Mode face used to highlight HTML underlined text style."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-pre-face
  '((t nil))
  "Font Lock Mode face used to highlight HTML preformatted text style."
  :group 'jdee-font-lock-faces)

(defface jdee-font-lock-code-face
  '((t nil))
  "Font Lock Mode face used to highlight HTML program code style."
  :group 'jdee-font-lock-faces)

;; Define the extra font lock faces
(defvar jdee-font-lock-number-face    'jdee-font-lock-number-face
  "Face name to use for numbers.")
(defvar jdee-font-lock-operator-face  'jdee-font-lock-operator-face
  "Face name to use for operators.")
(defvar jdee-font-lock-constant-face  'jdee-font-lock-constant-face
  "Face name to use for constants.")
(defvar jdee-font-lock-package-face   'jdee-font-lock-package-face
  "Face name to use for packages.")
(defvar jdee-font-lock-javadoc-face   'jdee-font-lock-javadoc-face
  "Face name to use for javadocs (sans tags etc).")
(defvar jdee-font-lock-modifier-face  'jdee-font-lock-modifier-face
  "Face name to use for modifiers.")
(defvar jdee-font-lock-private-face	  'jdee-font-lock-private-face
  "Face name to use for private modifiers.")
(defvar jdee-font-lock-protected-face 'jdee-font-lock-protected-face
  "Face name to use for protected modifiers.")
(defvar jdee-font-lock-public-face    'jdee-font-lock-public-face
  "Face name to use for public modifiers.")
(defvar jdee-font-lock-constructor-face 'jdee-font-lock-constructor-face
  "Face name to use for constructors.")
(defvar jdee-font-lock-api-face       'jdee-font-lock-api-face
  "Face name to use for user's defined names.")
(defvar jdee-font-lock-doc-tag-face   'jdee-font-lock-doc-tag-face
  "Face name to use for doc tags.")
(defvar jdee-font-lock-link-face      'jdee-font-lock-link-face
  "Face name to use for links.")
(defvar jdee-font-lock-bold-face      'jdee-font-lock-bold-face
  "Face name to use for HTML bold text style.")
(defvar jdee-font-lock-italic-face    'jdee-font-lock-italic-face
  "Face name to use for HTML italic text style.")
(defvar jdee-font-lock-underline-face 'jdee-font-lock-underline-face
  "Face name to use for HTML underlined text style.")
(defvar jdee-font-lock-pre-face       'jdee-font-lock-pre-face
  "Face name to use for HTML preformatted text style.")
(defvar jdee-font-lock-code-face      'jdee-font-lock-code-face
  "Face name to use for HTML program code style.")

;;;;
;;;; Useful constants
;;;;

(eval-and-compile
  (defconst jdee-font-lock-capital-letter
    "A-Z\300-\326\330-\337_$"
    "Java identifier capital letter.")

  (defconst jdee-font-lock-letter
    (eval-when-compile
      (concat jdee-font-lock-capital-letter "a-z"))
    "Java identifier letter.")

  (defconst jdee-font-lock-capital-letter-or-digit
    (eval-when-compile
      (concat jdee-font-lock-capital-letter "0-9"))
    "Java identifier capital letter or digit.")

  (defconst jdee-font-lock-letter-or-digit
    (eval-when-compile
      (concat jdee-font-lock-letter "0-9"))
    "Java identifier letter or digit.")
  )

(defconst jdee-font-lock-modifier-regexp
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

(defconst jdee-font-lock-number-regexp
  (eval-when-compile
    ;; As of Java 7, underscores can appear anywhere _between_ digits
    (concat "\\("
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?[.][0-9]\\([0-9_]*[0-9]\\)?\\([eE][-+]?[0-9]\\([0-9_]*[0-9]\\)?\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?[.][eE][-+]?[0-9]\\([0-9_]*[0-9]\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?[.][fFdD]\\>"
	    "\\|"
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?[.]"
	    "\\|"
	    "[.][0-9]\\([0-9_]*[0-9]\\)?\\([eE][-+]?[0-9]\\([0-9_]*[0-9]\\)?\\)?[fFdD]?\\>"
	    "\\|"
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?[eE][-+]?[0-9]+[fFdD]?\\>"
	    "\\|"
	    "\\<0[xX][0-9a-fA-F]\\([0-9a-fA-F_]*[0-9a-fA-F]\\)?[lL]?\\>"
	    "\\|"
	    "\\<0[bB][01]\\([01_]*[01]\\)?[lL]?\\>" ; Binary literal, introduced in Java 7
	    "\\|"
	    "\\<[0-9]\\([0-9_]*[0-9]\\)?+[lLfFdD]?\\>"
	    "\\)"
	    ))
  "Regular expression to match Java numbers.")

(defconst jdee-font-lock-operator-regexp
  "[<>=|/*&!%:?~^]+"
  "Regular expression to match Java operators.")

(defconst jdee-font-lock-capital-id-regexp
  (eval-when-compile
    (concat "\\(\\b[" jdee-font-lock-capital-letter
	    "]+[" jdee-font-lock-capital-letter-or-digit
	    "]*\\b\\)"))
  "Regular expression to match capitalised identifiers.")

;;;;
;;;; Support for fontification inside javadocs and comments.
;;;;

(eval-and-compile
  (defun jdee-font-lock-html-tag-regexp (tag &rest aliases)
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
	   (hit-re "\\(.\\|[\r\n]\\)*?"))
      (format "<\\(%s\\)>\\(%s\\)</\\(%s\\)>" tag-re hit-re tag-re)
      )))

(defconst jdee-font-lock-comment-faces
  '(font-lock-comment-face font-lock-doc-face)
  "List of faces font-lock uses for comments.")

(defmacro jdee-font-lock-at-comment (pos)
  "Return non-nil if POS is in a comment."
  `(memq (get-text-property ,pos 'face)
	 jdee-font-lock-comment-faces))

(defsubst jdee-font-lock-search-in-comment (regexp end)
  "Search forward from point for regular expression REGEXP.
Ensure matching occurs in a java comment.  Buffer position END bounds
the search.  The match found must not extend after that position."
  (let ((here (point))
	ok b p)
    (while (and (not ok)
		(setq p (re-search-forward regexp end t)))
      (setq b (match-beginning 0))
      (setq ok (and (jdee-font-lock-at-comment b)
		    (< p (next-single-property-change
			  b 'face nil (point-max))))))
    (if ok
	(point)
      (goto-char here)
      nil)))

(defsubst jdee-font-lock-search-in-javadoc (regexp end)
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
		    (jdee-font-lock-at-comment
		     (goto-char (setq c (match-beginning 1))))
		    (forward-comment 1)
		    (< p (point))
		    (>= b c)
		    (setq here p)))
      (goto-char p))
    (set-match-data md)
    (goto-char here)
    ok))

(defun jdee-font-lock-quote-matcher (end)
  "Font lock matcher for comment enclosed in \`\'.
Limit search to END position."
  (jdee-font-lock-search-in-comment
   "`\\([^']*\\)'"
   end))

(defconst jdee-font-lock-quote-keyword
  '(jdee-font-lock-quote-matcher
    1 jdee-font-lock-doc-tag-face t)
  "Font lock keyword for comment enclosed in \`\'.")

(defun jdee-font-lock-html-ahref-matcher (end)
  "Font lock matcher for HTML A HREF anchor in javadoc comments.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   "<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
   end))

(defconst jdee-font-lock-html-ahref-keyword
  '(jdee-font-lock-html-ahref-matcher
    1 jdee-font-lock-link-face t)
  "Font lock keyword for javadoc HTML A HREF anchor.")

;; Highlight javadoc comments through cc-fonts.
(defconst jdee-font-lock-doc-comments
  `(
    ;; Fontify javadoc tags (including non official ones)
    (,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
	      "\\(@[" jdee-font-lock-letter-or-digit "]+\\)")
     2 jdee-font-lock-doc-tag-face t)
    ;; Fontify @param variable name
    ("^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@param\\>[ \t]*\\(\\sw+\\)?"
     2 font-lock-variable-name-face prepend t)
    ;; Fontify @exception or @throws exception type
    ("^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*\
@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?"
     3 font-lock-type-face prepend t)
    ;; Fontify @docRoot
    ("{\\(@docRoot\\)}"
     1 jdee-font-lock-doc-tag-face t)
    ;; Fontify @link
    ("{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
     (1 jdee-font-lock-doc-tag-face t)
     (2 jdee-font-lock-link-face t))
    ;; Fontify @see reference
    (,(concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
	      "@see\\>[ \t]*"
	      "\\([.#" jdee-font-lock-letter-or-digit "]+\\)")
     2 jdee-font-lock-code-face t)
    ;; Fontify the text of a HREF anchor
    ("<[Aa]\\s-+[Hh][Rr][Ee][Ff][^>]*>\\([^>]+\\)</[Aa]>"
     1 jdee-font-lock-link-face t)
    ;; Fontify other HTML tags
    (,(jdee-font-lock-html-tag-regexp "b")
     2 jdee-font-lock-bold-face t)
    (,(jdee-font-lock-html-tag-regexp "strong")
     2 jdee-font-lock-bold-face t)
    (,(jdee-font-lock-html-tag-regexp "i")
     2 jdee-font-lock-italic-face t)
    (,(jdee-font-lock-html-tag-regexp "em")
     2 jdee-font-lock-italic-face t)
    (,(jdee-font-lock-html-tag-regexp "u")
     2 jdee-font-lock-underline-face t)
    (,(jdee-font-lock-html-tag-regexp "code")
     2 jdee-font-lock-code-face t)
    (,(jdee-font-lock-html-tag-regexp "pre")
     2 jdee-font-lock-pre-face t)
    )
  "Keywords highlighted in javadoc comments.")

;; Highlight javadoc comments through JDEE's own support
(defun jdee-font-lock-remove-javadoc-keywords (keywords)
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

(defconst jdee-font-lock-html-ahref-keyword
  '(jdee-font-lock-html-ahref-matcher
    1 jdee-font-lock-link-face t)
  "Font lock keyword for javadoc HTML A HREF anchor.")

(defvar jdee-font-lock-html-keywords nil
  "List of HTML keywords defined so far.")

(defmacro jdee-font-lock-def-html-keyword (tag face)
  "Define a font-lock keyword for HTML TAG.
Data inside TAG will be highlighted with FACE.
A new keyword is pushed into `jdee-font-lock-html-keywords'."
  (let* ((matcher (intern (format "jdee-font-lock-html-%s-matcher" tag))))
    `(progn
       (add-to-list 'jdee-font-lock-html-keywords '(,matcher 2 ,face t))
       (defun ,matcher (end)
	 (jdee-font-lock-search-in-javadoc
	  ,(jdee-font-lock-html-tag-regexp tag) end)))))

(jdee-font-lock-def-html-keyword "b" jdee-font-lock-bold-face)
(jdee-font-lock-def-html-keyword "strong" jdee-font-lock-bold-face)
(jdee-font-lock-def-html-keyword "i" jdee-font-lock-italic-face)
(jdee-font-lock-def-html-keyword "em" jdee-font-lock-italic-face)
(jdee-font-lock-def-html-keyword "u" jdee-font-lock-underline-face)
(jdee-font-lock-def-html-keyword "code" jdee-font-lock-code-face)
(jdee-font-lock-def-html-keyword "pre" jdee-font-lock-pre-face)

(defun jdee-font-lock-javadoc-tag-matcher (end)
  "Font lock matcher for javadoc tags.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   (eval-when-compile
     (concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
	     "\\(@[" jdee-font-lock-letter-or-digit "]+\\)"))
   end))

(defconst jdee-font-lock-javadoc-tag-keyword
  '(jdee-font-lock-javadoc-tag-matcher
    2 jdee-font-lock-doc-tag-face t)
  "Font lock keyword for javadoc tags.")

(defun jdee-font-lock-javadoc-docroot-matcher (end)
  "Font lock matcher for javadoc @docRoot tags.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   "{\\(@docRoot\\)}"
   end))

(defconst jdee-font-lock-javadoc-docroot-keyword
  '(jdee-font-lock-javadoc-docroot-matcher
    1 jdee-font-lock-doc-tag-face t)
  "Font lock keyword for javadoc @docRoot tags.")

(defun jdee-font-lock-javadoc-link-matcher (end)
  "Font lock matcher for javadoc @link tags.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   "{\\(@link\\)\\>[ \t]+\\([^}]*\\)}"
   end))

(defconst jdee-font-lock-javadoc-link-keyword
  '(jdee-font-lock-javadoc-link-matcher
    (1 jdee-font-lock-doc-tag-face t)
    (2 jdee-font-lock-link-face t))
  "Font lock keyword for javadoc @link tags.")

(defun jdee-font-lock-javadoc-see-ref-matcher (end)
  "Font lock matcher for javadoc @see references.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   (eval-when-compile
     (concat "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*"
	     "@see\\>[ \t]*"
	     "\\([.#" jdee-font-lock-letter-or-digit "]+\\)"))
   end))

(defconst jdee-font-lock-javadoc-see-ref-keyword
  '(jdee-font-lock-javadoc-see-ref-matcher
    2 jdee-font-lock-code-face t)
  "Font lock keyword for javadoc @see references.")

(defun jdee-font-lock-javadoc-param-name-matcher (end)
  "Font lock matcher for javadoc @param names.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*@param\\>[ \t]*\\(\\sw+\\)?"
   end))

(defconst jdee-font-lock-javadoc-param-name-keyword
  '(jdee-font-lock-javadoc-param-name-matcher
    2 font-lock-variable-name-face prepend t)
  "Font lock keyword for javadoc @param names.")

(defun jdee-font-lock-javadoc-exception-type-matcher (end)
  "Font lock matcher for javadoc exception types.
Limit search to END position."
  (jdee-font-lock-search-in-javadoc
   "^[ \t]*\\(/\\*\\*\\|\\*?\\)[ \t]*\
@\\(exception\\|throws\\)\\>[ \t]*\\(\\S-+\\)?"
   end))

(defconst jdee-font-lock-javadoc-exception-type-keyword
  '(jdee-font-lock-javadoc-exception-type-matcher
    3 font-lock-type-face prepend t)
  "Font lock keyword for javadoc exception types.")


;;;;
;;;; Support for fontification of user's defined names.
;;;;

(defcustom jdee-font-lock-api-file
  (expand-file-name "~/jdee-font-lock.api")
  "*File which contains a list of user's defined names to fontify.
If nil no name fontification occurs.  Otherwise the specified file must
contain one name by line.  Lines not beginning with a letter are
ignored.  When you change this file or modify its content a new cache
of font lock regular expressions will be rebuilt when restarting
Emacs.  Also, you can manually rebuild the cache and update font lock
keywords by entering the command:

\\[universal-argument] \\[jdee-font-lock-setup-keywords]."
  :group 'jdee-project
  :type '(choice :tag "Names"
		 (const :tag "No" nil)
		 (file  :tag "In file" :format "%t\n%v")))

(defcustom jdee-font-lock-api-name-filter nil
  "*Function used to filter a name."
  :group 'jdee-project
  :type 'function)

(defconst jdee-font-lock-api-entry-regexp
  (eval-when-compile
    (concat "^[" jdee-font-lock-letter "]"
	    "[" jdee-font-lock-letter-or-digit "]+$"))
  "Regexp to match a valid entry in `jdee-font-lock-api-file'.")

(defconst jdee-font-lock-api-entry-match 0
  "Index of the match data in `jdee-font-lock-api-entry-regexp'.")

(defun jdee-font-lock-api-names (&optional filter)
  "Return the list of names in `jdee-font-lock-api-file'.
If optional FILTER function is non-nil it is called for each name
found and must return non-nil to include it in the result list."
  (let (k kl)
    (if (and jdee-font-lock-api-file
	     (file-readable-p jdee-font-lock-api-file))
	(with-temp-buffer
	  (erase-buffer)
	  (insert-file-contents jdee-font-lock-api-file)
	  (goto-char (point-min))
	  (while (re-search-forward jdee-font-lock-api-entry-regexp nil t)
	    (setq k (match-string jdee-font-lock-api-entry-match))
	    ;; Allow filtering of names
	    (if (or (null filter) (funcall filter k))
		(setq kl (cons k kl))))))
    kl))

(defun jdee-font-lock-api-split-list (l n)
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

(defun jdee-font-lock-api-build-regexps (max-matches)
  "Build regular expressions matching names to fontify.
MAX-MATCHES is the maximum number of names that one regular expression
will match.  If MAX-MATCHES is less than 1 one regular expression will
match all the names."
  (let ((max-specpdl-size 2000)) ;; Prevent errors in `regexp-opt'
				 ;; when processing long string listes
    (mapcar (function
	     (lambda (k)
	       (concat "\\<" (regexp-opt k t) "\\>")))
	    (jdee-font-lock-api-split-list
	     (jdee-font-lock-api-names
	      jdee-font-lock-api-name-filter)
	     max-matches))))

(defvar jdee-font-lock-api-cache nil
  "Cache of regular expressions matching names to fontify..")

(defun jdee-font-lock-api-cache-file ()
  "Return the filename of the regular expressions cache.
There is a different cache file for each major version of (X)Emacs
because of incompatible regular expressions returned by `regexp-opt'."
  (and jdee-font-lock-api-file
       (format "%s.emacs-%d.apicache"
	       jdee-font-lock-api-file
	       emacs-major-version)))

(defconst jdee-font-lock-api-cache-file-header
  ";;; Regular expressions matching names to fontify.
;;; Automatically generated by `jdee-font-lock' on %s.
"
  "Header to be written into the cache file.")

(defun jdee-font-lock-api-regexps (&optional rebuild)
  "Return regular expressions matching names to fontify.
The list is cached in variable `jdee-font-lock-api-cache'.  If it
is nil try to initialize it from the cache file (see function
`jdee-font-lock-api-cache-file').  If optional REBUILD flag is
non-nil or there is no cache file or the cache file is older than the
names file (see variable `jdee-font-lock-api-file'), a new cache
is created."
  (let ((cache (jdee-font-lock-api-cache-file)))
    (cond

     ;; Inconditionnal rebuild
     (rebuild
      ;; Clear the cache to rebuild
      (setq jdee-font-lock-api-cache nil))

     ;; No names file exists
     ((null cache)
      ;; Clear the cache (no fontification)
      (setq jdee-font-lock-api-cache nil))

     ;; A cache file exists
     ((file-readable-p cache)
      (if (file-newer-than-file-p jdee-font-lock-api-file cache)
	  (progn
	    (message
	     "jdee-font-lock: names file %s newer than cache file %s"
	     jdee-font-lock-api-file cache)
	    ;; The api file has been modified since the cache was
	    ;; created, so clear the cache to rebuild
	    (setq jdee-font-lock-api-cache nil))
	;; Try to load the existing cache if needed
	(or jdee-font-lock-api-cache
	    (condition-case nil
		(load-file cache)
	      ;; If load fails clear the cache to rebuild
	      (error
	       (setq jdee-font-lock-api-cache nil)))))))

    (or jdee-font-lock-api-cache
	(not cache)
	;; Build a new cache if it is empty and available
	(progn
	  (message "jdee-font-lock: building names cache...")
	  (when (setq jdee-font-lock-api-cache
		      (jdee-font-lock-api-build-regexps
		       jdee-font-lock-max-names-by-regexp))
	    ;; Save regexps in cache
	    (with-current-buffer (find-file-noselect cache)
	      (erase-buffer)
	      (insert
	       (format jdee-font-lock-api-cache-file-header
		       (current-time-string))
	       (format "(setq jdee-font-lock-api-cache '%S)"
		       jdee-font-lock-api-cache))
	      (save-buffer)
	      (kill-buffer (current-buffer))))
	  (message "jdee-font-lock: building names cache...%s"
		   (if jdee-font-lock-api-cache "done" "empty"))))
	  jdee-font-lock-api-cache))

(defun jdee-font-lock-api-keywords (&optional rebuild)
  "Return a list of font lock keywords for user's defined names.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (mapcar (function
	   (lambda (k)
	     (cons k 'jdee-font-lock-api-face)))
	  (jdee-font-lock-api-regexps rebuild)))

;;;;
;;;; Font lock setup.
;;;;

(defvar java-font-lock-keywords-4 nil
  "Extra level fontification keywords for JDE mode.")

(defun jdee-font-lock-refontify ()
  "Re-fontify buffers in `java-mode' and `jdee-mode'."
  (dolist (b (buffer-list))
    (when (buffer-live-p b)
      (with-current-buffer b
	(when (and font-lock-mode
		   (memq major-mode '(java-mode jdee-mode)))
	  (message "JDEE refontify buffer %s..." b)
	  (font-lock-mode -1)
	  (font-lock-mode 1)
	  (message "JDEE refontify  buffer %s...done" b))))))

(defun jdee-font-lock-keywords (&optional rebuild)
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
	       (jdee-font-lock-api-keywords rebuild))
       (list
	;; Fontify modifiers.
	`(,(c-make-font-lock-search-function
	    jdee-font-lock-modifier-regexp
	    '(0 jdee-font-lock-modifier-face t)))
	`(,(c-make-font-lock-search-function
	    "\\<\\(false\\|null\\|true\\)\\>"
	    '(1 jdee-font-lock-constant-face t)))
	;; Fontify default and assert as keywords
	`(,(c-make-font-lock-search-function
	    "\\<\\(default\\|assert\\)\\>"
	    '(1 font-lock-keyword-face t)))
	;; Fontify const and goto with warning face. These keywords are
	;; reserved, even though they are not currently used.
	`(,(c-make-font-lock-search-function
	    "\\<\\(const\\|goto\\)\\>"
	    '(1 font-lock-warning-face t)))
	;; package and imports
	`(,(c-make-font-lock-search-function
	    "\\<\\(package\\|import\\(?:\\s-+static\\)?\\)\\s-+\\(\\(?:[a-z_$*][a-zA-Z0-9_$]*\\.?\\)*\\)"
	    '(1 font-lock-keyword-face t)
	    '(2 jdee-font-lock-package-face t)))
	;; constructor
	`(,(c-make-font-lock-search-function
	    (concat
	     "^\\s-*\\<\\(?:public\\|private\\|protected\\)\\>?\\s-*"
	     "\\([" jdee-font-lock-capital-letter "]\\sw*\\)"
	     "(.*?)")
	    '(1 jdee-font-lock-constructor-face t)))
	;; class names
	`(,(c-make-font-lock-search-function
	    "\\<\\(new\\|instanceof\\)\\>[ \t]+\\(\\sw+\\)"
	    '(2 font-lock-type-face t)))

	;; modifier protections
        `(,(c-make-font-lock-search-function
	    "\\<\\(private\\)\\>"
	    '(1 jdee-font-lock-private-face t)))
	`(,(c-make-font-lock-search-function
	    "\\<\\(protected\\)\\>"
	    '(1 jdee-font-lock-protected-face t)))
	`(,(c-make-font-lock-search-function
	    "\\<\\(public\\)\\>"
	    '(1 jdee-font-lock-public-face t)))
	;; Fontify numbers
	`(,(c-make-font-lock-search-function
	    jdee-font-lock-number-regexp
	    '(0 jdee-font-lock-number-face t)))
	;; Fontify operators
;;;     `(,(c-make-font-lock-search-function
;;;         jdee-font-lock-operator-regexp
;;;         '(0 jdee-font-lock-operator-face t)))
	;; Fontify capitalised identifiers as constant
	`(,jdee-font-lock-capital-id-regexp
	  1 jdee-font-lock-constant-face)
	;; Fontify text between `' in comments
	jdee-font-lock-quote-keyword
	;; Fontify javadoc comments
	'((lambda (limit)
	    (c-font-lock-doc-comments
		"/\\*\\*" limit
	      jdee-font-lock-doc-comments)))
	)
       )

    ;; Through JDEE's own support
    (append
     ;; Feature scoping: These must come first or the Special
     ;; constants, Modifiers and Packages from keywords-1 will catch
     ;; them.
     (list
      ;; Fontify default as keyword
      '("\\<\\(default\\)\\>" (1 font-lock-keyword-face))
      ;; Fontify assert as keyword
      '("\\<\\(assert\\)\\>" (1 font-lock-keyword-face))
      ;; Fontify const and goto with warning face. These keywords are
      ;; reserved, even though they are not currently used.
      '("\\<\\(const\\|goto\\)\\>" (1 font-lock-warning-face))
      ;; Fontify modifiers.
      (cons jdee-font-lock-modifier-regexp
	    'jdee-font-lock-modifier-face)
      ;; Fontify package directives
      '("\\<\\(package\\)\\>\\s-+\\(\\sw+\\)"
	(1 font-lock-keyword-face)
	(2 jdee-font-lock-package-face nil t)
	("\\=\\.\\(\\sw+\\)" nil nil
	 (1 jdee-font-lock-package-face nil t)))
      ;; Fontify import directives
      '("\\<\\(import\\)\\>\\s-+\\(\\sw+\\)"
	(1 font-lock-keyword-face)
	(2 (if (equal (char-after (match-end 0)) ?\.)
	       jdee-font-lock-package-face
	     font-lock-type-face))
	("\\=\\.\\(\\*\\|\\sw+\\)" nil nil
	 (1 (if (equal (char-after (match-end 0)) ?\.)
		jdee-font-lock-package-face
	      (if (equal (char-before (match-end 0)) ?\*)
		  jdee-font-lock-number-face
		font-lock-type-face)))))
      ;; modifier protections
      '("\\<\\(private\\)\\>" (1 jdee-font-lock-private-face))
      '("\\<\\(protected\\)\\>" (1 jdee-font-lock-protected-face))
      '("\\<\\(public\\)\\>" (1 jdee-font-lock-public-face))
      )
     ;; Fontify user's defined names
     (jdee-font-lock-api-keywords rebuild)

     ;; Remove existing javadoc font lock keywords from GNU Emacs
     ;; `java-font-lock-keywords-3'
     (jdee-font-lock-remove-javadoc-keywords
      java-font-lock-keywords-3)

     ;; GNU Emacs don't fontify capitalized types so do it
     (list
      (list
       (concat "\\<\\([" jdee-font-lock-capital-letter "]\\sw*\\)\\>"
               "\\([ \t]*\\[[ \t]*\\]\\)*"
               "\\([ \t]*\\sw\\)")
       '(font-lock-match-c-style-declaration-item-and-skip-to-next
         (goto-char (match-beginning 3))
         (goto-char (match-beginning 3))
         (1 (if (match-beginning 2)
                font-lock-function-name-face
              font-lock-variable-name-face))))
      (cons
       (concat "\\<\\([" jdee-font-lock-capital-letter "]\\sw*\\)\\>"
               "\\([ \t]*\\[[ \t]*\\]\\)*"
               "\\([ \t]*\\sw\\)")
       '(1 font-lock-type-face))
      '("\\<\\(new\\|instanceof\\)\\>[ \t]+\\(\\sw+\\)"
        2 font-lock-type-face))

     ;; Some extra fontification
     (list
      ;; Fontify numbers
      (cons jdee-font-lock-number-regexp
	    'jdee-font-lock-number-face)
      ;; Fontify operators
;;;      (cons jdee-font-lock-operator-regexp
;;;            'jdee-font-lock-operator-face)
      ;; Fontify capitalised identifiers as constant
      (cons jdee-font-lock-capital-id-regexp
	    '(1 jdee-font-lock-constant-face))
      ;; Fontify text between `' in comments
      jdee-font-lock-quote-keyword
      ;; Fontify javadoc tags (including non official ones)
      jdee-font-lock-javadoc-tag-keyword
      ;; Fontify @param variable name
      jdee-font-lock-javadoc-param-name-keyword
      ;; Fontify @exception or @throws exception type
      jdee-font-lock-javadoc-exception-type-keyword
      ;; Fontify @docRoot
      jdee-font-lock-javadoc-docroot-keyword
      ;; Fontify @link
      jdee-font-lock-javadoc-link-keyword
      ;; Fontify @see reference
      jdee-font-lock-javadoc-see-ref-keyword
      ;; Fontify the text of a HREF anchor
      jdee-font-lock-html-ahref-keyword
      )
     ;; Fontify other HTML tags
     jdee-font-lock-html-keywords
     )
    ))

;;;###autoload
(defun jdee-font-lock-setup-keywords (&optional rebuild)
  "Setup font lock keywords in `java-font-lock-keywords-4'.
If optional REBUILD flag is non-nil create a new cache of regular
expressions."
  (interactive "P")
  (and (called-interactively-p 'interactive)
       (consp current-prefix-arg)
       (setq rebuild t))

  ;; Setup the JDEE's extra font lock keywords.
  (setq java-font-lock-keywords-4 (jdee-font-lock-keywords rebuild))

  ;; Update fontification of buffers in `java-mode' and `jdee-mode'.
  (when (called-interactively-p 'interactive)
    (jdee-font-lock-refontify)))

;; Setup `java-font-lock-keywords-4'
(jdee-font-lock-setup-keywords)

;; Define new defaults for Font Lock mode
(defconst jdee-font-lock-defaults
  (let ((java-defaults
         ;;; To avoid compilation warning, replace obsolete variable,
         ;;; font-lock-defaults-alist, with its value for java-mode.
         ;;; (cdr (assq 'java-mode font-lock-defaults-alist)))))
         ;;; Paul Kinnucan.
         '((java-font-lock-keywords
            java-font-lock-keywords-1
            java-font-lock-keywords-2
            java-font-lock-keywords-3)
           nil nil ((?_ . "w") (?$ . "w") (?@ . "w")) nil
           (font-lock-mark-block-function . mark-defun))))
    (cons (append (car java-defaults) '(java-font-lock-keywords-4))
	  (cdr java-defaults)))
  "Defaults for coloring Java keywords in jdee-mode. The defaults
consist of the java-mode defaults plus `java-font-lock-keywords-4'.")

;;;;
;;;; Mode hook to setup syntax coloring.
;;;;

(defun jdee-setup-syntax-coloring ()
  "Mode hook to setup syntax coloring in `java-mode' and `jdee-mode'.
When `jdee-use-font-lock' is non-nil syntax coloring is always turned
on and uses `java-font-lock-keywords-4' extra level of fontification.
If `jdee-use-font-lock' is nil syntax coloring rules are those of
standard `java-mode'."
  ;; If `jdee-use-font-lock' is non-nil setup
  ;; `java-font-lock-keywords-4' extra level of fontification.
  (when jdee-use-font-lock
    ;; Setup `font-lock-defaults'
    (set (make-local-variable 'font-lock-defaults)
	 jdee-font-lock-defaults)
    ;; Use the maximum decoration available
    (set (make-local-variable 'font-lock-maximum-decoration) t)
    ;; Handle multiline
    (set (make-local-variable 'font-lock-multiline) t)
    )
  ;; Turn on Font Lock Mode as needed (based on parent `java-mode'
  ;; if `jdee-use-font-lock' is nil).
  (let ((major-mode (if jdee-use-font-lock major-mode 'java-mode)))
    (if global-font-lock-mode
        (if (fboundp 'turn-on-font-lock-if-enabled)
            (turn-on-font-lock-if-enabled)
          (turn-on-font-lock-if-desired))))
  ;; Always turn on font locking if `jdee-use-font-lock' is non-nil.
  (if jdee-use-font-lock
      (turn-on-font-lock)))

;; Java has a different font for comments than Emacs Lisp, but by default,
;; `jdee-font-lock-javadoc-face' inherits from `font-lock-doc-face', which
;; is the mapping for `c-doc-face-name' Emacs 22 and up
(if (> emacs-major-version 23)
    (defconst c-doc-face-name 'jdee-font-lock-javadoc-face)
  ;; starting with 24, cc-fonts clobbers this because of some change of order
  ;; of loading
  (eval-after-load
      "cc-fonts"
    '(defconst c-doc-face-name 'jdee-font-lock-javadoc-face)))

;; By default, enable extra fontification in `jdee-mode'.
(add-hook 'java-mode-hook #'jdee-setup-syntax-coloring)

(provide 'jdee-font-lock)

;;; jdee-font-lock.el ends here
