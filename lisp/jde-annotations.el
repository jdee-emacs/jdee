;;; jde-annotations.el --- Indentation for Java 5 annotations.

;; Copyright (C) 2006 by Suraj Acharya

;; Author: Suraj Acharya <sacharya@cs.indiana.edu>
;; Maintainer: Suraj Acharya <sacharya@cs.indiana.edu>
;; Created: 22 Feb 2006
;; Keywords: cc-mode java annotations indentation

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
;; This library tries to indent java annotations
;; (http://java.sun.com/j2se/1.5.0/docs/guide/language/annotations.html)
;; like the code examples listed in the webpage.

;;; Code:

(eval-when-compile
  (require 'cc-mode))

(defun c-preprend-offset (symbol offset)
  "Find the offset entry for SYMBOL and add OFFSET at the front of the list.
See `c-set-offset' for a description of OFFSET and SYMBOL."
  (let ((old-offset (cdr-safe (or (assq symbol c-offsets-alist)
                                  (assq symbol (get 'c-offsets-alist
                                                      'c-stylevar-fallback))))))
    (if old-offset
        (if (listp old-offset)
            (c-set-offset symbol (cons offset old-offset))
          (c-set-offset symbol (list offset old-offset)))
      (c-set-offset symbol offset))))

(defun jde-annotations-setup ()
  "Set up java mode indent function to handle java 1.5 annotations.
The setup function adds one of the custom indentation functions
`c-single-indent-after-java-annotations' or `c-no-indent-after-java-annotations' 
to the offset lists of the symbols `arglist-intro', `topmost-intro-cont', `arglist-intro',
`arglist-close', `statement-cont' and `func-decl-cont'. 
This function should be called after any calls to `c-set-style'."
  (c-preprend-offset 'arglist-intro 'c-single-indent-after-java-annotations)
  (c-preprend-offset 'topmost-intro-cont 'c-no-indent-after-java-annotations)
  (c-preprend-offset 'arglist-close 'c-no-indent-after-java-annotations)
  (c-preprend-offset 'statement-cont 'c-no-indent-after-java-annotations)
  (c-preprend-offset 'func-decl-cont 'c-no-indent-after-java-annotations))

(defun c-only-java-annotations-p (langelem)
  "Check if there are only java annotations before the current line.
It does this by moving across the region from the start of
LANGELEM to the beginning of this line one sexp at a time.  If
during this traversal, this function only sees whitespaces
followed by either a '@' or a '(' then it returns t."
  (save-excursion
    (condition-case err ;; return nil if  any errors are thrown by forward-sexp
        (let* ((lim (1- (c-point 'bol)))
               (throws (catch 'notAnno
		     (goto-char (cdr langelem))
		     (while (< (point) lim)
                       (if (not (looking-at "\\(\\s \\|\n\\)*\\(@\\|(\\)"))
			   (throw 'notAnno t))
                       (forward-sexp 1)))))
          (if (not throws)
              t)))))

(defun c-no-indent-after-java-annotations (langelem)
  "If preceeded by java annotations, indent this line the same as the previous.
Works with topmost-intro-cont, statement-cont, arglist-close and func-decl-cont.

With topmost-intro-cont, indents as
@Id
Long foo;

instead of
@Id
<-->Long foo;
Also for method and class declarations instead of the field foo.
With statement-cont, indents local variables with annotations as above.

With statement-cont, indents as
@Id(
    arg=\"value\"
)
Long foo;

instead of
@Id(
    arg=\"value\"
)
<-->Long foo;


With arglist-close, indents as
@RequestForEnhancement(
...
)

Instead of
@RequestForEnhancement(
...
<-->)

Argument LANGELEM The language element being indented."
  (if (c-only-java-annotations-p langelem)
      0))


(defun c-single-indent-after-java-annotations (langelem)
  "If preceeded by java annotations, indent this line more than previous.
This function indents this line by `c-basic-offset' columns more
than the previous line.

Works with arglist-intro.

Indents as
@RequestForEnhancement(
    id	     = 2868724,
...

instead of
@RequestForEnhancement(
                       id	     = 2868724,
...

Argument LANGELEM The language element being indented."
    (if (c-only-java-annotations-p langelem)
      c-basic-offset))

(provide 'jde-annotations)

;;; jde-annotations.el ends here
