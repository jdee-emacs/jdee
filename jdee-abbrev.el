;;; jdee-abbrev.el -- Java code templates based on abbrev-mode

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

(defcustom jdee-enable-abbrev-mode nil
  "Enable expansion of abbreviations in jdee-mode.
See `jdee-mode-abbreviations' for more information."
  :group 'jdee-project
  :type 'boolean
  :set '(lambda (sym val)
          (set-default sym val)
          (if (featurep 'jdee) ;; skip initial set.
              (mapc
               (lambda (buf)
                 (with-current-buffer buf
                   (setq abbrev-mode val)
                   (when abbrev-mode
                     (setq local-abbrev-table (make-abbrev-table))
                     (jdee-init-abbrev-table))))
               (jdee-get-project-source-buffers)))))

(defcustom jdee-mode-abbreviations
  (list
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "thro" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
  "Abbreviations used for Java keywords.
To use these abbreviations, you must enable `abbrev-mode' (see
`jdee-enable-abbrev-mode').  To use an abbreviation, enter the
abbreviation followed by a white-space character.  To suppress
expansion, enter C-q white-space."
  :group 'jdee-project
  :type '(repeat
          (cons :tag "jdee-mode abbreviation"
                (string :tag "Abbreviation")
                (string :tag "Expansion")))
  :set '(lambda (sym val)
          (set-default sym val)
          (if (and
               (featurep 'jdee)
               jdee-enable-abbrev-mode)
              (progn
                (mapc
                 (lambda (buf)
                   (with-current-buffer buf
                     (setq local-abbrev-table (make-abbrev-table))
                     (jdee-init-abbrev-table)))
                 (jdee-get-project-source-buffers))))))

(defun jdee-init-abbrev-table ()
  "Load the abbrev table.
Load it with a set of abbrevs that invoke an anonymous function that
does the expansion only if point is not in a quoted string or a
comment."

  ;; Note the use of lexical-let - must have the common lisp packages
  ;; around, since the anonymous function needs the closure provided by
  ;; lexical-let.
  (interactive)
  (mapc
   (lambda (x)
     (lexical-let
         ((abbrev (car x))     ; this is the abbrev, lexically scoped
          (expansion (cdr x))) ; this is the expansion
       (define-abbrev
         local-abbrev-table
         abbrev
         t
         (lambda ()
           (unless (jdee-parse-comment-or-quoted-p)
             (delete-char (- (length abbrev))) ; remove abbreviation and
             (insert expansion)))                   ; insert expansion
         0)))
   jdee-mode-abbreviations)

  (if jdee-gen-cflow-enable
      (jdee-gen-load-abbrev-templates))

  (setq abbrevs-changed nil))

;; The next two functions contributed by s.nicolas@videotron.ca
(defun jdee-abbrev-mode ()
  "Toggle abbreviation mode in JDEE without altering project settings.
See `jdee-mode-abbreviations' for more information."
  (interactive)
  (setq jdee-enable-abbrev-mode (not jdee-enable-abbrev-mode))
  (setq abbrev-mode jdee-enable-abbrev-mode)
  (when jdee-enable-abbrev-mode
    ;; Define abbreviations.
    (jdee-init-abbrev-table))
  (if jdee-enable-abbrev-mode
      (message "abbreviation mode on")
    (message "abbreviation mode off")))

(defun jdee-show-abbrevs ()
  "Show a popup menu containing all available expansions.
See `jdee-mode-abbreviations' for more information."
  (interactive)
  (when (fboundp 'imenu--mouse-menu)
    ;; not all users want imenu loaded
    (let* ((expansions
            (mapcar
             (lambda(x) (cons (cdr x) (car x)))
             jdee-mode-abbreviations))
           (expansion (car (imenu--mouse-menu expansions t "Abbreviations"))))
      (insert expansion))))

(provide 'jdee-abbrev)

;;; jdee-abbrev.el ends here
