;;; jde-properties.el --- Java properties major mode

;; Copyright (C) 2003-2010  Paul Landes

;; Maintainer: Paul Landes landes at mailc dot net
;; Keywords: Java properties JDEE

;; This file is part of Emacs.

;; Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; Provides a major mode for Sun's java.util.Properties file format.
;; Includes typical major mode stuff (font lock), pagination, and file
;; format validation.  Ant style properties and class names
;; fontification is supported.
;;
;; Paragraphs are comment/property group blocks with no space in
;; between the comments and the property lines.
;;
;; Currently, only regular expressions are used for fontification and
;; because the properties format can span multiple lines (with ending
;; escaped back slash (\)) multi-line fontification doesn't work.  In
;; these cases, use `font-lock-fontify-buffer' to re-fontify
;; everything.  When I have the time, I will go back and fix this,
;; but if someone wants to fix it...(hint, hint :).

;;; Code:

(defgroup jde-java-properties-font-lock-faces nil
  "JDE Autocoder"
  :group 'jde
  :prefix "jde-java-properties-font-lock-")

;; face definitions
(defface jde-java-properties-font-lock-key-face
  '((t (:foreground "darkcyan")))
  "Font Lock mode face used to highlight keys."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-value-face
  '((t (:foreground "forest green")))
  "Font Lock mode face used to highlight values."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-equal-face
  '((t (:foreground "darkorange")))
  "Font Lock mode face used to highlight equals."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-substitution-face
  '((t (:foreground "blue3")))
  "Font Lock mode face used to highlight substitutions."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-class-name-face
  '((t (:foreground "purple")))
  "Font Lock mode face used to highlight class names."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-backslash-face
  '((t (:foreground "red")))
  "Font Lock mode face used to highlight backslashes."
  :group 'jde-java-properties-font-lock-faces)
(defface jde-java-properties-font-lock-comment-face
  '((t (:foreground "red3")))
  "Font Lock mode face used to highlight comments."
  :group 'jde-java-properties-font-lock-faces)

;; font variables
(defvar jde-java-properties-font-lock-key-face
  'jde-java-properties-font-lock-key-face
  "Face name to use for keys.")
(defvar jde-java-properties-font-lock-value-face
  'jde-java-properties-font-lock-value-face
  "Face name to use for values.")
(defvar jde-java-properties-font-lock-equal-face
  'jde-java-properties-font-lock-equal-face
  "Face name to use for equals.")
(defvar jde-java-properties-font-lock-substitution-face
  'jde-java-properties-font-lock-substitution-face
  "Face name to use for substitutions.")
(defvar jde-java-properties-font-lock-class-name-face
  'jde-java-properties-font-lock-class-name-face
  "Face name to use for class names.")
(defvar jde-java-properties-font-lock-backslash-face
  'jde-java-properties-font-lock-backslash-face
  "Face name to use for backslashes.")
(defvar jde-java-properties-font-lock-comment-face
  'jde-java-properties-font-lock-comment-face
  "Face name to use for comments.")


;;;###autoload
(defun jde-java-properties-mode ()
  "Major mode for Java properties files buffer."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'jde-java-properties-mode
        mode-name "Java-Props")

  (set (make-local-variable 'paragraph-start) (concat "$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'paragraph-ignore-fill-prefix) t)
  (set (make-local-variable 'fill-prefix) "# ")
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-start-skip) "\\(^\\|\\s-\\)#+ *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-column) 32)

  (if (not (featurep 'xemacs))
      (set (make-local-variable 'font-lock-defaults)
	   '(jde-java-properties-font-lock-keywords t)))

  (use-local-map jde-java-properties-mode-map)
  (font-lock-mode t)
  (run-hooks 'jde-java-properties-mode-hook))


(defvar jde-java-properties-mode-map nil
  "Keymap for Java-Properties mode.")

(if jde-java-properties-mode-map
    nil
  (setq jde-java-properties-mode-map (make-keymap)))


(defvar jde-java-properties-font-lock-keywords
  '(
    ;; property keys
    ("^\\([^\n]+\\)[ \t]*=" 1 jde-java-properties-font-lock-key-face t)
    ;; property values
    ;;
    ;; Property values are tricky: we must allow for \ escaping multiple
    ;; lines.  This is not the ideal implementation since line by line doesn't
    ;; work here, functions need to be implemented and used
    ("^[\t ]*\\([^\n\t=]+?\\)\\\\$" 1
     jde-java-properties-font-lock-value-face t)
    ("^[^\n]+?\\(=\\).*\\\\?$" 1 jde-java-properties-font-lock-equal-face t)
    ("^[^\n]+?=\\(.*\\)\\\\?$" 1 jde-java-properties-font-lock-value-face t)
    ("\\(\\\\\n.+\\)\\\\$" 1 jde-java-properties-font-lock-value-face t)
    ("\\(\\\\\n[^\\\\\n]+\\)$" 1 jde-java-properties-font-lock-value-face t)
    ;; Jakarata Ant like substitution properties
    ("\\(\\${[^}]+?}\\)" 1 jde-java-properties-font-lock-substitution-face t)
    ;; Java fully qualified class names
    ("^.*=.*?\\([a-z][0-9a-z.-]*\\.[A-Z][0-9A-Za-z-]*\\)" 1
     jde-java-properties-font-lock-class-name-face t)
    ;; make the ending slashes nice and visible
    ("\\(\\\\\\)$" 1 jde-java-properties-font-lock-backslash-face t)
    ;; comments
    ("^[\n\t ]*\\(#.*\\)$" 1
     jde-java-properties-font-lock-comment-face t)
    )
  "Additional expressions to highlight in Java-Props mode.")
(if (featurep 'xemacs)
    (put 'jde-java-properties-mode 'font-lock-defaults
	 '(jde-java-properties-font-lock-keywords t)))

(defun jde-java-properties-parse (&optional buffer)
  "Parse a file of Java properties and return them as an alist.
BUFFER is the buffer to get the properties and defaults the current buffer."
  (let (prop-alist)
    (save-excursion
      (if buffer (set-buffer buffer))
      (save-match-data
	(beginning-of-buffer)
	(while (re-search-forward "^\\(.*?\\)=\\(.*\\)$" nil t)
	  (let ((key (match-string 1))
		(val (match-string 2)))
	    (set-text-properties 0 (length key) nil key)
	    (set-text-properties 0 (length val) nil val)
	    (setq prop-alist (append prop-alist (list (cons key val))))
	    ))))
    prop-alist))

;;;###autoload
(defun jde-java-properties-validate (&optional goto)
  "Determine whether or not the properties file conforms to the Sun
java.util.Properties format.  Only the first invalid line is found.

GOTO indicates whether or not to put the point at the first invalid line
found."
  (interactive "P")
  (let (dangle-pos)
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward "\\(\\\\\\)\n[\t ]*$" nil t)
            (setq dangle-pos (match-beginning 1)))

        (if (not dangle-pos)
            (message "Properties file is valid")
          (goto-char dangle-pos)
          (message (format "Found dangling continuation on line %d"
                           (line-number)
                           )))))
    (if goto (goto-char dangle-pos))
    dangle-pos))

(provide 'jde-properties)

;;; jde-properties.el ends here
