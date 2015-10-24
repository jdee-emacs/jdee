;;; jdee-util.el -- JDE utilities.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2001-2006 Paul Kinnucan.
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

;; This package provides useful macros, functions, and classes
;; required by multiple JDE packages. You should not put any macros,
;; functions, or classes in this package that require other
;; JDE packages.
;;
;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jdee.sourceforge.net>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@comcast.net.

;;; Code:

(require 'cl-lib)
(require 'efc)

;; FIXME: refactor
(defvar jdee-buffer-project-file);; jdee-project-file.el
(declare-function jdee-find-class-source-file "jdee-open-source" (class))

(require 'arc-mode)

(defsubst jdee-line-beginning-position (&optional n)
  (line-beginning-position n))

(defsubst jdee-line-end-position (&optional n)
  (line-end-position))

;;;###autoload
(defun jdee-require (feature)
  "Require FEATURE, either pre-installed or from the distribution.
 That is, first try to load the FEATURE library. Then try to load the
 jdee-FEATURE library from the JDEE's distribution.
 Signal an error if FEATURE can't be found."
  (condition-case nil
      ;; If the library if available, use it.
      (require feature)
    (error
     ;; Try to use the one from the JDEE's distribution.
     (require feature (format "jdee-%s" feature)))))

(defmacro jdee-assert-source-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert  (eq major-mode 'jdee-mode) nil
            "This command works only in a Java source or debug buffer."))


;; Provided for XEmacs compatibility.
(if (not (fboundp 'subst-char-in-string))
    (defun subst-char-in-string (fromchar tochar string &optional inplace)
      "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
      (let ((i (length string))
	    (newstr (if inplace string (copy-sequence string))))
	(while (> i 0)
	  (setq i (1- i))
	  (if (eq (aref newstr i) fromchar)
	      (aset newstr i tochar)))
	newstr)))

(defun jdee-replace-in-string  (string regexp newtext &optional literal)
  "Replace REGEXP with NEWTEXT in STRING. see: `replace-match'"
  (if (string-match regexp string)
      (replace-match newtext nil literal string)
    string))


(defun jdee-get-line-at-point (&optional pos)
  "Get the number of the line at point."
  (let* ((point (or pos (point)))
	 (ln (if (= point 1)
		 1
	       (count-lines (point-min) point))))
    (save-excursion
      (goto-char point)
      (if (eq (char-before) ?\n)
	  (1+ ln)
	ln))))

(defun jdee-root ()
  "Return the path of the root directory of this JDEE
installation. The root directory is the parent of the
directory that contains the JDEE's Lisp files. On
Emacs and installations that use the JDEE distributable,
the root directory is the root directory that results
from unpacking the distributable."
  (let ((directory-sep-char ?/))
    (expand-file-name
     (file-name-directory (locate-library "jdee")))))

(defun jdee-find-jdee-data-directory ()
  "Return the path of the JDEE data directory.
Returns the path of the directory containing the JDEE java
and documentation directories;  nil if the directory cannot
be found. On all other Emacs versions, the JDEE expects
to find the documentation in the doc subdirectory of directory
that contains the JDEE lisp directory."
  (let ((directory-sep-char ?/))
    (jdee-root)))

(defun jdee-temp-directory ()
  "Get the location used by the host system to store temporary files."
  (or (if (boundp 'temporary-file-directory) temporary-file-directory)
      (if (fboundp 'temp-directory) (temp-directory)
	(error "no temp-directory function found"))))

;; FIXME: this checks that temp-directory is unbound, then calls it anyway!?
;; (if (member system-type '(cygwin32 cygwin))
;;     (jdee-cygwin-path-converter-cygpath (temp-directory))
;;   (temp-directory)))))

(defun jdee-get-java-source-buffers ()
  "Return a list of Java source buffers open in the current session."
  (delq
   nil
   (mapcar
    #'(lambda (buffer)
	(with-current-buffer buffer
	  (if (eq major-mode 'jdee-mode)
	      buffer)))
    (buffer-list))))

(defun jdee-get-project-source-buffers (&optional project-file)
  "Return a list of the Java source buffers belonging to the project
whose project file is PROJECT-FILE. If PROJECT-FILE is not specified,
this function returns the buffers belonging to the project in the
currently selected source buffer."
  (let ((proj-file
	 (or project-file
	     (if (boundp 'jdee-current-project)
		 jdee-current-project))))
    (delq
     nil
     (mapcar
      (lambda (buffer)
	(with-current-buffer buffer
          (if (equal jdee-buffer-project-file proj-file)
              buffer)))
      (jdee-get-java-source-buffers)))))

(defun jdee-get-visible-source-buffers ()
  "Return a list of visible Java source buffers."
  (delq nil (mapcar #'(lambda (buffer)
			(if (get-buffer-window buffer 'visible)
			    buffer))
		    (jdee-get-java-source-buffers))))

(defun jdee-get-selected-source-buffer ()
  (with-current-buffer (window-buffer (selected-window))
    (if (eq major-mode 'jdee-mode)
	(current-buffer))))

(defvar jdee-exception-goto-regexp
  "[ \t]+\\(?:at \\)?\\([a-zA-Z0-9.]+\\)\\(?:\\$?[a-zA-Z0-9]*\\)\\.\\([^(]+\\)([^:]+:\\([0-9]+\\))$"
  "*Regular expression used to find the file and line number of a frame in a
stack trace.")

;;;###autoload
(defun jdee-exception-goto ()
  "Go to the Java source file and line indicated by an exception stack trace."
  (interactive)
  (let ((regexp jdee-exception-goto-regexp)
	file line end)
    (save-match-data
      (save-excursion
	(end-of-line)
	(setq end (point))
	(beginning-of-line)
	(if (not (re-search-forward regexp end t))
	    (error (concat "Current line doesn't look "
			   "like an exception stack trace line")))
	(let ((full-class (match-string 1))
	      (method (match-string 2)))
	  (setq line (string-to-number (match-string 3)))
	  (setq file (jdee-find-class-source-file full-class))
	  (if (null file)
	      (error "Java source for class `%s' not found" full-class)))))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autolaod
(defalias 'jdee-goto-exception 'jdee-exception-goto)

(defcustom jdee-htmlize-code-destinations '("~/Desktop" "~/tmp")
  "*Directories to put the output of `jdee-htmlize-code'.
The function iterates through each and stops when it finds an existing
directory."
  :group 'jdee-project
  :type '(repeat directory))

;;;###autoload
(defun jdee-htmlize-code (start end &optional no-line-numbers-p)
  "Write the current code region as an HTML document.
Line numbers are added as well.

Requires ELPA package `htmlize'.

See `jdee-htmlize-code-destinations'."
  (interactive
   (append (if mark-active
	       (list (region-beginning) (region-end))
	     (list (point-min) (point-max)))
	   (list (not current-prefix-arg))))
  (unless (require 'htmlize nil t)
    (error "Requires ELPA package `htmlize'."))
  (save-restriction
    (narrow-to-region start end)
    (let ((code-buf (current-buffer))
	  (line-width (ceiling (log (count-lines (point-min) (point-max)) 10)))
	  (ln 0))
      (with-temp-buffer
	(insert-buffer-substring code-buf)
	(untabify (point-min) (point-max))
	(goto-char (point-min))
	(if (not no-line-numbers-p)
	    (while (not (eobp))
	      (beginning-of-line)
	      (insert (format (format "%%.%dd " line-width) (cl-incf ln)))
	      (forward-line)))
	(rename-buffer (concat (buffer-name code-buf) ".html"))
	(let ((buf (when (fboundp 'htmlize-buffer)
		     (htmlize-buffer)))
	      (bname (buffer-name)))
	  (unwind-protect
	      (with-current-buffer buf
		(set-visited-file-name
		 (dolist (dir jdee-htmlize-code-destinations)
		   (setq dir (expand-file-name dir))
		   (if (file-exists-p dir)
		       (cl-return (expand-file-name bname dir)))))
		(save-buffer)
		(if (featurep 'browse-url)
		    (browse-url (buffer-file-name))))
	    (if (buffer-live-p buf)
		(kill-buffer buf))))))))

(defun jdee-create-default-prompt (prompt default)
  "Format a prompt with optional default formatting."
  (format "%s%s"
	  prompt (if default
		     (format " (default %s): " default) ": ")))

(provide 'jdee-util)

;;; jdee-util.el ends here
