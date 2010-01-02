;;; jde-util.el -- JDE utilities.
;; $Id$

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

(require 'efc)

(if (featurep 'xemacs)
     (load "arc-mode")
  (require 'arc-mode))

(defsubst jde-line-beginning-position (&optional n)
  (if (featurep 'xemacs)
      (save-excursion (beginning-of-line n) (point))
    (line-beginning-position n)))

(defsubst jde-line-end-position (&optional n)
  (if (featurep 'xemacs)
      (save-excursion (end-of-line n) (point))
    (line-end-position)))

;;;###autoload
(defun jde-require (feature)
   "Require FEATURE, either pre-installed or from the distribution.
 That is, first try to load the FEATURE library. Then try to load the
 jde-FEATURE library from the JDEE's distribution.
 Signal an error if FEATURE can't be found."
   (condition-case nil
      ;; If the library if available, use it.
       (require feature)
     (error
      ;; Try to use the one from the JDEE's distribution.
      (require feature (format "jde-%s" feature)))))

(defmacro jde-assert-source-buffer ()
  "Asserts that the current buffer is a
Java source or a debug buffer."
  '(assert  (eq major-mode 'jde-mode) nil
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

(if (not (fboundp 'replace-in-string))
    (defun replace-in-string  (string regexp newtext &optional literal)
      "Replace REGEXP with NEWTEXT in STRING. see: `replace-match'"
      (if (string-match regexp string)
	  (replace-match newtext nil literal string)
	string)))


(defun jde-get-line-at-point (&optional pos)
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

(defun jde-root ()
  "Return the path of the root directory of this JDEE
installation. The root directory is the parent of the
directory that contains the JDEE's Lisp files. On
Emacs and on XEmacs installations that use the
JDEE distributable, the root directory is the root
directory that results from unpacking the distributable.
On installations based on the version of the JDEE
packaged with XEmacs, the root directory is
xemacs-packages/lisp."
  (let ((directory-sep-char ?/))
    (expand-file-name
     "../"
     (file-name-directory (locate-library "jde")))))

(defun jde-find-jde-data-directory ()
  "Return the path of the JDE data directory.
Returns the path of the directory containing the
JDE java and documentation directories;  nil if the
directory cannot be found. If XEmacs, returns the location of
the data directory in the XEmacs distribution hierarchy. On all other Emacs versions,
the JDE expects to find the documentation and Java class directories
in the same directory that contains the JDE lisp directory."
  (let ((directory-sep-char ?/))
    (if (featurep 'xemacs)
	(let ((dir (locate-data-directory "jde")))
	  (if dir dir (jde-root)))
      (jde-root))))

(defun jde-temp-directory ()
"Get the location used by the host system to store temporary files."
  (or (if (boundp 'temporary-file-directory) temporary-file-directory)
      (if (fboundp 'temp-directory) (temp-directory)
	(if (member system-type '(cygwin32 cygwin))
	    (jde-cygwin-path-converter-cygpath (temp-directory))
	  (temp-directory)))))

(defun jde-get-java-source-buffers ()
  "Return a list of Java source buffers open in the current session."
  (delq
   nil
   (mapcar
    #'(lambda (buffer)
	(with-current-buffer buffer
	  (if (eq major-mode 'jde-mode)
	      buffer)))
    (buffer-list))))

(defun jde-get-project-source-buffers (&optional project-file)
  "Return a list of the Java source buffers belonging to the project
whose project file is PROJECT-FILE. If PROJECT-FILE is not specified,
this function returns the buffers belonging to the project in the
currently selected source buffer."
  (let ((proj-file
	 (or project-file
	     (if (boundp 'jde-current-project)
		 jde-current-project))))
    (delq
     nil
     (mapcar
      (lambda (buffer)
	(with-current-buffer buffer
	 (if (equal jde-buffer-project-file proj-file)
	     buffer)))
      (jde-get-java-source-buffers)))))

(defun jde-get-visible-source-buffers ()
  "Return a list of visible Java source buffers."
  (delq nil (mapcar #'(lambda (buffer)
			(if (get-buffer-window buffer 'visible)
			    buffer))
		    (jde-get-java-source-buffers))))

(defun jde-get-selected-source-buffer ()
  (with-current-buffer (window-buffer (selected-window))
    (if (eq major-mode 'jde-mode)
	(current-buffer))))

;;;###autoload
(defun jde-exception-goto ()
  "Go to the Java source file and line indicated by an exception stack trace."
  (interactive)
  (let ((regexp "[ \t]+at \\([a-zA-Z0-9.]+\\)\\(?:\\$?[a-zA-Z0-9]*\\)\\.\\([^(]+\\)([^:]+:\\([0-9]+\\))$")
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
	  (setq line (string-to-int (match-string 3)))
	  (setq file (jde-find-class-source-file full-class))
	  (if (null file)
	      (error "Java source for class `%s' not found" full-class)))))
    (find-file-other-window file)
    (goto-line line)))

;;;###autolaod
(defalias 'jde-goto-exception 'jde-exception-goto)

(defcustom jde-htmlize-code-destinations '("~/Desktop" "~/tmp")
  "*Directories to put the output of `jde-htmlize-code'.
The function iterates through each and stops when it finds an existing
directory."
  :group 'jde-project
  :type '(repeat directory))

;;;###autoload
(defun jde-htmlize-code (start end &optional no-line-numbers-p)
  "Write the current code region as an HTML document.
Line numbers are added as well.

See `jde-htmlize-code-destinations'."
  (interactive
   (append (if mark-active
	       (list (region-beginning) (region-end))
	     (list (point-min) (point-max)))
	   (list (not current-prefix-arg))))
  (require 'htmlize)
  (save-restriction
    (narrow-to-region start end)
    (let ((code-buf (current-buffer))
	  (line-width (ceiling (log10 (count-lines (point-min) (point-max)))))
	  (ln 0))
      (with-temp-buffer
	(insert-buffer-substring code-buf)
	(untabify (point-min) (point-max))
	(goto-char (point-min))
	(if (not no-line-numbers-p)
	    (while (not (eobp))
	      (beginning-of-line)
	      (insert (format (format "%%.%dd " line-width) (incf ln)))
	      (forward-line)))
	(rename-buffer (concat (buffer-name code-buf) ".html"))
	(let ((buf (htmlize-buffer))
	      (bname (buffer-name)))
	  (unwind-protect
	      (with-current-buffer buf
		(set-visited-file-name
		 (dolist (dir jde-htmlize-code-destinations)
		   (setq dir (expand-file-name dir))
		   (if (file-exists-p dir)
		       (return (expand-file-name bname dir)))))
		(save-buffer)
		(if (featurep 'browse-url)
		    (browse-url (buffer-file-name))))
	    (if (buffer-live-p buf)
		(kill-buffer buf))))))))

(provide 'jde-util)

;; End of jde-util.el
