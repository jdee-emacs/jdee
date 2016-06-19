;;; jdee-refactor.el -- Refactor Java code.
;; $Id: $

;; Author: Paul Landes <landes <at> mailc dt net>
;; Maintainer: Paul Landes
;; Keywords: java refactor

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

;; Provide refactoring (think Eclipse) utility like renaming and repackaging
;; classes.

;;; Code:

(require 'dired-aux)
(require 'jdee-parse)
(require 'jdee-util)

;;;###autoload
(defun jdee-rename-class (new-class-name)
  "Rename the current class to another class name.  This does a class name
string replace, changes the buffer name, and changes the file name."
  (interactive "sNew class name: ")
  (if (not (eql 'jdee-mode major-mode))
      (error "Not a Java source buffer."))
  (let* ((buf-name (buffer-name))
	 (old-class-name
	  (with-temp-buffer
	    (insert buf-name)
	    (goto-char (point-min))
	    (if (re-search-forward "\\.java$" nil t)
		(replace-match ""))
	    (buffer-substring-no-properties (point-min) (point-max))))
	 (old-class-regexp (regexp-quote old-class-name)))
    (save-some-buffers)
    (dired-rename-file (buffer-file-name) (concat new-class-name ".java") t)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward old-class-regexp nil t)
	(replace-match new-class-name)))))

;;;###autoload
(defun jdee-replace-fully-qualified-class-at-point (class)
  "Replace the unqualified class at the current point with the fully qualified
class."
  (interactive (list (jdee-read-class "Fully qualify" nil nil nil t)))
  (let ((range (bounds-of-thing-at-point 'symbol)))
    (delete-region (car range) (cdr range))
    (insert class)))

(provide 'jdee-refactor)

;;; jdee-refactor.el ends here
