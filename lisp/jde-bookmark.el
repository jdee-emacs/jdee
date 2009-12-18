;;; jde-bookmark.el -- Organize bookmarked classes.
;; $Id: $

;; Author: Paul Landes <landes <at> mailc dt net>
;; Maintainer: Paul Landes
;; Keywords: java bookmark

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

;; Like bookmark.el but classes are bookmarked instead of files.  No file
;; information is kept, everything uses JDEE project information to find
;; bookmarked classes.

;;; Code:

(require 'jde-parse)
(require 'jde-util)

(declare-function jde-assert-mode "jde" (&optional no-raise-p))

(defgroup jde-bookmark nil
  "JDEE Bookmarks"
  :group 'jde
  :prefix "jde-bookmark-")

(defcustom jde-bookmark-class-bookmarks nil
  "*A list of fully qualified class names to quickly visit.
Function `jde-find-class-source' is used to visit these Java source files (see
`jde-bookmark-visit')."
  :group 'jde-bookmark
  :type '(repeat (cons :tag "Entry"
		       (string :tag "Name")
		       (string :tag "Class"))))

(defvar jde-bookmark-history nil
  "History item list for `jde-bookmark-prompt'.")

(defun jde-bookmark-prompt (&optional prompt)
  (let ((default (car jde-bookmark-history)))
    (setq prompt (or prompt 
		     (format "Class%s"
			     (if default
				 (format " (default %s): " default)
			       ": "))))
    (completing-read prompt
		     (mapcar #'car jde-bookmark-class-bookmarks)
		     nil t nil 'jde-bookmark-history
		     (car jde-bookmark-history))))

(defun jde-bookmark-class (key)
  (cdr (assoc key jde-bookmark-class-bookmarks)))

;;;###autoload
(defun jde-bookmark-visit (key)
  "Visit a class by bookmark name."
  (interactive (list (jde-bookmark-prompt)))
  (let ((class (jde-bookmark-class key)))
    (message (format "Finding class `%s'..." class))
    (jde-find-class-source class)))

;;;###autoload
(defun jde-bookmark-add (key &optional fq-class)
  "Add the current visited class as a bookmark."
  (interactive
   (list (read-string "Class entry: " (jde-parse-get-buffer-class t))))
  (jde-assert-mode)
  (setq fq-class (or fq-class (jde-parse-get-buffer-class)))
  (message (format "Adding bookmark `%s' as class `%s'"
		   key fq-class))
  (customize-save-variable 'jde-bookmark-class-bookmarks
			   (append jde-bookmark-class-bookmarks
				   (list (cons key fq-class)))))

;;;###autoload
(defun jde-bookmark-list ()
  "List bookmarks."
  (interactive)
  ;; a more dynamnic display is needed, like what's currently in bookmark.el
  ;; would be nice
  (let ((max-name-len (apply #'max
			     (mapcar #'(lambda (arg)
					 (length (car arg)))
				     jde-bookmark-class-bookmarks))))
    (save-excursion
      (set-buffer (get-buffer-create "*JDEE Bookmarks*"))
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (entry jde-bookmark-class-bookmarks)
	(insert (format "%s:%s%s\n"
			(car entry)
			(make-string (+ 1 (- max-name-len
					     (length (car entry)))) ? )
			(cdr entry))))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (display-buffer (current-buffer)))))

(provide 'jde-bookmark)

;; End of jde-bookmark.el
