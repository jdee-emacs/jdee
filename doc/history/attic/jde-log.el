;;; jde-log.el --- jde-log
;;; $Id$

;; Copyright (C) 2009 Paul Landes
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: cvs svn log

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
;; This is a throw about piece of code to wrangle all the old CVS entries from
;; each jde lisp file from before it trashing it.  I'm adding it to the
;; repository in case we want to use it for other log entries from other files
;; (i.e. Java, etc).

;;; Code:

(defun jde-log-transform ()
  "Create a subversion xml file by scraping files with exported $Log$ entries.
It parse exported log entries from CVS and puts them into a format that
conforms to the output of
  svn log --xml --versbose"
  (interactive)
  (let ((elibs (directory-files "~/view/jde/lisp" t "\\.el$"))
	buf entries)
    (dolist (elib elibs)
      (message "Finding libs in %s" elib)
      (setq buf (find-file-noselect elib))
      (setq entries (append entries (jde-log-entries buf))))
    (save-excursion
      (set-buffer (get-buffer-create "cvs-revision-log.xml"))
      (erase-buffer)
      (goto-char (point-min))
      (jde-log-create-xml entries)
      (nxml-mode)
      (indent-region (point-min) (point-max))
      (display-buffer (current-buffer)))))

(defun jde-log-entries (&optional buf)
  (flet ((first-line
	  ()
	  (let ((i 0))
	    (mapcar '(lambda (field)
		       (cons field (match-string-no-properties (incf i))))
		    '(revision date time author)))))
    (let (entry entries msg)
      (save-excursion
	(if buf (set-buffer buf))
	(goto-char (point-min))
	(if (not (re-search-forward "^;;? +\\$Log: " nil t))
	    (progn
	      (message "No entries found for %s" (buffer-name))
	      nil)
	  (while (re-search-forward "^;;? +Revision \\([0-9.]+\\)[ \t]+\\([0-9/]+\\) \\([0-9:]+\\)[ \t]+\\(.+\\)$" nil t)
	    (setq msg nil
		  entry (first-line))
	    (forward-line 1)
	    (beginning-of-line)
	    (while (and (not (eobp))
			(not (looking-at "^;;?[ \t]*$"))
			(looking-at "^;;? +\\(.*\\)$"))
	      (setq msg (append msg (list (match-string-no-properties 1))))
	      (forward-line 1))
	    (setq entry (append entry `((msg . ,msg)
					(name . ,(buffer-name))
					)))
	    (setq entries (append entries (list entry)))))
	entries))))

(defun jde-log-insert-entry (entry)
  (insert (format "<logentry revison=\"%s\">\n" (cdr (assq 'revision entry))))
  (insert (format "<author>%s</author>\n" (cdr (assq 'author entry))))
  (insert (format "<date>%s %s</date>\n"
		  (cdr (assq 'date entry))
		  (cdr (assq 'time entry))))
  (insert "<paths>\n")
  (insert (format "<path action=\"M\">/jde/lisp/%s</path>\n" (cdr (assq 'name entry))))
  (insert "</paths>\n")
  (insert (format "<msg>%s</msg>\n" (cdr (assq 'msg entry))))
  (insert "</logentry>\n")))

(defun jde-log-create-xml (entries)
  (insert "<?xml version=\"1.0\"?>\n")
  (insert "<log>\n")
  (dolist (entry entries)
    (jde-log-insert-entry entry))
  (insert "</log>"))

(provide 'jde-log)

;;; jde-log.el ends here
