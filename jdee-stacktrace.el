;;; jdee-stacktrace.el --- Support for stacktraces

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes
;; Keywords: java, tools
;; URL: http://github.com/jdee-emacs/jdee
;; Version: 2.4.2
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 1997-2008 Paul Kinnucan.
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
;;
;; minimum Emacs version supported is 24.3

;;; Code:


(defun jdee-stacktrace-file ()
  "A function for use in `compilation-error-regexp-alist' as the
file name.  Expects (match-string 2) to return the fully
qualified name of the class."
  (jdee-stacktrace-file* (match-string 2)))

(defun jdee-stacktrace-file* (fqn)
  "Return the full path to the source file for FQN (fully qualified name).  If not found, return FQN."
  (let* ((path (save-match-data (jdee-find-class-source-file fqn))))
    (or path fqn)))

(define-derived-mode jdee-stacktrace-mode compilation-mode "JVM Stack Trace"
  "Major mode for inspecting stack traces.  Expects
`jdee-sourcepath' to be set appropriately"
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist
        ;; 1 = all
        ;; 2 = qualified class name
        ;; 3 = method
        ;; 4 = file
        ;; 5 = line
        '(("\tat \\(\\([[:alpha:]_$][[:alnum:]._$]*\\)[.]\\([[:alpha:]_$][[:alnum:]_$]*\\)(\\([[:alnum:].]+\\):\\([0-9]+\\))\\)"
           jdee-stacktrace-file 5 nil nil
           1
           (4 compilation-info-face)
           (2 compilation-error-face))))
  )


(defun jdee-stacktrace-buffer ( &optional buffer)
  "Open a buffer to paste a stack trace.  Parses the stack trace
to allow editting of the source."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let* ((active (region-active-p))
	     (beg (if active (region-beginning)))
	     (end (if active (region-end))))
	(with-current-buffer (pop-to-buffer  "*JDEE Stack Trace*")
	  (jdee-stacktrace-mode)
	  (setq inhibit-read-only t)
	  (when active
	    (let ((start (point-max)))
	      (goto-char start)
	      (insert-buffer-substring buffer beg end)
	      (goto-char start))))))))

(provide 'jdee-stacktrace)

;;; jdee-stacktrace.el ends here
