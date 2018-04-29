;;; jdee-stacktrace.el --- Support for stacktraces

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: Paul Landes

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

;;; Code:

(defun jdee-stacktrace-file ()
  "A function for use in `compilation-error-regexp-alist' as the file name.
Expects (match-string 1) to return the fully qualified name of the class."
  (jdee-stacktrace-file* (match-string 1)))

(defun jdee-stacktrace-file* (fqn)
  "Return the full path to the source file for FQN (fully qualified name).
If not found, return FQN."
  (let* ((path (save-match-data (jdee-find-class-source-file fqn))))
    (cond
     ((bufferp path) (with-current-buffer path buffer-file-name))
     ((stringp path) path)
     (t fqn))))

(defun jdee-stacktrace-item-re ()
  "Match regions set:
   1 - FQN
   2 - package name
   3 - class name
   4 - method name
   5 - file name
   6 - line number"
  (format "at %s[.]%s(%s:\\([[:digit:]]+\\))"
          (jdee-parse-java-fqn-re)   ; FQN
          (jdee-parse-java-name-part-re)    ; Method
          (jdee-parse-java-name-parts-re)))  ; file name

(define-derived-mode jdee-stacktrace-mode compilation-mode "JVM Stack Trace"
  "Major mode for inspecting stack traces.  Expects
`jdee-sourcepath' to be set appropriately"
  ;; FIXME: Should be a minor mode
  (make-local-variable 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist
        (list
         (list
          (jdee-stacktrace-item-re)
          1 ;'jdee-stacktrace-file
          6 nil nil
          1
          '(4 compilation-info-face)
          '(2 compilation-error-face)))))

(defun jdee-stacktrace-buffer (clear &optional buffer)
  "Open a buffer to paste a stack trace.
Parses the stack trace to allow editting of the source.

If called with a prefix, will clear the stake trace buffer.

If a region is active, paste it into the stack trace buffer."
  (interactive "P")
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let* ((active (region-active-p))
	     (beg (if active (region-beginning)))
	     (end (if active (region-end))))
	(with-current-buffer (pop-to-buffer  "*JDEE Stack Trace*")
          (when clear (erase-buffer))
	  (jdee-stacktrace-mode)
          (make-local-variable 'jdee-sourcepath)
	  (setq inhibit-read-only t)
	  (when active
	    (let ((start (point-max)))
	      (goto-char start)
	      (insert-buffer-substring buffer beg end)
	      (goto-char start))))))))

(provide 'jdee-stacktrace)

;;; jdee-stacktrace.el ends here
