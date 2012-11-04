;; which.el - UNIX command line `which' like library
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2000, 2001, 2002, 2003, 2004 Paul Kinnucan.
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

(defvar windows-suffixes 
  (if (memq system-type (list 'ms-dos 'windows-nt))
      (list ".exe" ".EXE" ".cmd" ".CMD" ".bat" ".BAT" "")
    (list ""))
  "List of suffixes used by Windows executables")

(defun which (exe &optional insert &optional silent) 
  "Show the full path name of an executable.
With a prefix argument, insert the full-path name at point.
This command searches the directories in `exec-path'."
  (interactive "sWhich: \nP")
  (let ((executable (which-find-executable exe exec-path)))
    (if (not executable)
(or silent (message "Can't find %s in search path" exe))
      (and insert
   (insert executable))
      (or silent (message "%s is %s" exe executable))
      executable)))

(defun which-find-executable (exe directory-list) 
  "Show the full path name of an executable in DIRECTORY-LIST."
  (catch 'answer
    (mapc
     '(lambda (dir)
(mapcar
'(lambda (suf)
    (let ((try (expand-file-name (concat exe suf) dir)))
      (and (file-executable-p try)
   (null (file-directory-p try))
   (throw 'answer try))))
windows-suffixes))
     directory-list)
    nil))

(defun which-find-all-executables (exe directory-list) 
  "Show the full path name of an executable in DIRECTORY-LIST."
  (let ((answers))
    (mapc
     '(lambda (dir)
(mapc
'(lambda (suf)
    (let ((try (expand-file-name (concat exe suf) dir)))
      (and (file-executable-p try)
   (null (file-directory-p try))
   (setq answers (cons try answers))
   )))
windows-suffixes))
     directory-list)
    answers
    ))

(defun which-find-file (exe)
  "Find an executable file from `exec-path'."
  (interactive "sWhich: ")
  (let ((file (which exe nil t)))
    (and file
(find-file file))))

(provide 'which)
