;;; jdee-file-util.el --- General functions for working with files

;; Maintainer: jdee-devel

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
;; This file contains functions for working with files.
;; TODO: replaced with some files package from ELPA.

;;; Code:

(defun jdee-file-util-remove-all-matching (dir &optional match-re)
  "Remove from directory DIR all dirs and files matching MATCH-RE."
  (if (file-directory-p dir)
      (progn
        (mapc 'jdee-file-util-remove-all-matching
              (directory-files dir t match-re))
        (delete-directory dir))
    (delete-file dir)))

(defun jdee-root-dir-p (dir)
  "Return nonnil if DIR is a root directory."
  (let ((parent (expand-file-name  ".." dir)))
    (cond
     ((and
       (fboundp 'ange-ftp-ftp-name)
       (fboundp 'ange-ftp-get-file-entry)
       (ange-ftp-ftp-name dir))
      (ange-ftp-get-file-entry parent))
     ((eq system-type 'windows-nt)
      ;; If the current directory tree is on a
      ;; virtual drive created by the subst command
      ;;
      ;;  (not (file-exists-p parent))
      ;;
      ;; fails. Hence, the following hack contributed
      ;; by Nat Goodspeed.
      (or
       (string= parent "//") ; for paths like //host/d/prj/src
       (string= parent "\\\\") ; for paths like \\host\d\prj\src
       (string= (substring parent -3) "/..") ; for paths like d:/prj/src
       (save-match-data
         (and (string-match "^[a-zA-Z]:/$" parent) t)))) ; for paths like d:/
     ((member system-type '(cygwin32 cygwin))
      (or (string= (file-truename dir) (file-truename "/"))
          (string= parent "//") ; for paths like //host/d/prj/src
          (string= parent "\\\\") ; for paths like \\host\d\prj\src
          (and (> (length parent) 3) ; for paths like d:/prj/src
               (string= (substring parent -3) "/.."))
          (not (file-exists-p (file-truename dir)))))
     (t
      (or (or (not (file-readable-p dir))
              (not (file-readable-p parent)))
          (and
           (string= (file-truename dir) "/")
           (string= (file-truename parent) "/")))))))

(provide 'jdee-file-util)
;;; jdee-file-util.el ends here
