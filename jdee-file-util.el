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

(provide 'jdee-file-util)
;;; jdee-file-util.el ends here
