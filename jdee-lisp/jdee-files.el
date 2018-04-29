;;; jdee-files.el --- General functions for working with files

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

(defun jdee-files-remove-all-matching (dir &optional match-re)
  "Remove from directory DIR all dirs and files matching MATCH-RE."
  (if (file-directory-p dir)
      (progn
        (mapc 'jdee-files-remove-all-matching
              (directory-files dir t match-re))
        (delete-directory dir))
    (delete-file dir)))

(defcustom jdee-resolve-relative-paths-p t
  "If this variable is non-nil, the JDEE converts relative paths to
absolute paths. The JDEE does this by appending the relative path to the path
of the project file for the current source buffer, if such
a file exists. Otherwise, the JDEE appends the relative path to the path
of the current directory."
  :group 'jdee-project
  :type 'boolean)

(defun jdee-normalize-path (path &optional symbol)
  "This function performs the following transformation on PATH:

  * Replaces environment variables of the form $VAR or ${VAR} with
    their values. Note that you must use the Unix notation for
    environment variables on the native Windows versions of Emacs and
    XEmacs.

  * Replaces the tilde character with the value of the home directory,
    typically specified by the HOME environment variable.

  * Converts Cygwin style paths to DOS notation on Windows.

  * Converts relative paths to absolute paths if
    `jdee-resolve-relative-paths-p' is non-nil.  Paths are resolved
    according to the location of the deepest project file found, or if
    optional SYMBOL is non-nil, paths are resolved to the location of
    the deepest project file found that defines SYMBOL.

Note: PATH can either be a path string or a symbol corresponding to a
variable that holds a path string, in which case the optional arg
SYMBOL is unnecessary."
  (if (symbolp path)
      (setq symbol path
            path (symbol-value symbol)))
  (let* ((directory-sep-char ?/)
         (p (substitute-in-file-name path))
         (len (length p)))
    (if (and
         jdee-resolve-relative-paths-p
         (> len 0)
         (eq (aref p 0) ?.))
        (let* (prj-file-path
               (dir (file-name-directory (or (buffer-file-name)
                                             default-directory))))
          ;; find the deepest originating project for the symbol
          ;; based on the current directory, and resolve to that
          ;; project's directory
          (if symbol
              (let ((prjs (get symbol 'jdee-project))
                    (sort-fn
                     (lambda (x1 x2)
                       (let* ((dir1 (file-name-directory (car x1)))
                              (dir2 (file-name-directory (car x2)))
                              match1 match2)
                         (if (null dir1)
                             (null dir2)
                           (if (null dir2)
                               t
                             (setq match1 (compare-strings
                                           dir1 0 (length dir1)
                                           dir 0 (length dir1)))
                             (setq match2 (compare-strings
                                           dir2 0 (length dir2)
                                           dir 0 (length dir2))))
                           (cond
                            ((not (eq match1 t))
                             (if (eq match2 t)
                                 nil
                               (> (length dir1) (length dir2))))
                            ((not (eq match2 t))
                             t)
                            ((> (length dir1) (length dir2)))))))))
                (setq prjs (sort prjs sort-fn))
                (setq prj-file-path (caar prjs))
                (if (string= prj-file-path "default")
                    ;; Case where the project file that sets symbol
                    ;; is the user's .emacs file. Assume that the
                    ;; user wants the relative path in the .emacs
                    ;; file to be the default relative path for
                    ;; projects that do not specify a
                    ;; relative path.
                    (setq prj-file-path
                          (jdee-find-project-file dir))))
            (setq prj-file-path
                  (jdee-find-project-file dir)))
          (if prj-file-path
              (setq dir (file-name-directory prj-file-path))
            (setq dir default-directory))
          (if (and (> len 1)
                   (eq (aref p 1) ?.))
              ;; path actually begins with `..', so normalize to one
              ;; directory up
              (save-match-data
                (string-match "\\.+/?" p)
                (setq p (expand-file-name (substring p (match-end 0))
                                          (expand-file-name (concat dir "../")))))
            (setq p (expand-file-name p dir))))
      ;; Do tilde expansion but not relative path expansion when
      ;; jdee-resolve-relative-paths-p is false.
      (if (not
           (or
            (string= p ".")
            (string-match "[.]/" p)))
          (setq p (expand-file-name p))))
    (setq p (jdee-convert-cygwin-path p))
    p))

(defmacro jdee-normalize-paths (pathlist &optional symbol)
  "Normalize all paths of the list PATHLIST and return a list with the
expanded paths.  SYMBOL is passed to `jdee-normalize-path' to expand
relative paths."
  `(mapcar (lambda (path)
             (jdee-normalize-path path ,symbol))
           ,pathlist))

(defcustom jdee-expand-wildcards-in-paths-p t
  "Expands entries in the 'jdee-sourcepath which are wildcards patterns into
a list of matching files or directories which are interpolated into the sourcepath list."
  :group 'jdee-project
  :type 'boolean)

(defun jdee-expand-wildcards-and-normalize (path &optional symbol)
  "Expand any entries with wildcard patterns in path and interpolate them into the result"
  (if jdee-expand-wildcards-in-paths-p
      (cl-mapcan
       (lambda (path)
         (let ((exp-paths (file-expand-wildcards path)))
           (if exp-paths exp-paths (list path))))
       (jdee-normalize-paths path symbol))
    (jdee-normalize-paths path symbol)))

(defun jdee-directory-files-recurs (dir &optional include-regexp)
  "Get all the files in DIR, and any subdirectories of DIR, whose
names match INCLUDE-REGEXP."
  (let (files)
    (loop for file in (directory-files dir) do
          (if (not (member file '("." "..")))
              (let ((file (concat dir "/" file)))
                (if (file-directory-p file)
                    (setq files (append files (jdee-directory-files-recurs file include-regexp)))
                  (if (or (not include-regexp)
                          (string-match include-regexp file))
                      (setq files (append files (list file))))))))
    files))

(defun jdee-expand-directory (dir include-regexp exclude-regexps symbol)
  "Get all the files in DIR whose names match INCLUDE-REGEXP except those whose
root names match EXCLUDE-REGEXPS. Return the files normalized against SYMBOL."
  (mapcar
   (lambda (included-file)
     (jdee-normalize-path included-file symbol))
   (cl-remove-if
    (lambda (file-path)
      (let ((file-name
             (file-name-nondirectory file-path)))
        (catch 'match
          (loop for regexp in exclude-regexps do
                (if (string-match regexp file-name)
                    (throw 'match t))))))
    (jdee-directory-files-recurs dir include-regexp))))

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

(provide 'jdee-files)
;;; jdee-files.el ends here
