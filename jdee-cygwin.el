;;; jdee-cygwin.el --- Cygwin helper functions

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

;;; Code:

(defun jdee-cygpath (path &optional direction)
  "If `system-type' is cygwin or cygwin32, converts PATH from cygwin
to DOS form (if DIRECTION is nil) or to cygwin form (if DIRECTION is
nonnil). The converion requires that cygpath be in your path. If the
`system-type' is not cygwin or cygwin32, returns PATH unchanged."
  (interactive "sPath: ")
  (if (member system-type '(cygwin32 cygwin))
      (if (executable-find "cygpath")
          (save-excursion
            (let ((buf-name "*cygwin-output*")
                  (output-type (if direction "-u" "-w")))
              (shell-command
               (concat "cygpath " output-type " -p '" path "'") buf-name)
              (set-buffer buf-name)
              (let ((output (buffer-substring (point-min) (point-max))))
                (kill-buffer buf-name)
                (cl-substitute ?\/ ?\\ (cl-remove ?\n output)))))
        (error "Cannot find cygpath executable"))
    path))

(defvar jdee-cygwin-root-cache nil
  "Cache of converted cygwin root directory paths.")

(defun jdee-cygwin-path-converter-cygpath (path)
  (interactive "sPath: ")
  (if (string-match "^[a-zA-Z]:" path)
      path
    (cond
     ((string-match "^/\\(cygdrive\\)?/\\([a-zA-Z]\\)/" path)
      (concat
       (substring
        path
        (match-beginning 2)
        (match-end 2))
       ":/"
       (substring path (match-end 0))))
     ((string-match "^/[^/]*" path)
      (let* ((root (substring
                    path (match-beginning 0) (match-end 0)))
             (rest (substring path (match-end 0)))
             (converted-root (cdr (assoc root jdee-cygwin-root-cache))))
        (if (not converted-root)
            (progn
              (setq converted-root (jdee-cygpath root))
              (if converted-root
                  (add-to-list 'jdee-cygwin-root-cache
                               (cons root converted-root))
                (error "Cannot convert %s" path))))
        (if (string= rest "")
            converted-root
          (concat converted-root rest))))
     (t
      (error "Cannot convert %s" path)))))


(defun jdee-cygwin-path-converter-internal (path)
  "Convert cygwin style `PATH' to a form acceptable to java vm.
Basically converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file'
to 'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (if (fboundp 'mswindows-cygwin-to-win32-path)
      (cl-substitute ?/ ?\\ (mswindows-cygwin-to-win32-path path))
    (let* ((path-re "/\\(cygdrive\\)?/\\([a-zA-Z]\\)/")
           (subexpr 2)
           (index1 (* 2 subexpr))
           (index2 (1+ index1)))
      (if (string-match (concat "^" path-re) path)
          (let ((new-path
                 (concat (substring path
                                    (nth index1 (match-data))
                                    (nth index2 (match-data)))
                         ":/"
                         (substring path (match-end 0)))))
            (while (string-match (concat ":" path-re) new-path)
              (setq new-path
                    (concat
                     (substring new-path 0 (match-beginning 0))
                     ";"
                     (substring new-path
                                (nth index1 (match-data))
                                (nth index2 (match-data)))
                     ":/"
                     (substring new-path (match-end 0)))))
            (cl-substitute ?\\ ?\/ new-path))
        path))))

(defcustom jdee-cygwin-path-converter '(jdee-cygwin-path-converter-internal)
  "Function to use to convert cygwin paths to DOS paths.
Choose jdee-cygwin-path-converter-internal, jdee-cygwin-path-converter-cygpath,
or \"custom-function.\" jdee-cygwin-path-converter-cygpath handles all
cygwin-style paths, including mount points, e.g.,/bin.
jdee-cygwin-path-converter-internal does not handle mount
paths. However, it is much faster as it does not require running a
subprocess every time the JDEE needs to convert a path. Choose
\"custom-function\" if you want the JDEE to use a function that you
supply. Replace \"custom-function\" with the name of the function that
you want to use."
  :group 'jdee-project
  :type  '(list
           (radio-button-choice :format "%t \n%v"
                                :tag "Converter: "
                                :entry-format "  %b %v"
                                (const jdee-cygwin-path-converter-internal)
                                (const jdee-cygwin-path-converter-cygpath)
                                (function custom-function))))


(defun jdee-convert-cygwin-path (path &optional separator)
  "Convert cygwin style `PATH' to a form acceptable to java vm, using
the conversion function specified by `jdee-cygwin-path-converter'."
  (interactive "sPath: ")
  (funcall (car jdee-cygwin-path-converter)
           (if separator
               (cl-substitute ?\: (string-to-char separator) path)
             path)))

(provide 'jdee-cygwin)

;;; jdee-cygwin.el ends here
