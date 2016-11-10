;;; jdee-archive.el -- Archive mode for jdee

;; Author: Matthew O. Smith <matt@m0smith.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 2000, 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
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
;; Boston, MA 02111-1307, US
;;; Commentary:

;; Code to manage archives like JAR files.  This code takes advantage
;; of archive-mode which knows how to open and read various types of
;; archive including JAR files.

;;; Code:

(require 'memoize)

(defun jdee-archive-files-hashtable (coll)
  "Convert the COLL which looks like an `archive-files' vector to
a hashtable of string to 'indexed."
  (let ((rtnval (make-hash-table :test 'equal)))
    (cl-loop for r across coll
             when r
             do (puthash (elt r 1) 'indexed rtnval))
    rtnval))

(defun jdee-archive-resource-from-ht (ht archive resource)
  "For the buffer BUR, which needs to be in `archive-mode', find
RESOURCE."
  (when ht
    (if (eq 'missing (gethash resource ht 'missing))
        nil
      (list archive resource))))
  

(defun jdee-archive-resource (buf archive resource)
  "For the buffer BUR, which needs to be in `archive-mode', find
RESOURCE."
  (when buf
    (with-current-buffer buf
      (let* ((files archive-files)
             (r (jdee-archive-files-hashtable files)))
        (jdee-archive-resource-from-ht r archive resource)))))


(defun jdee-archive-has-resource-in-existing-buffer-p (archive resource)
  "See if ARCHIVE is in a buffer and if so, check it for
RESOURCE."
  (let* ((buf (get-file-buffer archive))
        (rtnval (jdee-archive-resource buf archive resource)))
    rtnval))


(defun jdee-archive-extract-ht-non-cached (archive)
  "Load ARCHIVE into a buffer and return a hashtable of the `archive-files'.
Delete the temp buffer. "
  (when (and (file-exists-p archive)
             (not (file-directory-p archive)))
    (let* ((buffer (find-file-noselect archive)))
      (when buffer
        (with-current-buffer buffer
          (let ((rtnval (jdee-archive-files-hashtable archive-files)))
            (kill-buffer buffer)
            rtnval))))))


;; Create a caching version of jdee-archive-extract-ht-non-cached
(fset 'jdee-archive-extract-ht (memoize #'jdee-archive-extract-ht-non-cached))


(defun jdee-archive-has-resource-p (archive resource)
  "Using `archive-mode', try to load the ARCHIVE and see if it contains
RESOURCE.  Kills the buffer with the archive."
  (let* ((ht (jdee-archive-extract-ht archive))
         (rtnval (jdee-archive-resource-from-ht ht archive resource)))
    rtnval))

(defun jdee-archive-is-resource-on-disk-p (dir resource)
  (let ((file (expand-file-name resource dir)))
    (when (file-exists-p file)
        file)))

(defun jdee-archive-find-resource (resource &rest paths)
  "Return the archive that has RESOURCE.  PATHS is a list of
lists of file names.

Return:
   nil if not found  
   \"file\" if fqn is a class file on disk
   '(archive resource) for a file contained in an archive (JAR file).
"
  
  (cl-loop for p in (apply #'append paths)  
           for rtnval = (or (jdee-archive-is-resource-on-disk-p p resource)
                            (jdee-archive-has-resource-in-existing-buffer-p p resource)
                            (jdee-archive-has-resource-p p resource))
           when rtnval
               return rtnval))
(defun jdee-archive-resource-to-string (resource)
  "Convert the result of `jdee-archive-find-resource' to a string.
"
  (cond
   ((null resource) nil)
   ((listp resource) (apply #'format "%s:%s" resource))
   (t resource)))
  

;;;###autoload
(defun jdee-archive-which (fqn &optional disp &rest paths)
  "Finds which archive contains FQN.
Search PATHS or `jdee-global-classpath' if nil.

Return:
   nil if not found  
   \"file\" if fqn is a class file on disk
   '(archive resource) for a file contained in an archive (JAR file).
"
  (interactive "sFQN: \np")
  (let* ((resource (format "%s.class" (subst-char-in-string ?. ?/ fqn)))
         (rtnval (apply #'jdee-archive-find-resource resource (or paths (list jdee-global-classpath)))))
    
    (if rtnval
        (let ((as-string (jdee-archive-resource-to-string rtnval)))
          (kill-new as-string)
          (when disp
            (message "%s added to kill ring." as-string)))
      (when disp
        (message "%s not found." fqn)))
    rtnval))



(provide 'jdee-archive)

;;; jdee-archive.el ends here
