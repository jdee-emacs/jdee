;; FIXME: rename
(defun list-to-hash (coll)
  "Convert the `archive-files' vector to a hashtable of string to 'indexed."
  (let ((rtnval (make-hash-table :test 'equal)))
    (cl-loop for r across coll
             when r
             do (puthash (elt r 1) 'indexed rtnval))
    rtnval))
  

(defun jdee-archive-resource (buf archive resource)
  "For the buffer BUR, which needs to be in arc-mode, find
RESOURCE."
  (when buf
    (with-current-buffer buf
      (let* ((files archive-files)
             (r (list-to-hash files)))
        (if (eq 'missing (gethash resource r 'missing))
            nil
          (list archive resource))))))

(defun jdee-archive-has-resource-in-existing-buffer-p (archive resource)
  "See if ARCHIVE is in a buffer and if so, check it for
RESOURCE."
  (let* ((buf (get-file-buffer archive))
        (rtnval (jdee-archive-resource buf archive resource)))
    rtnval))
      
(defun jdee-archive-has-resource-p (archive resource)
  "Using arc-mode, try to load the ARCHIVe and see if it contains
RESOURCE.  Kills the buffer with the archive."
  (when (and (file-exists-p archive)
             (not (file-directory-p archive)))
    (let* ((buffer (find-file-noselect archive))
           (rtnval (jdee-archive-resource buffer archive resource)))
      (when buffer
        (kill-buffer buffer))
      rtnval)))
           
  
(defun jdee-archive-find-resource (resource &rest paths)
  "Return the archive that has RESOURCE.  PATHS is a list of
lists of file names."
  
  (cl-loop for p in (apply #'append paths)  
           for rtnval = (or (jdee-archive-has-resource-in-existing-buffer-p p resource)
                            (jdee-archive-has-resource-p p resource))
           when rtnval
              return rtnval))

(defun jdee-archive-which (disp fqn &rest paths)
  (interactive "p\nsFQN: ")
  (let* ((resource (format "%s.class" (subst-char-in-string ?. ?/ fqn)))
         (rtnval (apply #'jdee-archive-find-resource resource (or paths (list jdee-global-classpath)))))
    (when (and disp rtnval)
      (apply #'message "Adding %s:%s to kill ring" rtnval))
    rtnval))



