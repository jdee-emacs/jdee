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
    (mapcar
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
    (mapcar
     '(lambda (dir)
(mapcar
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