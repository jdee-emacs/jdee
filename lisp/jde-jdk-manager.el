;;; package --- Finds and manages access to JDKs

;;; Commentary:

;;; Code:

(defun jde-set-jdk-dir-type (sym val)
  (if val
      (let ((type
	     (list
	      (quote radio-button-choice)
	      )))
	(loop for jdk in val do
	      (setq
	       type
	       (append
		type
		(list (list (quote item) (car jdk))))))
	(put 'jde-jdk
	     'custom-type
	     (list (quote list) type))
	(put 'jde-jdk 'customized-value nil)
	(put 'jde-jdk
	     'standard-value
	     (list (list (quote list) (car (car val)))))
	(customize-set-value 'jde-jdk (list (car (car val)))))
    (progn
      (put 'jde-jdk 'custom-type 'symbol)
      (put 'jde-jdk 'standard-value nil)
      (put 'jde-jdk 'customized-value nil)
      (customize-set-value 'jde-jdk nil)))
  (set-default sym val))

(defun jde-default-jdk-registry ()
  "Attempts to build a default value for jde-jdk-registry.
This function uses platform specific rules and/or heuristics to
pick a sensible default for jde-jdk-registry."
  (let (version dir)
    ;; Set version and dir for the current system
    (cond
     ;; Mac OS X: find default
     ((eq system-type 'darwin)
      (when (file-executable-p "/usr/libexec/java_home")
	(setq dir (substring (shell-command-to-string "/usr/libexec/java_home")
			     0 -1))
	(if (string-match "\\(1\\.[45678]\\)\\.[0-9]" dir)
	    (setq version (match-string 1 dir)))))

     ;; On Linux use the default javac if it is installed
     ((eq system-type 'gnu/linux)
      (when (file-executable-p "/usr/bin/javac")
	(let ((javac "/usr/bin/javac"))
	  (while (file-symlink-p javac)
	    (setq javac (file-symlink-p javac)))
	  (setq dir (expand-file-name ".." (file-name-directory javac)))
	  (cond
	   ;; java-1.6.0-openjdk-amd64 or jdk1.7.0_21 etc.
	   ((string-match "\\(1\\.[4567]\\)\\.[0-9]" dir)
	    (setq version (match-string 1 dir)))

	   ;; j2sdk1.6-oracle etc
	   ((string-match "[^0-9]\\(1\\.[4567]\\)\\-" dir)
	    (setq version (match-string 1 dir)))

	   ;; java-7-openjdk-amd64 etc
	   ((string-match "-\\([45678]\\)-" dir)
	    (setq version (concat "1." (match-string 1 dir))))))))
     ;; On other systems the user needs to customize this to get a
     ;; fully functional install (patches welcome!)
     (t
      nil))
    (and version dir (list (cons version dir)))))


;; (makunbound 'jde-jdk-registry)
(defcustom jde-jdk-registry (jde-default-jdk-registry)
  "Specifies the versions and locations of the JDKs installed on your
system.  For each JDK to be registered, enter the version number
\(e.g., 1.4.0) of the JDK in the Version field. Enter the path of the
JDK's root directory (e.g., c:/jdk1.3.1 or $JAVA_HOME) in the Path
field. Setting this variable determines the choices offered by the
`jde-jdk' variable. You should therefore customize this variable
first."
  :group 'jde-project
  :type '(repeat
	  (cons
	   :tag "JDK"
	   (string :tag "Version")
	   (string :tag "Path")))
  :set 'jde-set-jdk-dir-type)

(provide 'jde-jdk-manager)
;;; jde-jdk-manager ends here
