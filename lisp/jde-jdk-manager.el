;;; package --- Finds and manages access to JDKs

;;; Commentary:
;; This code is responsible for finding installed JDKs and defining
;; default `jde-jdk-registry', which is a set of pairs (version dir).
;;
;; It tries to find JDKs in default paths for each system. For example
;; for GNU/Linux usually it is "/usr/lib/jvm".

;;; Code:

(require 'cl-macs)

(defun jde--jdk-set-dir-type (sym val)
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

(defun jde--jdk-get-version (dir)
  "Return version of JDK in given DIR."
  (cond
   ((null dir) nil)

   ;; java-1.6.0-openjdk-amd64 or jdk1.7.0_21 etc.
   ((string-match "\\(1\\.[4567]\\)\\.[0-9]" dir)
    (match-string 1 dir))

   ;; j2sdk1.6-oracle etc
   ((string-match "[^0-9]\\(1\\.[4567]\\)\\-" dir)
    (match-string 1 dir))

   ;; java-7-openjdk-amd64 etc
   ((string-match "-\\([45678]\\)-" dir)
    (concat "1." (match-string 1 dir)))))

(defun jde--jdk-find-darwin-jdk ()
  "Return a (VERSION DIR) pair or nil when not found.
Mac OS X default."
  (let (version dir)
    (when (file-executable-p "/usr/libexec/java_home")
      (setq dir (substring (shell-command-to-string "/usr/libexec/java_home")
			   0 -1))
      (if (string-match "\\(1\\.[45678]\\)\\.[0-9]" dir)
	  (setq version (match-string 1 dir))))
    (and version dir (list (cons version dir)))))

(defun jde--jdk-find-linux-jdk ()
  "Return a (VERSION DIR) pair or nil when not found."
  ;; On Linux use the default javac if it is installed.
  (let (version dir)
    (when (file-executable-p "/usr/bin/javac")
      (let ((javac "/usr/bin/javac"))
	(while (file-symlink-p javac)
	  (setq javac (file-symlink-p javac)))
	(setq dir (expand-file-name ".." (file-name-directory javac)))
	(setq version (jde--jdk-get-version dir))))
    (and version dir (list (cons version dir)))))

(defun jde--jdk-find-other-os-jdk ()
  "Return a (VERSION DIR) pair or nil when not found."
  ;; On other systems the user needs to customize this to get a
  ;; fully functional install (patches welcome!)
  nil)

(defun jde-jdk-build-default-registry ()
  "Attempts to build a default value for jde-jdk-registry.
This function uses platform specific rules and/or heuristics to
pick a sensible default for jde-jdk-registry."
  ;; Set version and dir for the current system
  (cond
   ((eq system-type 'darwin)
    (jde--jdk-find-darwin-jdk))
   ((eq system-type 'gnu/linux)
    (jde--jdk-find-linux-jdk))
   (t
    (jde--jdk-find-other-os-jdk))))


;; (makunbound 'jde-jdk-registry)
(defcustom jde-jdk-registry (jde-jdk-build-default-registry)
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
  :set 'jde--jdk-set-dir-type)

(provide 'jde-jdk-manager)
;;; jde-jdk-manager ends here
