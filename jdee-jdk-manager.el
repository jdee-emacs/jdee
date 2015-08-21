;;; package --- Finds and manages access to JDKs

;;; Commentary:
;; This code is responsible for finding installed JDKs and defining
;; default `jdee-jdk-registry', which is a set of pairs (version dir).
;;
;; It tries to find JDKs in default paths for each system. For example
;; for GNU/Linux usually it is "/usr/lib/jvm".

;;; Code:

(require 'cl-macs)

(defun jdee--jdk-set-dir-type (sym val)
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
	(put 'jdee-jdk
	     'custom-type
	     (list (quote list) type))
	(put 'jdee-jdk 'customized-value nil)
	(put 'jdee-jdk
	     'standard-value
	     (list (list (quote list) (car (car val)))))
	(customize-set-value 'jdee-jdk (list (car (car val)))))
    (progn
      (put 'jdee-jdk 'custom-type 'symbol)
      (put 'jdee-jdk 'standard-value nil)
      (put 'jdee-jdk 'customized-value nil)
      (customize-set-value 'jdee-jdk nil)))
  (set-default sym val))

;; TODO: replace with some existing file library
(defun jdee--jdk-find-dirs (paths)
  "Return full paths to dirs in given `PATHS'."
  (let (dirs)
    (dolist (path paths dirs)
      (let ((all-files (ignore-errors (directory-files path t))))
	(setq all-files (delete (concat path "/" ".") all-files))
	(setq all-files (delete (concat path "/" "..") all-files))
	(dolist (f all-files dirs)
	  (when (and (file-directory-p f)
		     (not (file-symlink-p f)))
	    (setq dirs (cons f dirs))))))))

(defun jdee--jdk-p (path)
  "Return t if given `PATH' is path to JDK (has Java compiler)."
  (file-executable-p (concat path "/bin/javac")))

(defun jdee--jdk-get-version (dir)
  "Return version of JDK in given DIR."
  (cond
   ((null dir) nil)

   ;; java-1.6.0-openjdk-amd64 or jdk1.7.0_21 etc.
   ((string-match "\\(1\\.[456789]\\)\\.[0-9]" dir)
    (match-string 1 dir))

   ;; j2sdk1.6-oracle etc
   ((string-match "[^0-9]\\(1\\.[456789]\\)\\-" dir)
    (match-string 1 dir))

   ;; java-7-openjdk-amd64 etc
   ((string-match "-\\([456789]\\)-" dir)
    (concat "1." (match-string 1 dir)))))

(defun jdee--jdk-find-darwin-jdk ()
  "Return a (VERSION DIR) pair or nil when not found.
Mac OS X default."
  (let (version dir)
    (when (file-executable-p "/usr/libexec/java_home")
      (setq dir (substring (shell-command-to-string "/usr/libexec/java_home")
			   0 -1))
      (if (string-match "\\(1\\.[45678]\\)\\.[0-9]" dir)
	  (setq version (match-string 1 dir))))
    (and version dir (list (cons version dir)))))

(defun jdee--jdk-newest-first (jdks)
  "Sort `JDKS' ordering from newest to oldest or nil when empty."
  (sort jdks
	(lambda (c1 c2) ; Compare only versions:
	  (string< (first c2) (first c1)))))

(defun jdee--jdk-find-linux-jdk ()
  "Return a (VERSION . DIR) pair or nil when not found."
  (let ((jdks '()))
    ;; Default JDK paths for GNU/Linux are:
    ;; - /usr/lib/jvm for Debian based and RedHat
    ;; - /usr/lib64/jvm for Open Suse

    (dolist (dir (jdee--jdk-find-dirs '("/usr/lib/jvm" "/usr/lib64/jvm")))
      (let ((version (jdee--jdk-get-version dir)))
	(when (and version (jdee--jdk-p dir))
	  (setq jdks (cons (cons version dir) jdks)))))

    ;; On Linux use the default javac if it is installed.
    (let (version dir)
      (when (file-executable-p "/usr/bin/javac")
	(let ((javac "/usr/bin/javac"))
	  (while (file-symlink-p javac)
	    (setq javac (file-symlink-p javac)))
	  (setq dir (expand-file-name ".." (file-name-directory javac)))
	  (setq version (jdee--jdk-get-version dir))))
      (when (and version dir)
	(setq jdks (cons (cons version dir) jdks))))

    ;; Path scan and /usr/bin/javac may resolve to the same values:
    (delete-dups jdks)

    (jdee--jdk-newest-first jdks)))

(defun jdee--jdk-find-other-os-jdk ()
  "Return a (VERSION DIR) pair or nil when not found."
  ;; On other systems the user needs to customize this to get a
  ;; fully functional install (patches welcome!)
  nil)

(defun jdee-jdk-build-default-registry ()
  "Attempts to build a default value for jdee-jdk-registry.
This function uses platform specific rules and/or heuristics to
pick a sensible default for jdee-jdk-registry."
  ;; Set version and dir for the current system
  (cond
   ((eq system-type 'darwin)
    (jdee--jdk-find-darwin-jdk))
   ((eq system-type 'gnu/linux)
    (jdee--jdk-find-linux-jdk))
   (t
    (jdee--jdk-find-other-os-jdk))))


;; (makunbound 'jdee-jdk-registry)
(defcustom jdee-jdk-registry (jdee-jdk-build-default-registry)
  "Specifies the versions and locations of the JDKs installed on your
system.  For each JDK to be registered, enter the version number
\(e.g., 1.4.0) of the JDK in the Version field. Enter the path of the
JDK's root directory (e.g., c:/jdk1.3.1 or $JAVA_HOME) in the Path
field. Setting this variable determines the choices offered by the
`jdee-jdk' variable. You should therefore customize this variable
first."
  :group 'jdee-project
  :type '(repeat
	  (cons
	   :tag "JDK"
	   (string :tag "Version")
	   (string :tag "Path")))
  :set 'jdee--jdk-set-dir-type)

(provide 'jdee-jdk-manager)
;;; jdee-jdk-manager ends here
