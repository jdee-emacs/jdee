;;; jdee-jdk-manager.el --- Finds and manages access to JDKs

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
;; This code is responsible for finding installed JDKs and defining
;; default `jdee-jdk-registry', which is a set of pairs (version dir).
;;
;; It tries to find JDKs in default paths for each system.  For example for
;; GNU/Linux usually it is "/usr/lib/jvm".

;;; Code:

(require 'cl-macs)
(require 'jdee-backend)

(defvar jdee-java-version-cache nil
  "Cache to hold the version of Java being used.")

(defun jdee-java-version-via-java ()
  "Get the version of the Java VM on the system command path."
  (if (not jdee-java-version-cache)
      (let ((buf (get-buffer-create "java version"))
	    proc)
	(with-current-buffer buf
	  (setq proc
		(start-process
		 "java version" buf "java" "-version"))
	  (set-process-query-on-exit-flag proc nil)
	  (accept-process-output proc 10)
	  (goto-char (point-min))
          (re-search-forward "[1-9]\\([.][1-9]\\)?" (point-max) t)
	  (setq jdee-java-version-cache (match-string 0)))
	(kill-buffer buf)))
  jdee-java-version-cache)

(defun jdee-java-version ()
  "Get the version of Java used by the JDEE."
  (interactive)
  (let ((java-version (if jdee-jdk (car jdee-jdk)
			(getenv
			 (nth 0 jdee-java-environment-variables)))))
    (if (not java-version)
	(if jdee-java-version-cache
	    (setq java-version jdee-java-version-cache)
	  (if (jdee-backend-running-p)
	      (progn
		(setq jdee-java-version-cache
                      (jdee-backend-get-java-version))
		(setq java-version jdee-java-version-cache))
	    (setq java-version (jdee-java-version-via-java)))))
    (if (called-interactively-p 'interactive)
	(message java-version)
      java-version)))

(defun jdee-java-major-version ()
  "Return an integer representing the major version of the JDK being used
by the current project."
  (let ((version (jdee-java-version)))
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)"
                  version)
    (string-to-number
     (substring
      version
      (match-beginning 1)
      (match-end 1)))))

(defun jdee-java-minor-version ()
  "Return an integer representing the minor version of the JDK being used
by the current project."
  (let ((version (jdee-java-version)))
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)"
                  version)
    (string-to-number
     (substring
      version
      (match-beginning 2)
      (match-end 2)))))

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

(defcustom jdee-java-environment-variables '("JAVA_VERSION" "JAVA_HOME")
  "This variable specifies the names of environment variables used to
specify the version and location of the JDK to be used by the JDEE.
If set, the `jdee-jdk' customization variable overrides the
java enviroment variables."
  :group 'jdee-project
  :type '(list
	  (string :tag "Java Version")
	  (string :tag "Java Home")))

(defcustom jdee-jdk
  (if (and (null (getenv
		  (nth 1
		       jdee-java-environment-variables)))
	   jdee-jdk-registry)
      (list (caar jdee-jdk-registry))
    nil)
  "Specifies the JDK version used to develop the current project.

This will be set to nil by default if the Java version
environment variable (see `jdee-java-enviroment-variables') is
set.  Otherwise it defaults to the first JDK registered in
`jdee-jdk-registry'.  If that variable is nil, then this will
default to nil.

The version must be one of the versions listed in the
`jdee-jdk-registry'.  If you specify nil, the JDEE uses the
JDK specified by the Java version environment variable (see
`jdee-java-enviroment-variables'), if set; otherwise, the first JDK
located on the system command path specified by the PATH environment
variable is used (on Mac OS X the default Java installation is tried
first).

You must customize `jdee-jdk-registry' first, then `jdee-jdk'.  After you
have customized jdee-jdk-registry, the customization buffer for`
jdee-jdk' presents you with a set of radio buttons, one for each
registered JDK.  Select the button of the JDK that you want to use for
the current project."
  :group 'jdee-project
  :type 'symbol
  :set-after '(jdee-jdk-registry))

(defun jdee-get-jdk-dir ()
  "Get the root directory of the JDK currently being used by the
JDE. The directory is the directory of the version of the JDK
specified by `jdee-jdk'. If none is specified, this function returns
the value of the Java home environment variable (see
`jdee-java-environment-variables') or the first JDK directory on the
system path, i.e., the directory that contains java on Unix systems or
java.exe on Windows systems.  If neither `jdee-jdk' nor the system path
nor the Java home environment variable specify a JDK directory, this
function displays an error message."
  (interactive)

  (cond
   ;; If jdee-jdk is set, we try to find it in jdee-jdk-registry and
   ;; make sure the directory exists
   (jdee-jdk
    (let* ((jdk-alias (car jdee-jdk))
	   (registry-entry (assoc jdk-alias jdee-jdk-registry)))
      (if (null registry-entry)
	  (error (format
		  "No mapping in the jdee-jdk-registry found for JDK version %s"
		  jdk-alias))
	;; check if directory exists. Originally this was only done if
	;; the string was non-empty I'm not sure why, I have not
	;; preserved that (shyamalprasad)
	(let ((jdk-dir (substitute-in-file-name (cdr registry-entry))))
	  (if (file-exists-p jdk-dir)
	      jdk-dir
	    (error (format "The path specified for JDK %s does not exist: %s"
			   jdk-alias jdk-dir)))))))

   ;; otherwise use JAVA_HOME if set
   ((getenv (nth 1 jdee-java-environment-variables))
    (let ((jdk-dir (substitute-in-file-name
		    (getenv (nth 1 jdee-java-environment-variables)))))
      (if (file-exists-p jdk-dir)
	  jdk-dir
	(error (format "The path specified by %s does not exist: %s"
		       (nth 1 jdee-java-environment-variables) jdk-dir)))))

   ;; otherwise, use Apple Java Policy on Mac OS X
   ((and (eq system-type 'darwin)
	 (file-executable-p "/usr/libexec/java_home"))
    (substring (shell-command-to-string "/usr/libexec/java_home") 0 -1))

   ;; Otherwise default to java in $PATH
   (t
    (let* ((javac (executable-find "javac")))
      (if javac
	  ;; follow symbolic links since gnu/linux systems might be
	  ;; using /etc/alternatives to the final installation
	  (let ((javac-symlink (file-symlink-p javac)))
	    (while javac-symlink
	      (setq javac javac-symlink)
	      (setq javac-symlink (file-symlink-p javac)))
	    (expand-file-name ".." (file-name-directory javac)))
	(error "Cannot find the JDK directory.  See `jdee-jdk'"))))))

(defun jdee-get-jdk-prog (progname)
  "Return the full path of the program `PROGNAME' passed in.
By default, assume it's in the bin directory under `jdee-get-jdk-dir',
but if not, look in the environment's command path."
  (let* ((progname-str
          (if (symbolp progname)
              (symbol-name progname) progname))
         (full-progname
          (if (eq system-type 'windows-nt)
              (concat progname-str ".exe")
            progname-str))
         (progpath
          (expand-file-name
           (concat
            (if (eq system-type 'darwin) "Home/bin/" "bin/")
            full-progname)
           (jdee-get-jdk-dir))))
    (if (file-exists-p progpath)
        progpath
      (executable-find full-progname))))

(defun jdee-get-tools-jar ()
  "Gets the correct tools.jar or equivalent.
Signals an error if it cannot find the jar."
  (let* ((jdk-dir (jdee-get-jdk-dir))
         (tools
	  (expand-file-name
	   (if (eq system-type 'darwin)
	       (let ((classes-jar
		      (cond
		       ((file-exists-p
			 (expand-file-name
			  "Classes/classes.jar" jdk-dir))
			"Classes/classes.jar")
		       ((file-exists-p
			 (expand-file-name
			  "../Classes/classes.jar" jdk-dir))
			"../Classes/classes.jar")
		       ((file-exists-p
			 (expand-file-name
			  "bundle/Classes/classes.jar" jdk-dir))
			"bundle/Classes/classes.jar")
		       ;; starting with 1.7 (Oracle's JDK release) the
		       ;; tools.jar location has become a little more
		       ;; standardized
		       (t "lib/tools.jar"))))
		 classes-jar)
	     "lib/tools.jar")
	   jdk-dir)))
    (if (file-exists-p tools)
	tools
      nil)))

(provide 'jdee-jdk-manager)

;;; jdee-jdk-manager.el ends here
