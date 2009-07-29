;;; jde.el -- Integrated Development Environment for Java.
;; $Revision: 1.357.2.4 $ $Date: 2006/03/09 04:19:37 $ 

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997-2008, 2008 Paul Kinnucan.

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

;; This is one of a set of packages that make up the 
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; The latest version of the JDE is available at
;; <URL:http://jdee.sunsite.dk>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

;;; Code:

;;;###autoload
(defconst jde-version "2.3.6"
  "JDE version number.")

(defconst jde-cedet-min-version "1.0beta2"
  "Cedet minimum version")
(defconst jde-cedet-max-version "1.0"
  "Cedet maximum version")

(defconst jde-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defconst jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))
 
(defconst jde-emacs21p (and (string-match "\\bEmacs\\b" (emacs-version))
			    (>= emacs-major-version 21)))

(defconst jde-emacs22p (and (string-match "\\bEmacs\\b" (emacs-version))
			    (>= emacs-major-version 22)))

(unless (fboundp 'custom-set-default)
   (defalias 'custom-set-default 'set-default)) 

;; Autoloads must be loaded first since move to sole `(require 'jde)' style.
(require 'jde-autoload)

(require 'jde-util)

;; The version of the JDEE distributed with XEmacs has its own
;; autoloads file (auto-autoloads.el). Therefore, require
;; jde-autoloads only if this JDEE version is not part of the XEmacs
;; package.
(unless 
    (and jde-xemacsp
	 (file-exists-p 
	  (expand-file-name 
	   "jde/auto-autoloads.el"
	   (jde-root))))
	 (require 'jde-autoload))

(require 'jde-custom)
(require 'jde-help)
(require 'semantic-load)
(require 'easymenu)
(require 'cl)
(require 'font-lock)
(require 'cc-mode)
(require 'cus-edit)
(require 'comint)
(require 'jde-compile)
(require 'jde-db)
(require 'jde-bug)
(require 'jde-jdb)
(require 'jde-run)
(require 'jde-gen)
(require 'compile)
(require 'imenu)
(require 'browse-url)
(require 'beanshell)
(require 'jde-plugins)
(require 'jde-wiz)
(require 'jde-java-grammar)
(require 'jde-complete)
(require 'jde-which-method)
(require 'jde-java-font-lock)
(require 'jde-import)
(require 'jde-class)
(require 'executable)  ;; in XEmacs' sh-script package
(require 'efc)
(require 'etags)
(require 'jde-open-source)
(require 'jde-annotations)
(require 'regress)

(if (not (fboundp 'custom-set-default))
    (defalias 'custom-set-default 'set-default))


(defgroup jde nil
  "Java Development Environment"
  :group 'tools
  :prefix "jde-")


(defcustom jde-check-version-flag t
  "*Non-nil means to check versions of semantic, eieio, and speedbar.
That is if they meet the requirements for this version of the JDE.
If nil only check if semantic, eieio, and speedbar are available.
See also the function `jde-check-versions'."
  :group 'jde
  :type 'boolean)

;; (makunbound 'jde-key-bindings)
(defcustom jde-key-bindings
  (list 
   (cons "[?\C-c ?\C-v ?\C-a]" 'jde-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'jde-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'jde-compile)
   (cons "[?\C-c ?\C-v ?\C-d]" 'jde-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'jde-find)
   (cons "[?\C-c ?\C-v ?\C-g]" 'jde-open-class-at-point)
   (cons "[?\C-c ?\C-v ?\C-k]" 'jde-bsh-run)
   (cons "[?\C-c ?\C-v ?\C-l]" 'jde-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'jde-help-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'jde-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'jde-wiz-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'jde-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'jde-jdb-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'jde-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-x]" 'jde-show-superclass-source)
   (cons "[?\C-c ?\C-v ?\C-y]" 'jde-open-class-at-point)
   (cons "[?\C-c ?\C-v ?\C-z]" 'jde-import-find-and-import)
   (cons "[?\C-c ?\C-v ?e]"    'jde-wiz-extend-abstract-class)
   (cons "[?\C-c ?\C-v ?f]"    'jde-gen-try-finally-wrapper)
   (cons "[?\C-c ?\C-v ?i]"    'jde-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"    'jde-javadoc-autodoc-at-line)
   (cons "[?\C-c ?\C-v ?o]"    'jde-wiz-override-method)
   (cons "[?\C-c ?\C-v ?t]"    'jde-gen-try-catch-wrapper)
   (cons "[?\C-c ?\C-v ?z]"    'jde-import-all)
   (cons "[?\C-c ?\C-v ?\C-[]" 'jde-run-etrace-prev)
   (cons "[?\C-c ?\C-v ?\C-]]" 'jde-run-etrace-next)
   (cons "[(control c) (control v) (control ?.)]" 'jde-complete)
   (cons "[(control c) (control v) ?.]" 'jde-complete-in-line)
   )
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies 
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jde-project
  :type '(repeat
	  (cons :tag "Key binding"
		(string :tag "Key")
		(function :tag "Command")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (when (boundp 'jde-mode-map)
		 ;; Unmap existing key bindings
		 (if (and (boundp 'jde-key-bindings)
			  jde-key-bindings)
		     (mapc 
		      (lambda (binding)
			(let ((key (car binding)))
			  (if (string-match "\\[.+]" key)
			      (setq key (car (read-from-string key))))
			  (local-unset-key key)))
		      jde-key-bindings))
		 ;; Map new key bindings.
		 (mapc 
		  (lambda (binding)
		    (let ((key (car binding))
			  (fcn (cdr binding)))
		      (if (string-match "\\[.+]" key)
			  (setq key (car (read-from-string key))))
		      (define-key (current-local-map) key fcn)))
		  val))))
	   (jde-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jde-launch-beanshell-on-demand-p t
  "If non-nil, the JDE launches the Beanshell the first time it is needed.
Otherwise, the JDE launches the Beanshell, if it is not already running,
whenever you open a Java source file."
  :group 'jde-project
  :type 'boolean)



(defcustom jde-java-environment-variables '("JAVA_VERSION" "JAVA_HOME")
  "This variable specifies the names of environment variables used to
specify the version and location of the JDK to be used by the JDE.
If set, the `jde-jdk' customization variable overrides the 
java enviroment variables."
  :group 'jde-project
  :type '(list
	  (string :tag "Java Version")
	  (string :tag "Java Home")))

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

;; (makunbound 'jde-jdk-registry)
(defcustom jde-jdk-registry nil
  "Specifies the versions and locations of the JDKs installed on your
system.  For each JDK to be registered, enter the version number
(e.g., 1.4.0) of the JDK in the Version field. Enter the path of the
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

;; (makunbound 'jde-jdk)
(defcustom jde-jdk nil
  "Specifies the version of the JDK to be used to develop the current
project. The version must be one of the versions listed in the
`jde-jdk-registry'. If you specify nil (the default), the JDE uses the
JDK specified by the Java version environment variable (see
`jde-java-enviroment-variables', if set; otherwise, the first JDK
located on the system command path specified by te PATH environment
variable.

You must customize `jde-jdk-registry' first, then `jde-jdk'. After you
have customized jde-jdk-registry, the customization buffer for`
jde-jdk' presents you with a set of radio buttons, one for each
registered JDK.  Select the button of the JDK that you want to use for
the current project."
  :group 'jde-project
  :type 'symbol

  (if (or (featurep 'xemacs) (< emacs-major-version 21))
      :set
    :set-after)

  (if (or (featurep 'xemacs) (< emacs-major-version 21))
      'custom-set-default
    '(jde-jdk-registry)))
 
(defun jde-find-jdk-in-exec-path ()
  "Search for a JDK in `exec-path' and return the path of
the root directory of the first JDK that is found.  Return nil if a
JDK is not found anywhere in exec-path."
  (let ((list exec-path)
	(command "java")
        file)
    (while list
      (setq list
	    (if (and (setq file (expand-file-name command (car list)))
		     (let ((suffixes executable-binary-suffixes)
			   candidate)
		       (while suffixes
			 (setq candidate (concat file (car suffixes)))
			 (if (and (file-executable-p candidate)
				  (not (file-directory-p candidate)))
			     (setq suffixes nil)
			   (setq suffixes (cdr suffixes))
			   (setq candidate nil)))
		       (setq file candidate)))
		nil
	      (setq file nil)
	      (cdr list))))
    file))


(defun jde-get-jdk-dir () 
  "Get the root directory of the JDK currently being used by the
JDE. The directory is the directory of the version of the JDK
specified by `jde-jdk'. If none is specified, this function returns
the value of the Java home environment variable (see
`jde-java-environment-variables') or the first JDK directory on the
system path, i.e., the directory that contains java on Unix systems or
java.exe on Windows systems.  If neither `jde-jdk' nor the system path
nor the Java home environment variable specify a JDK directory, this
function displays an error message."
  (interactive)

  (if jde-jdk
      (let* ((jdk (assoc (car jde-jdk) jde-jdk-registry))
	     (jdk-dir (cdr jdk)))
        (when (null jdk)
          (error (format
                  "No mapping in the jde-jdk-registry found for JDK version %s"
                  (car jde-jdk))))
	(if (not (string= jdk-dir ""))
	    (progn
	      (setq jdk-dir (substitute-in-file-name jdk-dir))
	      (if (not (file-exists-p jdk-dir))
		  (error 
		   (format "The path specified for JDK %s does not exist: %s"
			   jde-jdk
			   jdk-dir)))))
	jdk-dir)
    (let ((jdk-dir (getenv (nth 1 jde-java-environment-variables))))
      (if jdk-dir
	  (progn
	    (setq jdk-dir (substitute-in-file-name jdk-dir))
	    (if (not (file-exists-p jdk-dir))
		(error 
		 (format "The path specified by %s does not exist: %s"
			 (nth 1 jde-java-environment-variables)
			 jdk-dir))))
	(progn 
	  (setq jdk-dir 
		(executable-find "javac"))
	  (if jdk-dir
	      (setq jdk-dir 
		    (expand-file-name 
		     ".." 
		     (file-name-directory jdk-dir)))
	    (error "Cannot find the JDK directory. See `jde-jdk'."))))
      jdk-dir)))


(defun jde-get-jdk-prog (progname)
   "Returns the full path of the program passed in.  By default, assume
   it's in the bin directory under `jde-get-jdk-dir', but if not,
   look in the environment's command path."
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
	    (jde-get-jdk-dir))))
     (if (file-exists-p progpath)
       progpath
       (executable-find full-progname))))


(defun jde-get-tools-jar ()
  "Gets the correct tools.jar or equivalent. Signals an
error if it cannot find the jar."
  (let ((tools 
         (expand-file-name 
          (if (eq system-type 'darwin)
              "Classes/classes.jar"
            "lib/tools.jar")
          (jde-get-jdk-dir))))
    (if (file-exists-p tools)
        tools
      (error (concat "Cannot find JDK's tools jar file (or equivalent)." 
                     "Type M-x describe-function [RET] jde-get-jdk-dir for more info.")))))

(defvar jde-java-version-cache nil
"Cache to hold the version of Java being used.")

(defun jde-java-version-via-java ()
  "Get the version of the java vm on the
system command path."
  (if (not jde-java-version-cache)
      (let ((buf (get-buffer-create "java version"))
	    proc)
	(save-excursion
	  (set-buffer buf)
	  (setq proc
		(start-process
		 "java version" buf "java" "-version"))
	  (accept-process-output proc 10)
	  (goto-char (point-min))
	  (re-search-forward "[1-9][.][1-9]" (point-max) t)
	  (setq jde-java-version-cache (match-string 0)))
	(kill-buffer buf)))
  jde-java-version-cache)

(defun jde-java-version ()
  "Get the version of Java used by the JDE."
  (interactive)
  (let ((java-version (if jde-jdk (car jde-jdk)
			(getenv 
			 (nth 0 jde-java-environment-variables)))))
    (if (not java-version)
	(if jde-java-version-cache
	    (setq java-version jde-java-version-cache)
	  (if (jde-bsh-running-p)
	      (progn
		(setq jde-java-version-cache
		      (jde-jeval-r "jde.util.JdeUtilities.getJavaVersion();"))
		(setq java-version jde-java-version-cache))
	    (setq java-version (jde-java-version-via-java)))))	    
    (if (interactive-p)
      (message java-version)
      java-version)))

(defun jde-java-major-version ()
  "Returns an integer representing
the major version of the JDK being used
by the current project."
  (let ((version (jde-java-version)))
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)"
		version)
    (string-to-int
     (substring 
     version 
     (match-beginning 1)
     (match-end 1)))))

(defun jde-java-minor-version ()
  "Returns an integer representing
the minor version of the JDK being used
by the current project."
  (let ((version (jde-java-version)))
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)"
		version)
    (string-to-int
     (substring 
     version 
     (match-beginning 2)
     (match-end 2)))))


;;(makunbound 'jde-jdk-doc-url)
(defcustom jde-jdk-doc-url ""
  "*URL of JDK documentation. 
This can point to a remote or local copy of the documentation. If the value
of this variable is the null string, the JDE looks for the JDK documentation
in the docs subdirectory of the directory returned by `jde-get-jdk-dir'."
  :group 'jde-project
  :type 'file)

;;(makunbound 'jde-global-classpath)
(defcustom jde-global-classpath nil
  "Specify a common classpath for compile, run, and debug commands.
Use this variable if you want to the JDE to use the same classpath for
compiling, running,and debugging an application. Note that the value
of this variable is a list of strings, each of which specifies a
path. The JDE converts this list to a colon- or semicolon-separated
list before inserting in the compiler or vm command line. 

The path may start with a tilde (~) or period (.) and may include
environment variables. The JDEE replaces a ~ with your home directory.
If `jde-resolve-relative-paths-p' is nonnil, the JDEE replaces the
. with the path of the current project file. The JDEE replaces each
instance of an environment variable with its value before inserting it
into the command line.

You can specify different classpaths for compiling, running and
debugging applicaitons. Use `jde-compile-option-classpath' to specify
the compilation classpath, `jde-run-option-classpath' to specify the
run classpath, and/or `jde-db-option-classpath' to specify the debug
classpath. You can use these variables together. For example, suppose
that you need to use one classpath for compilation and other for
running and debugging. You could do this by setting
`jde-compile-option-classpath' to the compile classpath and
`jde-global-classpath' to the run and debug classpath. If you set
`jde-global-classpath', the JDE uses it to construct the classpath for
any operation for which you do not set the operation-specific
classpath variable (e.g., `jde-compile-option-classpath'). 

If you do not set `jde-global-classpath', the JDE uses the operation-specific
classpath if it is set. If neither the global nor the
operation-specific classpath is set, the JDE does not generate a
-classpath argument for the operation, e.g., compile or run a Java
class. In this case, the operation uses the value of the CLASSPATH variable
if specified."
  :group 'jde-project
  :type '(repeat (file :tag "Path")))

(defcustom jde-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jde-project
  :type 'boolean)

(defcustom jde-expand-classpath-p t
  "Replace each occurence of a directory named `jde-lib-directory-names'
 in the classpath with paths to the jar and zip files in that directory."
  :group 'jde-project
  :type 'boolean)

;; (makunbound 'jde-lib-directory-names)
(defcustom jde-lib-directory-names (list "^lib" "^jar")
  "Regular expressions that matches names of jar/zip directories for 
the current project. See `jde-expand-classpath-p' and
`jde-expand-classpath' for more information"
  :group 'jde-project
  :type '(repeat (string :tag "Name")))

(defcustom jde-lib-excluded-file-names nil
   "Regular expressions that matches names of jar or zip files that should
 be excluded when expanding a library specified by `jde-lib-directory-names'."
   :group 'jde-project
   :type '(repeat (string :tag "Name")))


;; (makunbound 'jde-sourcepath)
(defcustom jde-sourcepath nil
  "*List of source directory paths.  The JDE uses this list to locate
source files corresponding to class files.  When entering paths in the
custom buffer, enter each path as a separate item in a separate edit
field. Do NOT put more than one path in the same edit field. You'll
only confuse JDE.  Paths may contain environment variables."
  :group 'jde-project
  :type '(repeat (file :tag "Path")))


;; (makunbound 'jde-build-function)
(defcustom jde-build-function '(jde-make)
  "*Function that will be invoked by the `jde-build' command.
The `jde-make' function uses a make 
program to rebuild the project. The `jde-ant-build' function
uses the Apache Ant program to build the project. You may also
specify a custom function to use. The custom function must
be an interactive function that can be called by 
`call-interactively'."
  :group 'jde-project
  :type '(list
	  (radio-button-choice
	   :format "%t \n%v"
	   :tag "Function: "
	   :entry-format " %b %v"
	   (const jde-make)
	   (const jde-ant-build)
	   (function my-custom-build-function))))

;;(makunbound 'jde-debugger)
(defcustom jde-debugger (list "jdb")
  "Specify the debugger you want to use to debug Java programs.
Select JDEbug, if you want to use the JDE's builtin debugger.  Select
jdb, if you want to use the default version of jdb for the JDK used by
the current project (see `jde-jdk'). Select old jdb, if you are using
JDK 1.2.2 or later and want to use the the old (e.g., pre-JPDA)
version of jdb instead of the new (JPDA-based) version of jdb."
  :group 'jde-project
  :type '(list 
	  (radio-button-choice
	  (item "JDEbug")
	  (item "jdb")
	  (item "old jdb")))
  :set '(lambda (sym val)
	  (mapc 
	   (lambda (buff)
	     (save-excursion	       
	       (set-buffer buff)
	       (if (string= (car val) "JDEbug")
		   (progn
		     (jde-jdb-minor-mode -1)
		     (jde-bug-minor-mode 1))
		 (progn
		   (jde-jdb-minor-mode 1)
		   (jde-bug-minor-mode -1)))))
	   (jde-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jde-devel-debug nil
  "If true, use the JDEE Java classes in the jde/java/classes
directory instead of the jde.jar. This variable is intended for
use in testing the JDEE's java classes."
  :group 'jde-project
  :type 'boolean)


;; (makunbound 'jde-enable-senator)
(defcustom jde-enable-senator t
  "Enable senator minor mode.
This mode provides Java-aware buffer navigation and searching
commands."
  :group 'jde-project
  :type 'boolean
  :set '(lambda (sym val)
	  (set-default sym val)
          (unless (or (not (featurep 'jde)) ;; skip initial set.
		      jde-loading-project ;; skip when set by project loading system
		      (and (boundp 'global-senator-minor-mode)
			   global-senator-minor-mode))
	    (mapc 
	     (lambda (buff)
	       (save-excursion	       
		 (set-buffer buff)
		 (senator-minor-mode (if val 1 -1))))
	     (jde-get-java-source-buffers)))))

	 
(defcustom jde-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jde-mode.
See `jde-mode-abbreviations' for more information."
  :group 'jde-project
  :type 'boolean
  :set '(lambda (sym val)
	  (set-default sym val)
          (if (featurep 'jde) ;; skip initial set.
	      (mapc
	       (lambda (buf)
		(with-current-buffer buf
		  (setq abbrev-mode val)
		  (when abbrev-mode
		    (setq local-abbrev-table (make-abbrev-table))
		    (jde-init-abbrev-table))))
	       (jde-get-project-source-buffers)))))

(defcustom jde-mode-abbreviations
  (list 
   (cons "ab" "abstract")
   (cons "bo" "boolean")
   (cons "br" "break")
   (cons "by" "byte")
   (cons "byv" "byvalue")
   (cons "cas" "cast")
   (cons "ca" "catch")
   (cons "ch" "char")
   (cons "cl" "class")
   (cons "co" "const")
   (cons "con" "continue")
   (cons "de" "default")
   (cons "dou" "double")
   (cons "el" "else")
   (cons "ex" "extends")
   (cons "fa" "false")
   (cons "fi" "final")
   (cons "fin" "finally")
   (cons "fl" "float")
   (cons "fo" "for")
   (cons "fu" "future")
   (cons "ge" "generic")
   (cons "go" "goto")
   (cons "impl" "implements")
   (cons "impo" "import")
   (cons "ins" "instanceof")
   (cons "in" "int")
   (cons "inte" "interface")
   (cons "lo" "long")
   (cons "na" "native")
   (cons "ne" "new")
   (cons "nu" "null")
   (cons "pa" "package")
   (cons "pri" "private")
   (cons "pro" "protected")
   (cons "pu" "public")
   (cons "re" "return")
   (cons "sh" "short")
   (cons "st" "static")
   (cons "su" "super")
   (cons "sw" "switch")
   (cons "sy" "synchronized")
   (cons "th" "this")
   (cons "thr" "throw")
   (cons "thro" "throws")
   (cons "tra" "transient")
   (cons "tr" "true")
   (cons "vo" "void")
   (cons "vol" "volatile")
   (cons "wh" "while")
   )
"*Abbreviations used for Java keywords.
To use these abbreviations, you must enable abbrev-mode (see
`jde-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
  :group 'jde-project
  :type '(repeat 
          (cons :tag "jde-mode abbreviation"
              (string :tag "Abbreviation")
              (string :tag "Expansion")))
  :set '(lambda (sym val)
	  (set-default sym val)
          (if (and
	       (featurep 'jde)
	       jde-enable-abbrev-mode)
	      (progn
		(mapc
		 (lambda (buf)
		   (with-current-buffer buf
		     (setq local-abbrev-table (make-abbrev-table))
		     (jde-init-abbrev-table)))
		 (jde-get-project-source-buffers))))))

(defun jde-init-abbrev-table ()
  "Load the abbrev table.
Load it with a set of abbrevs that invoke an anonymous function that
does the expansion only if point is not in a quoted string or a
comment."

  ;; Note the use of lexical-let - must have the common lisp packages
  ;; around, since the anonymous function needs the closure provided by
  ;; lexical-let.
  (interactive)
  (mapc 
   (lambda (x)
     (lexical-let
         ((abbrev (car x))     ; this is the abbrev, lexically scoped
          (expansion (cdr x))) ; this is the expansion
       (define-abbrev
         local-abbrev-table
         abbrev
         (if (featurep 'xemacs) abbrev t)
         (lambda ()
           (unless (jde-parse-comment-or-quoted-p)
             (delete-backward-char (length abbrev)) ; remove abbreviation and
             (insert expansion)))                   ; insert expansion
	 0)))
   jde-mode-abbreviations)

  (if jde-gen-cflow-enable
      (jde-gen-load-abbrev-templates))

  (setq abbrevs-changed nil))

;; The next two functions contributed by s.nicolas@videotron.ca
(defun jde-abbrev-mode ()
"*Toggle abbreviation mode in JDE without altering project settings.
See `jde-mode-abbreviations' for more information."
 (interactive)
  (setq jde-enable-abbrev-mode (not jde-enable-abbrev-mode))
  (setq abbrev-mode jde-enable-abbrev-mode)
  (when jde-enable-abbrev-mode
     ;; Define abbreviations.a
    (jde-init-abbrev-table))
  (if jde-enable-abbrev-mode
      (message "abbreviation mode on")
    (message "abbreviation mode off")))

(defun jde-show-abbrevs ()
"*Shows a popup menu containing all available expansions.
See `jde-mode-abbreviations' for more information."
  (interactive)
   (let* ((expansions
          (mapcar
            (lambda(x) (cons (cdr x) (car x)))
              jde-mode-abbreviations))
         (expansion (car (imenu--mouse-menu expansions (if jde-xemacsp nil
t) "Abbreviations"))))
  (insert expansion)))


(defvar jde-classpath-separator (if (member system-type '(cygwin32 cygwin))
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")


;;;###autoload
(defun jde-set-global-classpath (classpath)
  "Set the value of `jde-global-classpath'.
It specifies the -classpath argument for the Java compiler and
interpreter."
  (interactive 
   "sEnter classpath: ")
  (custom-set-variables 
   '(jde-global-classpath (split-string classpath jde-classpath-separator) t)))	
		
(defun jde-show-run-options ()
  "Show the JDE Run Options panel."
  (interactive)
  (customize-apropos "jde-run-options" 'groups))

(defun jde-show-debug-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-db-options" 'groups))

(defun jde-show-project-options ()
  "Show the JDE Debug Options panel."
  (interactive)
  (customize-apropos "jde-project" 'groups))

(defun jde-show-autocode-options ()
  "Show the JDE Autocode panel."
  (interactive)
  (customize-apropos "jde-gen" 'groups))

(defun jde-show-wiz-options ()
  "Show the JDE Wizards Options panel."
  (interactive)
  (customize-apropos "jde-wiz" 'groups))

(defun jde-show-complete-options ()
  "Show the JDE Complete Options panel."
  (interactive)
  (customize-apropos "jde-complete" 'groups))

;;;###autoload
(defun jde-build ()
  "Rebuild the entire project.
This command invokes the function defined by `jde-build-function'."
  (interactive)
  (call-interactively (car jde-build-function)))

; (define-derived-mode 
;   jde-mode java-mode "JDE"
;   "Major mode for developing Java applications and applets.
;   \\{jde-mode-map}"
;   (jde-mode-internal)
; )

;; The following is the expansion of the above macro.
;; We include the expansion to permit automatic
;; loading of the JDE.
(derived-mode-init-mode-variables 'jde-mode)
(put 'jde-mode 'derived-mode-parent 'java-mode)

;;;###autoload
(defun jde-mode ()
  "Major mode for developing Java applications and applets.
\\{jde-mode-map}"
  (interactive)
  (condition-case err
      (progn 
        (jde-check-versions)
        (java-mode)
        (if (get 'java-mode 'special)
            (put 'jde-mode 'special t))
        (setq major-mode 'jde-mode)
        (setq mode-name "JDE")
        (derived-mode-set-keymap 'jde-mode)
        (derived-mode-set-syntax-table 'jde-mode)
        (derived-mode-set-abbrev-table 'jde-mode)

        ;; Define buffer-local variables.
        (make-local-variable 'jde-project-name)
        (make-local-variable 'jde-run-applet-document)

        (setq jde-current-project 
              (or (jde-find-project-file default-directory)
                  "")) ;; Avoid setting startup values twice!

	(setq jde-buffer-project-file jde-current-project)

        ;; Load the project file for this buffer. The project file
        ;; defines JDE options for a project.
        (if (and (not (jde-debugger-running-p)) jde-project-context-switching-enabled-p)
            (jde-load-project-file))

        ;; Enable support for automatic project switching.
        ;; This feature loads the appropriate project settings whenever
        ;; a user switches from a Java buffer belonging to one project
        ;; to a buffer belonging to another.
	(make-local-hook 'post-command-hook) ;; necessary for XEmacs (see below)
        (add-hook 'post-command-hook
                  'jde-detect-java-buffer-activation 
		  nil 
		  t ;; XEmacs ignores this argument if symbol is not local.
		  )

	(unless jde-monitor-post-command-hook-timer
	  (setq 
	   jde-monitor-post-command-hook-timer
	   (run-with-idle-timer 1 t 'jde-monitor-post-command-hook)))

	(unless (member 'jde-clean-up-after-jde kill-buffer-hook)
	  (add-hook 'kill-buffer-hook 'jde-clean-up-after-jde))
			
	(when jde-xemacsp
	  (require 'jde-xemacs)
	  (jde-insert-menu-in-xemacs-menubar))

        ;; Define underscore as a word constituent. This is needed
        ;; to support coding styles the begin fields with an underscore.
        (modify-syntax-entry ?_ "w")

        (when jde-enable-abbrev-mode
          ;; Define abbreviations.
          (jde-init-abbrev-table)
          (abbrev-mode 1))

        ;; Reset the key bindings in case jde-mode-keymap
        ;; was not bound at startup.
        (custom-initialize-reset 'jde-key-bindings nil)

        (if (and
             jde-setnu-mode-enable
             (< (point-max) jde-setnu-mode-threshold))
            (setnu-mode 1))

        (make-local-variable 'mode-line-format)
        (setq mode-line-format jde-mode-line-format)
  
        ;; When looking for a tag that has multiple matches
        ;; in the TAGS file, prefer (find first) the
        ;; occurrence in the _current_ buffer.
        ;; Contributed by Charles Rich, Mitsubishi Electric Research Laboratories,
        ;; Cambridge, MA>
	(when (boundp 'tags-table-format-functions)
	  (make-local-variable 'tags-table-format-functions)
	  (add-hook 'tags-table-format-functions 'jde-etags-recognize-tags-table nil t))

        (if (and
             (not jde-launch-beanshell-on-demand-p)
             (not (jde-bsh-running-p)))
            (bsh-launch (oref 'jde-bsh the-bsh)))

        (jde-wiz-set-bsh-project)

        ;; Setup Semantic stuff needed by the JDEE when Semantic is ready to
        ;; parse!
        (add-hook 'semantic-init-hooks  'jde-parse-semantic-default-setup)

	;; Install debug menu.
	(if (string= (car jde-debugger) "JDEbug")
	    (jde-bug-minor-mode 1)
	  (jde-jdb-minor-mode 1))

	;; Install plugin menu.
	(jde-plugin-minor-mode 1)

	(when (boundp 'jde-mode-map)
	  (let ((key (car (read-from-string "[return]"))))
	   (if jde-electric-return-mode
	     (define-key (current-local-map) key 'jde-electric-return))))

	;; Set up indentation of Java annotations.
        (jde-annotations-setup)


        ;; The next form must be the last executed
        ;; by jde-mode.
        (derived-mode-run-hooks 'jde-mode))    
    (error
     (message "%s" (error-message-string err)))))

                       
(defconst jde-check-versions-message
   "JDEE requires a version of CEDET between %s and %s (found %s)")
 
(defun jde-check-versions () 
  "Check for correct versions of CEDET provided packages.
Signal an error if CEDET is not installed.
When `jde-check-version-flag' is non-nil, signal an error if the
version of CEDET currently installed doesn't meet the requirements for
this version of the JDEE."
  ;; Check that CEDET is installed.
  (or (boundp 'cedet-version)
      (error jde-check-versions-message
             jde-cedet-min-version
             jde-cedet-max-version
             "none"))
  ;; Check version requirement when requested.
  (or (not jde-check-version-flag)
      (jde-check-version cedet-version
                         jde-cedet-min-version
                         jde-cedet-max-version)
      (error jde-check-versions-message
             jde-cedet-min-version
             jde-cedet-max-version
             cedet-version)))


(defun jde-check-version (current-version min-version max-version)
  "Return non-nil if CURRENT-VERSION >= MIN-VERSION or <= MAX-VERSION."
  (and (or (jde-earlier-versionp current-version 
                                 max-version)
           (string= current-version
                    max-version))
       (or (jde-earlier-versionp min-version
                                 current-version)
           (string= current-version
                    min-version))))

(defun jde-earlier-versionp (ver1 ver2) 
  "Return non-nil if VER1 is earlier than VER2"
  (let ((ver1n (replace-in-string ver1 "beta" "zb"))
	(ver2n (replace-in-string ver2 "beta" "zb")))
    (setq ver1n (replace-in-string ver1n "pre" "zp"))
    (setq ver2n (replace-in-string ver2n "pre" "zp"))
    (if (string-match "z" ver1n)
	(unless (string-match "z" ver2n)
	  (setq ver2n (concat ver2n "zz")))
      (if (string-match "z" ver2n)
	  (setq ver1n (concat ver1n "zz"))))
    (string< ver1n ver2n)))

      
(defcustom jde-log-max 500
  "*Maximum number of lines to keep in the JDE log buffer.
If nil, disable logging.  If t, don't truncate the buffer."
  :group 'jde-project
  :type '(choice (integer :tag "Number of lines to keep")
		 (boolean :tag "Disable/Unlimited")))

(defun jde-log-msg (msg &rest args)
  "Log message MSG to the *jde-log* buffer.
Optional ARGS are used to `format' MSG.
Does nothing if `jde-log-max' is nil."
  (if jde-log-max
      (save-match-data
	(save-excursion
	  (set-buffer (get-buffer-create "*jde-log*"))
	  (goto-char (point-max))
	  (insert (apply 'format msg args))
	  (insert "\n")
	  (if (integerp jde-log-max)
	      (let ((line-cnt 0))
		(while (search-backward "\n" nil t)
		  (setq line-cnt (1+ line-cnt)))
		(goto-char (point-min))
		(while (> line-cnt jde-log-max)
		  (delete-region (point) (search-forward "\n" nil t))
		  (setq line-cnt (1- line-cnt)))))))))

(defun jde-log-msg-t (msg &rest args)
  "Log message MSG to the *jde-log* buffer, and return t.
Optional ARGS are used to `format' MSG.
Does nothing but return t if `jde-log-max' is nil."
  (jde-log-msg msg args)
  t)

(defun jde-log-msg-nil (msg &rest args)
  "Log message MSG to the *jde-log* buffer, and return nil.
Optional ARGS are used to `format' MSG.
Does nothing but return nil if `jde-log-max' is nil."
  (jde-log-msg msg args)
  nil)

;; Make jde-mode the default mode for Java source code buffers.
;; Prepend the jde-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.java\\'" . jde-mode))

(defcustom jde-menu-definition 
  (list "JDE"
	["Compile"           jde-compile t]
	;; ["Run App"           jde-run (not (jde-run-application-running-p))]
	["Run App"           jde-run t]
	["Debug App"         jde-debug t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        jde-run-menu-run-applet t]
	["Debug Applet"      jde-debug-applet t]
	"-"  
	["Build"             jde-build t]
	(list "Find"
	      ["Expression"    jde-find 
	                          (and 
				   (executable-find
				    (if (eq system-type 'windows-nt) "find.exe" "find"))
				   (executable-find
				    (if (eq system-type 'windows-nt) "grep.exe" "grep")))]
	      ["Expression..."  jde-find-dlg
	                          (and 
				   (executable-find
				    (if (eq system-type 'windows-nt) "find.exe" "find"))
				   (executable-find
				    (if (eq system-type 'windows-nt) "grep.exe" "grep")))]
	       ["Symbol Definition" jde-open-class-at-point t]
	       ["Class"  jde-show-class-source t]
	       ["Super Class"  jde-show-superclass-source t]
	       ["Interface"  jde-show-interface-source t]
	      )
	(list "Interpreter"
	      ["Start"         jde-bsh-run t]
	      ["Exit"          jde-bsh-exit t]
	      "-"
	      ["Help"          jde-help-beanshell t]
	 )
        (list "Documentation"
	      ["Add"             jde-javadoc-autodoc-at-line (jde-javadoc-enable-menu-p)]
	      ["Remove"          jde-javadoc-remdoc-at-line (jde-javadoc-enable-menu-p)]
	      ["Check This"      jde-javadoc-checkdoc-at-line (jde-javadoc-enable-menu-p)]
	      ["Check All"           jde-javadoc-checkdoc t]
	      ["Generate All"        jde-javadoc-make t]
	      ["Generate Buffer"     jde-javadoc-make-buffer t]
	      "-"
	      ["Javadoc Reference"     jde-javadoc-browse-tool-doc t]
	)
        "-" 
	(list "Code Generation"
	      (list "Templates"
		    ["Get/Set Pair..."  jde-gen-get-set t]
		    ["Println..."       jde-gen-println t]
		    (list "Listener"
			  ["Action"          jde-gen-action-listener t]
                          ["Change"          jde-gen-change-listener t]
			  ["Window"          jde-gen-window-listener t]
			  ["Mouse"           jde-gen-mouse-listener t]
			  )
		    ["Other..."        jde-gen-code t]
		    )
	      (list "Import"
		    ["Class..."                jde-import-\find-and-import t]
		    ["All"                     jde-import-all t]
		    ["All Unique"              jde-import-all-unique t]
		    "-"
		    ["Expand Package Imports"  jde-import-expand-imports t]
		    ["Collapse Class Imports"  jde-import-collapse-imports t]
		    ["Delete Unneeded"         jde-import-kill-extra-imports t]
		    ["Organize Imports"        jde-import-organize t]
		    ["Show Unimported Classes" jde-import-all-show t]
		    )
	      (list "Wizards"
		    ["Override Method"             jde-wiz-override-method t]
		    ["Implement Interface..."      jde-wiz-implement-interface t]
		    ["Generate Get/Set Methods"    jde-wiz-get-set-methods t]
		    ["Generate toString Method"    jde-wiz-tostring t]
                    ["Update Package Statement"    jde-package-update t]
		    ["Implement Event Source..."   jde-wiz-implement-event-source t]
		    ["Extend Abstract Class..."    jde-wiz-extend-abstract-class t]
		    ["Delegate Methods..."         jde-wiz-delegate t]
		    "-"
		    ["Update Class List"   jde-wiz-update-class-list t]
		    )
	      (list "Modes"
		    (vector "Abbrev"
			    'jde-abbrev-mode
			    (if (featurep 'xemacs) :active :enable) t
			    :style 'toggle
			    :selected 'jde-enable-abbrev-mode)
		    (vector "Electric Return"
			    'jde-electric-return-mode
			    (if (featurep 'xemacs) :active :enable) t
			    :style 'toggle
			    :selected 'jde-electric-return-mode)
	      ))
	(list "Browse"
              ["Source Files"          jde-show-speedbar t]
              ["Class at Point"        jde-browse-class-at-point t]
             )
	["Check Style"  jde-checkstyle]
	(list "Project"
	      (vector "Auto Switch" 
                      'jde-toggle-project-switching
                      (if jde-xemacsp :active :enable) t 
                      :style 'toggle 
                      :selected 'jde-project-context-switching-enabled-p)
	      (list "Options"
		    ["General"         jde-show-project-options t]
		    ["Compile"         jde-compile-show-options-buffer t]
		    ["Run"             jde-show-run-options t]
		    ["Debug"           jde-show-debug-options t]
		    ["Autocode"        jde-show-autocode-options t]
		    ["Javadoc"         jde-javadoc-customize t]
		    ["Make"            jde-make-show-options t]
		    ["Ant"             jde-ant-show-options t]
		    ["Complete"        jde-show-complete-options t]
		    ["Wiz"             jde-show-wiz-options t]
		    )
	      (list "Project File"
		    ["Create New" jde-create-new-project t]
		    ["Save"     jde-save-project t]
		    ["Load"     jde-load-project-file t]
		    ["Load All" jde-load-all-project-files t]
		    )
	      )
	(list "Help"
	      ["JDEE Users Guide"      jde-show-help t]
	      ["JDK"                   jde-help-browse-jdk-doc t]
	      ["JDEE Key Bindings"     jde-keys t]
	      "-"
	      ["Class..."              jde-help-class t]
	      ["Class Member..."       jde-help-class-member t]
	      ["Symbol at Point"       jde-help-symbol t]
	      "-"
	      ["Submit problem report" jde-submit-problem-report t]
	      "-"
	      (concat "JDE " jde-version)
	      )
	)
  "*The JDE main menu"
  :group 'jde-project
  :type 'sexp
  :set '(lambda (sym val)
	  (set-default sym val)
	  ; Define JDE menu for FSF Emacs.
	  (if (or (not jde-xemacsp) (featurep 'infodock))
	      (easy-menu-define jde-menu 
				jde-mode-map
				"Menu for JDE."
				val))
	  (if (and jde-xemacsp 
                   (eq major-mode 'jde-mode))
	      (jde-insert-menu-in-xemacs-menubar))))


(defun jde-insert-menu-in-xemacs-menubar ()
  "Insert JDE menu in the XEmacs menu bar."
  (if (and 
       (not (featurep 'infodock))
       (not (memq 'infodock c-emacs-features))
       (boundp 'current-menubar)
       current-menubar)
      (if (fboundp 'add-submenu)
	  (add-submenu nil jde-menu-definition)
	(add-menu nil "JDE" (cdr jde-menu-definition)))))


(defcustom jde-new-buffer-menu 
  (list
   "JDE New"
   ["Class..."         jde-gen-class-buffer t]
   ["Interface..."     jde-gen-interface-buffer t]
   ["Console..."       jde-gen-console-buffer t]
   ["Bean..."          jde-gen-bean-buffer t]
   ["Unit Test..."     jde-junit-test-class-buffer t]
   (list
    "EJB"
    ["Session Bean"    jde-ejb-session-bean-buffer t]
    ["Entity Bean"     jde-ejb-entity-bean-buffer t])
   (list
    "Build file..."
    ["Makefile"        jde-gen-makefile-buffer t]
    ["Ant buildfile"   jde-gen-ant-buildfile-buffer t])
   ["Other..."         jde-gen-buffer t]
   )
  "*The JDE New buffer menu"
  :group 'jde-project
  :type 'sexp
  :set '(lambda (sym val)
	  (set-default sym val)
	  (if jde-xemacsp
	      (unless (featurep 'infodock)
		(when (fboundp 'add-submenu) 
		  (add-submenu '("File") val "Insert File...")))
	    (let* ((menu (if (fboundp 'easy-menu-create-menu)
			     (easy-menu-create-menu 
			      (car val) (cdr val))))
		   (menu-name (car val)))
	      (define-key-after menu-bar-file-menu [jde-new]
		(cons menu-name menu)
		  'open-file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Classpaths                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-cygpath (path &optional direction)
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
		(substitute ?\/ ?\\ (remove ?\n output)))))
	(error "Cannot find cygpath executable."))
    path))

(defvar jde-cygwin-root-cache nil
  "Cache of converted cygwin root directory paths.")

(defun jde-cygwin-path-converter-cygpath (path)
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
	      (converted-root (cdr (assoc root jde-cygwin-root-cache))))
	 (if (not converted-root)
	   (progn
             (setq converted-root (jde-cygpath root))
	     (if converted-root
		 (add-to-list 'jde-cygwin-root-cache 
			      (cons root converted-root))
	       (error "Cannot convert %s" path))))	 
	 (if (string= rest "")
	     converted-root
	   (concat converted-root rest))))
     (t
      (error "Cannot convert %s" path)))))
	 	 

(defun jde-cygwin-path-converter-internal (path)
  "Convert cygwin style PATH to a form acceptable to java vm.  Basically
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
unless they begin with '//[a-z]/' or '/cygdrive/[a-z]/'."
  (interactive "sPath: ")
  (if (fboundp 'mswindows-cygwin-to-win32-path)
      (substitute ?/ ?\\ (mswindows-cygwin-to-win32-path path))
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
	    (substitute ?\\ ?\/ new-path))
	path))))

(defcustom jde-cygwin-path-converter '(jde-cygwin-path-converter-internal)
  "Function to use to convert cygwin paths to DOS paths.  
Choose jde-cygwin-path-converter-internal, jde-cygwin-path-converter-cygpath,
or \"custom-function.\" jde-cygwin-path-converter-cygpath handles all
cygwin-style paths, including mount points, e.g.,/bin. 
jde-cygwin-path-converter-internal does not handle mount
paths. However, it is much faster as it does not require running a
subprocess every time the JDE needs to convert a path. Choose
\"custom-function\" if you want the JDE to use a function that you
supply. Replace \"custom-function\" with the name of the function that
you want to use."
  :group 'jde-project
  :type  '(list
	   (radio-button-choice :format "%t \n%v"
			       :tag "Converter: "
			       :entry-format "  %b %v"
			       (const jde-cygwin-path-converter-internal)
			       (const jde-cygwin-path-converter-cygpath)
			       (function custom-function))))
		       

(defun jde-convert-cygwin-path (path &optional separator)
  "Convert cygwin style PATH to a form acceptable to java vm, using
the conversion function specified by `jde-cygwin-path-converter'."
  (interactive "sPath: ")
  (funcall (car jde-cygwin-path-converter) 
	   (if separator (substitute ?\: (string-to-char separator) path) path)))

(defcustom jde-resolve-relative-paths-p t
  "If this variable is non-nil, the JDE converts relative paths to 
absolute paths. The JDE does this by appending the relative path to the path
of the project file for the current source buffer, if such
a file exists. Otherwise, the JDE appends the relative path to the path
of the current directory."
  :group 'jde-project
  :type 'boolean)

(defun jde-normalize-path (path &optional symbol) 
  "This function performs the following transformation on PATH:

  * Replaces environment variables of the form $VAR or ${VAR} with
    their values. Note that you must use the Unix notation for
    environment variables on the native Windows versions of Emacs and
    XEmacs.

  * Replaces the tilde character with the value of the home directory,
    typically specified by the HOME environment variable.

  * Converts Cygwin style paths to DOS notation on Windows.

  * Converts relative paths to absolute paths if
    `jde-resolve-relative-paths-p' is non-nil.  Paths are resolved
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
	 jde-resolve-relative-paths-p
	 (> len 0)
	 (eq (aref p 0) ?.))
	(let* (prj-file-path
	       (dir (file-name-directory (or (buffer-file-name)
					     default-directory))))
	  ;; find the deepest originating project for the symbol
	  ;; based on the current directory, and resolve to that
	  ;; project's directory
	  (if symbol
	      (let ((prjs (get symbol 'jde-project))
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
			  (jde-find-project-file dir))))
	    (setq prj-file-path 
		  (jde-find-project-file dir)))
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
      ;; jde-resolve-relative-paths-p is false.
      (if (not 
	   (or
	    (string= p ".")
	    (string-match "[.]/" p)))
	  (setq p (expand-file-name p))))
    (setq p (jde-convert-cygwin-path p))
    p))

(defun jde-directory-files-recurs (dir &optional include-regexp)
  "Get all the files in DIR, and any subdirectories of DIR, whose
names match INCLUDE-REGEXP."
  (let (files)
    (loop for file in (directory-files dir) do
          (if (not (member file '("." "..")))
	      (let ((file (concat dir "/" file)))
              (if (file-directory-p file)
                  (setq files (append files (jde-directory-files-recurs file include-regexp)))
                (if (or (not include-regexp)
                        (string-match include-regexp file))
		      (setq files (append files (list file))))))))
    files))

(defun jde-expand-directory (dir include-regexp exclude-regexps symbol)
  "Get all the files in DIR whose names match INCLUDE-REGEXP except those whose
root names match EXCLUDE-REGEXPS. Return the files normalized against SYMBOL."
  (mapcar
   (lambda (included-file)
     (jde-normalize-path included-file symbol))
   (remove-if
    (lambda (file-path)
      (let ((file-name 
	      (file-name-nondirectory file-path)))
	(catch 'match
	    (loop for regexp in exclude-regexps do
		  (if (string-match regexp file-name)
		      (throw 'match t))))))
    (jde-directory-files-recurs dir include-regexp))))


(defun jde-expand-classpath (classpath &optional symbol)
  "If `jde-expand-classpath-p' is nonnil, replaces paths to 
directories that match `jde-lib-directory-names' with paths to jar or
zip files in those directories, excepting those specified by
`jde-lib-excluded-file-names'. This function assumes that the
existing paths are already normalized."
  (if jde-expand-classpath-p
      (let (paths)
	(loop for path in classpath do
	      (if (and 
		   (file-exists-p path)
		   (file-directory-p path)
		   (let ((dir-name (file-name-nondirectory path)))
		     (member-if 
		      (lambda (lib-name)
			(string-match lib-name dir-name))
		      jde-lib-directory-names)))
		  (progn
		    (setq paths 
			  (append 
			   paths
			   (jde-expand-directory
			    path 
			    "\\.jar$"
			    jde-lib-excluded-file-names
			    symbol)))
		    (setq paths
			  (append 
			   paths 
			   (jde-expand-directory
			    path 
			    "\\.zip$"
			    jde-lib-excluded-file-names
			    symbol))))
		(setq paths (append paths (list path)))))
	paths)
    classpath))


(defun jde-build-classpath (paths &optional symbol quote-path-p)
  "Builds a classpath from PATHS.  PATHS is a either list of paths or
a symbol whose value is a list of paths, in which case the optional
arg SYMBOL is unnecessary. If QUOTE-PATH-P is nonnil, quote paths
that contain spaces."
  (if (symbolp paths)
      (setq symbol paths
	    paths (symbol-value symbol)))
  (mapconcat
   (lambda (path)
     path)
    (jde-expand-classpath 
     (mapcar
      (lambda (path)
	(jde-normalize-path path symbol))
      paths)
     symbol)
   jde-classpath-separator))

(defun jde-global-classpath ()
  "Builds a classpath string from the path entries in
`jde-global-classpath'."
  (jde-build-classpath 'jde-global-classpath))


(defun jde-build-path-arg (arg path-list &optional quote symbol)
"Build a command-line path argument from a list of paths."
  (let ((path (jde-build-classpath path-list symbol)))
    (if quote
        (setq path (concat "\"" path "\"")))
    (setq path (concat arg " " path))))


(defun jde-build-classpath-arg (path-list &optional quote symbol)
"Build a classpath from a list of paths."
 (jde-build-path-arg "-classpath" path-list quote symbol))

(defun jde-root-dir-p (dir)
  "Return nonnil if DIR is a root directory." 
  (let ((parent (expand-file-name  ".." dir)))
    (cond 
     ((and
       (fboundp 'ange-ftp-ftp-name)
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
       (string= parent dir) ;; for paths like d:/ on emacs 22
       (string= (substring parent -3) "/.."))) ;; for paths like d:/ on emacs 21
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

(defun jde-get-global-classpath () 
  "Return the value of `jde-global-classpath', if defined, otherwise
the value of the CLASSPATH environment variable converted to a list,
of normalized paths, i.e., with . and ~ characters expanded and backslashes
replaces with slashes."
  (if jde-global-classpath 
      jde-global-classpath
    (let ((cp (getenv "CLASSPATH")))
      (if (stringp cp)
          (mapcar
           (lambda (path)
             (let ((directory-sep-char ?/))
                   (expand-file-name path)))
           (split-string cp jde-classpath-separator))))))



(defvar jde-entering-java-buffer-hook 
  '(jde-reload-project-file 
    jde-which-method-update-on-entering-buffer)
   "*Lists functions to run when entering a jde-mode source buffer from another
jde-mode buffer. Note that these functions do not run when reentering the same
jde-mode buffer from a non-jde-mode buffer. You should use this hook only for
functions that need to be run when you switch from one jde-mode buffer to
a different jde-mode buffer. Use `jde-mode-hook' if the function needs to run
only once, when the buffer is created.")

(defvar jde-current-buffer (current-buffer)
  "*Internal JDE variable that holds the current active buffer.")

(defun jde-detect-java-buffer-activation ()
  "Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the 
`jde-entering-java-buffer-hook' hooks."
  (let ((curr-buff (current-buffer)))
    (unless (equal curr-buff jde-current-buffer)
      (setq jde-current-buffer curr-buff)
      (if (eq major-mode 'jde-mode)
	  (condition-case err
	      (run-hooks 'jde-entering-java-buffer-hook)
	    (error 
	     (message "jde-entering-java-buffer-hook error: %s" 
		      (error-message-string err))))))))

(defun jde-monitor-post-command-hook ()
  "Checks whether `post-command-hook' includes all hooks required
by JDEE. If not, it adds the required hooks."
  (if (eq major-mode 'jde-mode)
      (dolist (hook (list 'jde-detect-java-buffer-activation))
	(when (not (member hook post-command-hook))
	  (add-hook 'post-command-hook hook)))))

(defvar jde-monitor-post-command-hook-timer nil
"Timer that runs `jde-monitor-post-command-hook' during
idle moments.")

(defun jde-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (count 
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (save-match-data
	    (string-match file-type file-name)))))))
	 

(defun jde-clean-up-after-jde ()
  "Removes `jde-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (if  (eq major-mode 'jde-mode)
      (unless  (> (jde-count-open-java-buffers) 1)
	(remove-hook 'post-command-hook 'jde-detect-java-buffer-activation)
	(when jde-monitor-post-command-hook-timer
	  (cancel-timer jde-monitor-post-command-hook-timer)
	  (setq jde-monitor-post-command-hook-timer nil))
	(remove-hook 'kill-buffer-hook 'jde-clean-up-after-jde))))


;; JDE help
(defun jde-find-jde-doc-directory ()
  "Return the path of the JDE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDE will store the JDE doc in a data
directory called jde. On all other Emacs versions, the JDE
expects to find the documentation in a subdirectory 
named doc of the directory that contains the file
jde.el."
  (jde-find-jde-data-directory))

;;;###autoload
(defun jde-show-help ()
  "Displays the JDE User's Guide in a browser."
  (interactive)
  (let* ((jde-dir (jde-find-jde-doc-directory))
         (jde-help
          (if jde-dir
	      (expand-file-name "doc/html/jde-ug/jde-ug.html" jde-dir))))      
    (if (and
         jde-help
         (file-exists-p jde-help))
        (browse-url (concat "file://" (jde-convert-cygwin-path jde-help))
                    (if (boundp 'browse-url-new-window-flag)
			'browse-url-new-window-flag
		      browse-url-new-window-p))
      (signal 'error '("Cannot find JDE help file.")))))


;;
;; Problem reporting functions contributed by Phillip Lord <plord@hgmp.mrc.ac.uk>.
;;
(defvar jde-problem-report-mail-address "pkinnucan@comcast.net" )

(defun jde-submit-problem-report()
  "Submit a problem report for the JDEE." 
  (interactive)
  (require 'reporter)
  (and 
   (y-or-n-p "Do you want to submit a problem report on the JDEE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      jde-problem-report-mail-address
      (format "JDE version %s\nRequired packages: cedet-%s\n"
	      jde-version cedet-version)
      (jde-problem-report-list-all-variables)
      nil
      'jde-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))


(defun jde-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a JDEBug buffer and inserts the contents of that, and then prompts 
for insertion of the .emacs file"
  (save-excursion 
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*JDEbug*"))
	   (messages-buffer 
	    (get-buffer
	     (if jde-xemacsp " *Message-Log*" "*Messages*")))
	   (backtrace-buffer (get-buffer "*Backtrace*"))
	   (jde-log-buffer (get-buffer "*jde-log*"))
	   (process 
	    (let ((proc (jde-dbs-get-target-process)))
	      (if (not proc)
		  (let ((dead-proc-alist 
			 (oref jde-dbs-the-process-morgue proc-alist)))
		    (if dead-proc-alist
			(setq proc (cdr (car dead-proc-alist))))))
	      proc))
	   (cli-buffer (if (and process (slot-boundp process 'cli-buf))
			   (oref process cli-buf)))
	   (locals-buffer (if (and process (slot-boundp process 'locals-buf))
			      (oref process locals-buf))))
     

      ;;insert the contents of the debug buffer if it is there. 
      (if debug-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *JDEBug* buffer were\n\n")
	    (insert-buffer debug-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *JDEbug* buffer" ))
	(insert-string "\n\n\nThere was no *JDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the CLI buffer are\n\n")
	    (insert-buffer cli-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert CLI buffer" ))
	(insert-string "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the locals buffer are\n\n")
	    (insert-buffer locals-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert locals buffer" ))
	(insert-string "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there. 
      (if backtrace-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer backtrace-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Backtrace* buffer" ))
	(insert-string "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there. 
      (if messages-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *Messages* buffer were\n\n")
	    (insert-buffer messages-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *Messages* buffer" ))
	(insert-string "\n\n\nThere was no *Messages* buffer" ))

      ;;insert the contents of the jde-log buffer if it is there. 
      (if jde-log-buffer
	  (progn
	    (insert-string "\n\n\nThe contents of the *jde-log* buffer were\n\n")
	    (insert-buffer jde-log-buffer)
	    (goto-char (point-max))
	    (insert-string "\n\n\nEnd Insert *jde-log* buffer" ))
	(insert-string "\n\n\nThere was no *jde-log* buffer" )))

    (when process-environment
      (insert-string "\n\n\nProcess environment: \n\n")
      (insert-string (mapconcat (lambda (var) var) process-environment "\n")))

    (let* ((init-file-name (if (featurep 'xemacs) "init.el" ".emacs"))
	   (buf (get-buffer-create 
		 (format "*Insert %s*" init-file-name)))
	   (mail-buf (current-buffer)))
      
      (set-buffer buf)

      (widget-insert 
       (format       "You should include the entire contents of your %s file.\n"
	       init-file-name))

      (widget-insert
       (format       "This is because often parts of the %s file that appear\n"
	       init-file-name))
	
      (widget-insert "not to be JDEE-related do in fact contain the cause of\n")
      (widget-insert "reported bugs.\n\n")

      (widget-insert 
       (format "If you choose not to send your %s file or the file loads many\n" 
	       init-file-name))

      (widget-insert 
       (format "other files, please attempt to replicate the bug, using the\n" 
	       init-file-name))

      (widget-insert 
       (format "minimal %s file suggested in the JDEE documentation, and note\n" 
	       init-file-name))

      (widget-insert "that you have done this in this bug report.\n")
      (switch-to-buffer buf)
      
      (set-buffer mail-buf)
      (goto-char (point-max))

      (if (y-or-n-p 
	   (format "Insert your %s file into the problem report? " 
		   init-file-name))
	  (progn
	    (insert-string 
	     (format "\n\n\nThe contents of the %s file was\n\n\n"
		     init-file-name))

	    (if (featurep 'xemacs) 
		(insert-file "~/.xemacs/init.el")
	      (insert-file "~/.emacs"))
	    (goto-char (point-max))
	    (insert-string 
	     (format "\n\n\n=====end inserted %s file"
		     init-file-name)))
	(insert-string 
	 (format "\n\n\nThe user choose not to insert their %s file\n" 
		 init-file-name)))

      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun jde-problem-report-list-all-variables()
  "List all variables starting with `jde' or `bsh'."
  (let (vars)
    (mapatoms
     (lambda (symbol)
       (if  (jde-symbol-p symbol)
	 (setq vars (cons symbol vars)))))
    vars))


;; Line numbering support.
(eval-when (compile)
  (require 'setnu))

(defvar jde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'jde-setnu-deletion-check)

(defun jde-setnu-after-change (start end length)
 "When in setnu-mode, toggles setnu-mode off and on."
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     jde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq jde-setnu-deletion-check nil)))

(defun jde-setnu-before-change (start end) 
  "Determines whether any newlines were deleted."
   (if setnu-mode
       (if (> end start) 
	   (setq jde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties start end))))))


(defcustom jde-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering."
 :group 'jde-project
 :type 'integer)

(defcustom jde-setnu-mode-enable nil
 "Enable numbering of lines in Java source buffers."
 :group 'jde-project
 :type 'boolean
 :set '(lambda (sym val)
	 (if val
	     (progn
	       (require 'setnu)
	       (add-hook 
		'after-change-functions 
		'jde-setnu-after-change)
	       (add-hook 
		'before-change-functions 
		'jde-setnu-before-change)
	       (mapc
		(lambda (buf)
		  (save-excursion
		    (set-buffer buf)
		    (if (and
			 (not setnu-mode)
			 (< (point-max) jde-setnu-mode-threshold))
			(setnu-mode 1))))
		  (jde-get-java-source-buffers)))
	   (progn
	     (mapc 
	      (lambda (buf)
		(save-excursion
		  (set-buffer buf)
		  (if (and (boundp 'setnu-mode)
			   setnu-mode)
		      (setnu-mode))))
	      (jde-get-java-source-buffers))))	 
	 (set-default sym val)))

;; jde-describe-map is Ehud Karni's describe map with jde prepended.
(defun jde-keymap-test (var)           ; internal function for keymap checking
       (and (boundp var)
            (keymapp (symbol-value var))))

(defun jde-describe-map (map)          ; display map binding
 "Display binding for MAP which must be a quoted keymap variable"
  (interactive
       (let ((map (intern (completing-read "Key map: " obarray 'jde-keymap-test 1))))
           (list map)))
       (let ((val (symbol-value map)))
           (or (keymapp val)
               (error "%s is not a keymap !" (symbol-name map)))
           (with-output-to-temp-buffer "*Help*"
               (princ (format "Binding for keymap %s is:\n" (symbol-name map)))
               (princ (substitute-command-keys "\\{val}" ))
               (print-help-return-message))))

(defun jde-keys ()
  "Displays JDE key bindings. Use `jde-bug-keys' to display JDEbug keybindings ."
  (interactive)
  (jde-describe-map 'jde-mode-map))


;; Contributed by John Ciolfi, jciolfi@mathworks.com.
(defun jde-compile-file-if-necessary (file)
  "Compile the JDE file FILE if necessary.
This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (if (string= (file-name-extension file) "el")
      (let* ((root (file-name-sans-extension file))
	     (elc-file (concat root ".elc")))
	(if (and
	     (or (not (file-exists-p elc-file))
		(file-newer-than-file-p file  elc-file))
	     (or (not (string= root "jde-xemacs"))
		 (featurep 'xemacs)))
	    (progn
	      (message (format "Byte-compiling %s..." 
			       (file-name-nondirectory file)))
	      (byte-compile-file file))))))

;;;###autoload
(defun jde-compile-jde ()
  "Byte-compile all uncompiled files of jde."

  ;; Be sure to have . in load-path since a number of files in jde
  ;; depend on other files and we always want the newer one even if
  ;; a previous version of jde exists.

  (interactive)
  (require 'jde-compat)
  (let ((load-path (append '(".") load-path))
	(jde-lisp-directory (expand-file-name "lisp" (jde-find-jde-data-directory))))
    (save-excursion 
      (mapcar 
       (function jde-compile-file-if-necessary)
       (directory-files jde-lisp-directory t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;; 
;; Find command                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jde-find-case-sensitive nil
  "*Specifies whether the jde-find command performs a case-sensitive search.
If non-nil, the search is case-sensitive; otherwise, the search ignores case."
  :group 'jde-project
  :type 'boolean
)

;; (makunbound 'jde-find-granularity)
(defcustom jde-find-granularity '("Character")
  "*Specifies the granularity of the expression search
conducted by `jde-find': Character (expression starting
on any character), Word (match words only), Line
(match lines only)."
  :group 'jde-project
  :type  '(list
	   (radio-button-choice
	    :format "%t \n%v"
	    :tag "Search granularity:"
	    (item "Character")
	    (item "Word")
	    (item "Line"))))


(defcustom jde-find-file-regexp '("*.java")
  "*Specifies the regular expression that the jde-find command uses
to select files to be searched. You can use any regular expression  
supported by the -name option of the GNU find command."
  :group 'jde-project
  :type '(repeat (string :tag "Find regexp"))
)

(defclass jde-find-dialog (efc-dialog)
  ((expr-field     :initarg :expr-field
		   :documentation "Edit field for expression to find.")
   (expression     :initarg :expression
		   :type string
		   :initform ""
		   :documentation "Regular expression to find.")
   (dir-fields     :initarg :dir-fields
		   :documentation "List of directory path fields.")
   (dirs           :initarg :dirs
		   :type list
		   :documentation "Directories to search recursively.")
   (file-fields    :initarg :file-fields
		   :documentation "Regular expression fields for files to search.")
   (files          :initarg :files
		   :type list
		   :initform ("*.java")
		   :documentation "Files to search.")
   (ignore-case-cb :initarg :ignore-case
		   :documentation "Ignore case check box.")
   (ignore-case-p  :initarg :ignore-case-p
		   :type boolean
		   :initform t
		   :documentation "If true, ignore case when searching.")
   (grain-rbs      :initarg :grain-rbs
		   :documentation "Granularity radio buttons.")
   (grain          :initarg :grain
		   :type string
		   :initform "Character"
		   :documentation "Search granularity: Character, Word, Line")
   (ok             :initarg :ok
		   :type boolean
		   :initform nil
		   :documentation "True if user clicked the OK button.")
   (the-dialog     :type (or null jde-find-dialog)
		   :allocation :class
		   :initform nil
		   :documentation
		   "The only instance of the find expression dialog buffer."))
  "Dialog displayed by `jde-find' command.")

(defmethod initialize-instance ((this jde-find-dialog) &rest fields)
  "Find options dialog constructor."
  (oset this title "Find Dialog")
  (call-next-method))

(defmethod efc-dialog-create ((this jde-find-dialog))
 
  (widget-insert "Find Expression Options\n\n")

  (oset this expr-field
 	(widget-create
	 (list
	  'text
	  :tab-order 1
	  :format "%t %v"
	  :tag "Expression:"
	  :value (oref this expression))))

  (widget-insert "\n")

  (oset this dir-fields
	(widget-create
	 (list
	  'repeat 
	  :tag "Directories to search recursively"
	  :value (if (slot-boundp this 'dirs)
		     (oref this dirs)
		   (mapcar
		    (lambda (p)
		      (jde-normalize-path p 'jde-sourcepath))
		    jde-sourcepath))
	  (list 'file :tag "Path"))))
  (widget-insert "\n")

  (oset this file-fields
	(widget-create
	 (list
	  'repeat 
	  :tag "File types to search"
	  :value (oref this files)
	  (list 'file :tag "File regexp"))))

  (widget-insert "\n")

  (oset this ignore-case-cb
	(widget-create
	 (list 'checkbox 
	       :format "%[%v%] %t" 
	       :tag "Ignore case"
	       :value (oref this ignore-case-p)
	       )))

  (widget-insert "\n\n")

  (oset this grain-rbs
 	(widget-create
 	 (list
	  'radio-button-choice
	  :format "%t\n%v"
	  :tag "Search granularity:"
	  :value (oref this grain)
	  :args (list
		 (list 'item "Character")
		 (list 'item "Word")
		 (list 'item "Line")))))
		       
  (widget-insert "\n"))


(defmethod efc-dialog-show ((this jde-find-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (call-next-method)
  (recursive-edit))


(defmethod efc-dialog-ok ((this jde-find-dialog))
  "Invoked when the user selects the OK button on the options
dialog. Sets the :dirs field of THIS to the search paths chosen by the
user, kills the dialog buffer, and exits recursive-edit mode."
  
  (oset this 
	expression
	(widget-value (oref this expr-field)))

  (oset this 
	dirs
	(widget-value (oref this dir-fields)))

  (oset this
	files
	(widget-value (oref this file-fields)))

  (oset this 
	ignore-case-p
	(widget-value (oref this ignore-case-cb)))

  (oset this
	grain
	(widget-value (oref this grain-rbs)))

  (oset this ok t)

  (delete-window)
  (kill-buffer (oref this buf))
  (pop-to-buffer (oref this initbuf))
  (set-buffer (oref this initbuf))

  (exit-recursive-edit))

(defmethod efc-dialog-cancel ((this jde-find-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this ok nil)
  (exit-recursive-edit))



(defvar jde-find-root-history nil
  "History of directory trees searched in this session.")

(defvar jde-find-regexp-history nil
  "History of search expressions used in this session.")

(defun jde-find-grep-internal (regexp files &optional dirs no-case grain)
  "Find a regular expression REGEXP in files of type FILES in
 DIRS, where DIRS is a string of space-separated paths of
directories to search recursively. If NO-CASE is nonnil, ignore
case. GRAIN is a string that indicates the granularity of the search,
i.e., match any \"Character\" string, a \"Word\" only, or a \"Line\"
only."
  (if (not (executable-find 
	    ;; Hack required by faulty XEmacs implementation of executable-find.
	    (if (eq system-type 'windows-nt) "grep.exe" "grep")))
      (error "This command requires the Unix grep utility."))
  (if (not (executable-find 
	    (if (eq system-type 'windows-nt) "find.exe" "find")))
      (error (list "This command requires the Unix find utility.")))
  (let* ((directories-option
	  (if dirs dirs "."))
	 (case-sensitive-option 
	  (if no-case  "-i" ""))
	 (granularity-option
	  (cond
	   ((and grain (string= grain "Word"))
	    "-w")
	   ((and grain (string= grain "Line"))
	    "-x")
	   (t
	    " ")))	    
	  (file-regexp-option 
	   (mapconcat 
	    (lambda (x) 
	      (format "-name \"%s\"" x)) 
	    files
	    " -or "))
	  (cmd 
	   (format "find %s %s -type f | xargs grep %s %s -n \"%s\" /dev/null" 
		  directories-option
		  file-regexp-option
		  case-sensitive-option
		  granularity-option
		  regexp)))
    (grep cmd)))


(defun jde-find (&optional regexp)
  "Find a regular expression REGEXP in all of the files in the
current JDE project. Tests each of the following path variables
`jde-sourcepath', `jde-compile-option-sourcepath',
`jde-compile-option-classpath', or `jde-global-classpath' and uses the
directories specified by the first path variable that has a nonnil
value. The `jde-find-case-sensitive' variable controls case
sensitivity, `jde-find-granularity' determines the granularity of the
search (character, word, line), and `jde-find-file-regexp' determines
the type of files to be searched. Use `jde-find-dlg' if you want to
set case sensitivity, granularity, or file types interactively. This
command requires that the Unix grep and find utilities be installed on
your system in the Emacs command path. The Cygwin package contains
Windows versions of both utilities."
  (interactive)
  (let ((regexp 
	 (if (and (boundp 'regexp) regexp) 
	     regexp
	   (read-from-minibuffer 
	    "Search for regexp: "
	    (if (boundp 'jde-find-regexp-history) 
		(car jde-find-regexp-history)
	      nil)
	    nil nil 'jde-find-regexp-history)))
	(search-path 
	 (read-from-minibuffer 
	   "Search directories: "
	   (cons 
	    (mapconcat 
	     (lambda (x) x)
	     (cond
	      (jde-sourcepath
	       (mapcar 
		(lambda (path)
		  (jde-normalize-path path 'jde-sourcepath))
		jde-sourcepath))
	      (jde-compile-option-sourcepath 
	       (mapcar 
		(lambda (path)
		  (jde-normalize-path path 'jde-compile-option-sourcepath))
		jde-compile-option-sourcepath))
	      (jde-compile-option-classpath 
	       (mapcar 
		(lambda (path)
		  (jde-normalize-path path 'jde-compile-option-classpath))
		jde-compile-option-classpath))
	      (jde-global-classpath 
	       (mapcar 
		(lambda (path)
		  (jde-normalize-path path 'jde-global-classpath))
		jde-global-classpath))
	      (t
	       (list default-directory)))	
	     " ")
	    0)
	   nil nil 'jde-find-root-history)))
    (jde-find-grep-internal
     regexp
     jde-find-file-regexp
     search-path
     (not jde-find-case-sensitive)
     (car jde-find-granularity))))

(defun jde-find-dlg ()
  "Displays a dialog buffer that allows you to set all search options
interactively. Pressing the dialog's OK button initiates the
search. Use `jde-find' if you need to set only the expression to be
found and the directories to be searched and prefer using the
minibuffer."
  (interactive)
  (let ((dialog 
	 (progn
	   (if (not (oref 'jde-find-dialog the-dialog))
	       (oset-default 'jde-find-dialog the-dialog (jde-find-dialog "find dialog")))
	   (oref 'jde-find-dialog the-dialog))))
    (efc-dialog-show dialog)
    (when (oref dialog ok)
      (jde-find-grep-internal
       (oref dialog expression)
       (oref dialog files)
       (mapconcat
	'jde-normalize-path
	(oref dialog dirs)
	" ")
       (oref dialog ignore-case-p)
       (oref dialog grain)))))

(defun jde-create-prj-values-str ()
  "Create Java expression that updates the JDEE's class list
to include all the classes on `jde-global-classpath', if
defined, otherwise the classpath specified by the CLASSPATH
environment variable."
  (let* ((directory-sep-char ?/)  ;; Override NT/XEmacs setting
	 (classpath 
	      (jde-build-path-arg nil (jde-get-global-classpath) t)))
    (format "jde.util.JdeUtilities.setProjectValues(\"%s\", %s);"
	    jde-current-project 
	    classpath)))

(defclass jde-bsh-buffer (bsh-comint-buffer) ()
  "JDEE's beanshell buffer")

(defmethod initialize-instance ((this jde-bsh-buffer) &rest fields)
  (oset this buffer-name "*JDEE bsh*")
  (call-next-method))

(defclass jde-bsh (bsh)
  ((bsh-cmd-dir      :initarg :bsh-cmd-dir
                     :type string
                     :documentation
                     "Path of the BeanShell commmand directory.")

   (checkstyle-jar  :initarg :checkstyle-jar
                    :type string
                    :documentation
                    "Path of the Checkstyle jar.")

   (regexp-jar      :initarg :regexp-jar
                    :type string
                    :documentation
                    "Path of the Jakarta regexp jar.")

   (jde-jar         :initarg :jde-jar
                    :type string
                    :documentation
                    "Path of the JDEE jar.")

   (jde-classes-dir :initarg :jde-classes-dir
                    :type string
                    :documentation
                    "Path of the JDEE classes directory.")


   (the-bsh        :type jde-bsh
                   :allocation :class
                   :documentation
                   "The single instance of the JDEE's BeanShell."))
  "Class of JDEE BeanShells. There is only one per Emacs session.")

(defmethod initialize-instance ((this jde-bsh) &rest fields)
  "Constructor for the JDEE BeanShell instance."
  (call-next-method)
  (let* ((jde-java-directory
          (concat
           (jde-find-jde-data-directory)
           "java/")))

    (oset this bsh-cmd-dir (expand-file-name "bsh-commands" jde-java-directory))
    (oset this checkstyle-jar  (expand-file-name "lib/checkstyle-all.jar" jde-java-directory))
    (oset this regexp-jar (expand-file-name "lib/jakarta-regexp.jar" jde-java-directory))
    (oset this jde-classes-dir (expand-file-name "classes" jde-java-directory))
    (oset this jde-jar (expand-file-name "lib/jde.jar" jde-java-directory))
    (oset this jar  (expand-file-name "lib/bsh.jar" jde-java-directory))
    (oset-default 'jde-bsh the-bsh this)))

(defmethod bsh-create-buffer ((this jde-bsh))
  "Creates the JDEE's beanshell buffer."
  (oset this buffer (jde-bsh-buffer "JDEE bsh buffer")))

(defmethod bsh-build-classpath-argument ((this jde-bsh))
  (jde-build-classpath (oref this cp) 'jde-global-classpath t))

(defmethod bsh-launch :BEFORE ((this jde-bsh) &optional display-buffer)
  "Sets the vm and classpath to the vm and classpath for the current project before
the PRIMARY launch method is invoked."
  (let* ((project-ant-home
	  ;; Code referring to jde-ant variables uses symbols to
	  ;; avoid causing compilation errors since jde-ant is not required.
          (jde-get-project 'jde-ant-home jde-current-project))
         (ant-home (if (and (boundp 'jde-ant-home)
                            (not (string= (symbol-value 'jde-ant-home) "")))
                       (symbol-value 'jde-ant-home)     ;jde-ant loaded
                     (if (and project-ant-home
                              (not (string= project-ant-home "")))
                         project-ant-home ; jde-ant not loaded but
                                        ; jde-ant-home set in project
                                        ; file
                       (getenv "ANT_HOME")))) ; jde-ant-home not set in
                                        ; project file and not
                                        ; customized
         )

    (oset this vm (oref (jde-run-get-vm) :path))
    (oset  this  cp (delq
                     nil
                     (append
                      (list
                       (oref this jar)
                       (oref this bsh-cmd-dir)
                       (oref this checkstyle-jar)
                       (oref this regexp-jar)
                       (if jde-devel-debug
                           (oref this jde-classes-dir))
		       (oref this jde-jar)       
                       (jde-get-tools-jar)
                       (if ant-home (expand-file-name "lib" ant-home)))
		      (jde-pi-get-bsh-classpath)
                      (jde-expand-classpath (jde-get-global-classpath)))))))

;; Create the BeanShell wrapper object.
(jde-bsh "JDEE BeanShell")

(defun jde-bsh-running-p ()
  "Returns t if the JDEE's BeanShell instance is running."
  (bsh-running-p (oref 'jde-bsh the-bsh)))


(defun jde-jeval (java-statement &optional eval-return)
  "Uses the JDEE's instance of the BeanShell
Java interpreter to evaluate the Java expression EXPR.  If the
BeanShell is not running, the JDEE starts an instance. This function
returns any text output by the Java interpreter's standard out or
standard error pipes.  If EVAL-RETURN is non-nil, this function
returns the result of evaluating the Java output as a Lisp
expression."
  (let ((the-bsh (oref 'jde-bsh the-bsh)))
   
    (when (not (bsh-running-p the-bsh))
      (bsh-launch the-bsh)
      (bsh-eval the-bsh (jde-create-prj-values-str)))

    (bsh-eval the-bsh java-statement eval-return)))

(defun jde-jeval-r (java-statement)
  "Uses the JDEE's instance of the BeanShell to 
evaluate JAVA-STATEMENT and then uses the Emacs Lisp
interpreter to evaluate the result. This function
is intended to be used to implement Emacs extensions
coded in Java and executed by the BeanShell. The function 
assumes that the Java extension interacts with Emacs
by printing Lisp forms to the BeanShell's standard output \
port."
  (jde-jeval java-statement t))


(defun jde-jeval-cm (java-expr &optional buffer-head finish-fcn)
  "Evaluate JAVA-EXPR and display the result in a compilation-mode buffer.
The optional argument BUFFER-HEAD specifies text to appear at the head of
the compilation buffer. The optional argument FINISH-FCN specifies a
function to be called when the compilation is finished. This function
is intended to be used to invoke Java development utilities, such as 
source code style checkers, that emit compiler-like error messages.
Displaying the output in a compilation-mode buffer enables the user to
use compilation-mode's error message navigation and hyperlinking 
capabilities.

The following example uses this function to invoke the javac compiler on
a file in the current directory:

 (jde-bsh-compile-mode-eval \"jde.util.CompileServer.compile(\\\"Test.java\\\");\" 
   \"Compile Test.java\" 'jde-compile-finish-kill-buffer)"
  (let* ((buffer-obj (bsh-compilation-buffer "buffer"))
	 (native-buf (oref buffer-obj buffer))
	 (bufwin (display-buffer native-buf)))

    (compilation-set-window-height bufwin)

    (save-some-buffers (not compilation-ask-about-save) nil)

    (if finish-fcn 
	(lexical-let ((finish finish-fcn))
	  (setq compilation-finish-function 
		(lambda (buf msg) 
		  (funcall finish buf msg)
		  (setq compilation-finish-function nil)))))


    (if (not (featurep 'xemacs))
	(if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))  


    (if (not (featurep 'xemacs))
	(if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))     


    (save-excursion
      (set-buffer native-buf)

      (if buffer-head
	  (insert buffer-head)
	(insert java-expr))

      (insert "\n")
 

      (if (not (jde-bsh-running-p))
	  (progn
	    (bsh-launch (oref 'jde-bsh the-bsh))
	    (bsh-eval (oref 'jde-bsh the-bsh) (jde-create-prj-values-str))))


      (bsh-buffer-eval 
       (oref 'jde-bsh the-bsh)
       java-expr
       buffer-obj)
    
    (set-buffer-modified-p nil)	 
    (setq compilation-last-buffer native-buf))))


;;;###autoload
(defun jde-bsh-run()
  "*Starts the JDEE version of the BeanShell."
  (interactive)
  (bsh-launch (oref 'jde-bsh the-bsh) t))

(defun jde-bsh-exit ()
  "Closes the existing beanshell process"
  (interactive)
  (if (jde-bsh-running-p)
      (let ((process (bsh-get-process (oref 'jde-bsh the-bsh))))
        (if (and
             (boundp 'jde-ant-invocation-method) ;; ant package may not be loaded.
             (string= (car (symbol-value 'jde-ant-invocation-method)) "Ant Server"))
            (process-send-string process "jde.util.JdeUtilities.exit();\n")
          (process-send-string process "exit();\n")))
    (message "The beanshell is not running")))


(defun jde-show-speedbar ()
  "Show the speedbar after first checking whether the correct
version of speedar is installed."
  (interactive)
  (require 'speedbar)
  (speedbar-frame-mode))
    
(defun jde-autoload-update ()
  "Updates autoload definitions in jde-autoload.el."
  (interactive)
  (let* ((default-directory (expand-file-name "lisp" (jde-find-jde-data-directory)))
	 (generated-autoload-file (expand-file-name "jde-autoload.el" default-directory)))
    (mapc 'update-file-autoloads (directory-files "." nil "\\.el$"))
    (save-excursion
      (set-buffer  "jde-autoload.el")
      (save-buffer))
    (kill-buffer "jde-autoload.el")))


(defun jde-browse-class-at-point ()
  "Displays the class of the object at point in the BeanShell Class
Browser. Point can be in a variable name, class name, method name, or field name).
This command has the  same requirements to work as the field/method-completion
feature in JDE (see `jde-complete-at-point')."
  (interactive)
  (if (jde-open-functions-exist)
      (let* ((thing-of-interest (thing-at-point 'symbol))
             (pair (save-excursion (end-of-thing 'symbol)
                                   (jde-parse-java-variable-at-point)))
             (class-to-open (jde-open-get-class-to-open
                             pair thing-of-interest)))
        (if (and class-to-open (stringp class-to-open))
            (progn
              (bsh-eval
               (oref 'jde-bsh the-bsh)
               (concat "exploreClass(\"" class-to-open "\");")))
          (error "Can not parse the thing at point!")))
    (message "You need JDE >= 2.2.6 and Senator for using this feature!")))


(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  (defun jde-self-test () 
    "Runs jde self tests."
    (interactive)
    (require 'jde-junit)
    (jde-junit-self-test)
    (jde-dbs-self-test)))


;; This must come after all JDEE customization variables have been
;; defined.
(jde-custom-adjust-groups)

(provide 'jde)

;; Change History 

;;
;; $Log: jde.el,v $
;; Revision 1.357.2.4  2006/03/09 04:19:37  paulk
;; Fix bug in jde-earlier-versionp.
;;
;; Revision 1.357.2.3  2006/03/01 04:11:03  paulk
;; Add support for Java annotations.
;;
;; Revision 1.357.2.2  2006/02/22 04:58:44  paulk
;; "Emacs 22 compatibility fix: fix max-lisp-eval-depth error caused by change in behavior of expand-file-name that breaks jde-root-dir-p. Thanks to Phillip Lord [Phillip.Lord@newcastle.ac.uk].
;;
;; Revision 1.357.2.1  2006/02/21 04:03:20  paulk
;; Update jde-earlier-versionp to handle cedet pre releases.
;;
;; Revision 1.357  2004/12/13 05:55:46  paulk
;; Updated submit problem report command to include an XEmac user's init.el file.
;;
;; Revision 1.356  2004/12/06 05:40:14  paulk
;; Moved jde-debugger-running-p package to jde-db.el.
;;
;; Revision 1.355  2004/11/12 12:19:03  paulk
;; Remove dead code. Thanks to Jason Rumney.
;;
;; Revision 1.354  2004/11/12 04:32:30  paulk
;; Update to reflect change of files menu to file menu. Thanks to Jason Rumney.
;;
;; Revision 1.353  2004/10/18 03:14:42  paulk
;; Bump version number. Add self-test infrastructure.
;;
;; Revision 1.352  2004/10/15 05:41:24  paulk
;; Bumped version number to 2.3.4beta7. Added Unit Test to JDE New menu.
;;
;; Revision 1.351  2004/10/11 03:35:43  paulk
;; Back out junit jar.
;;
;; Revision 1.350  2004/09/21 05:17:41  paulk
;; Created an Import submenu under the Code Generation menu. The submenu contains all import-related commands.
;;
;; Revision 1.349  2004/09/20 05:31:40  paulk
;; Changes needed to make restructured debugging code work for applets.
;;
;; Revision 1.348  2004/08/25 12:30:04  paulk
;; Add menu item to display Beanshell help.
;;
;; Revision 1.347  2004/07/06 01:49:28  paulk
;; Optionally enable jde-electric-return-mode when buffer is loaded.
;;
;; Revision 1.346  2004/06/23 04:40:39  paulk
;; Update to require cedet 1.0beta2.
;;
;; Revision 1.345  2004/06/07 15:28:43  paulk
;; Make jde-bsh-run autoloadable.
;;
;; Revision 1.344  2004/06/07 05:47:40  paulk
;; - Made jde-entering-java-buffer-hook non-customizable. This variable is part
;;   of the project-switching machinery and therefore should not itself be
;;   customizable.
;;
;; - Made various changes intended to make the JDEE's project-switching
;;   code more robust.
;;
;; Revision 1.343  2004/06/05 04:17:43  paulk
;; Change jde-submit-problem-report to report the cedet version.
;;
;; Revision 1.342  2004/06/03 12:56:45  paulk
;; Restore make-local-hook for post-command-hook. Needed for XEmacs compatibility. Thanks to David Ponce.
;;
;; Revision 1.341  2004/06/03 03:05:39  paulk
;; Change radio button items on JDE menu to toggle button items. Thanks to Jens Lautenbacher.
;;
;; Revision 1.340  2004/06/03 02:40:11  paulk
;; Add Remove item to Documentation submenu.
;;
;; Revision 1.339  2004/06/03 02:27:09  paulk
;; Check for cedet not installed. Thanks to David Ponce.
;;
;; Revision 1.338  2004/05/28 12:16:08  paulk
;; Change required package version checking to check for cedet instead of semantic/eieio.
;;
;; Revision 1.337  2004/05/16 04:26:08  paulk
;; Fix require order error.
;;
;; Revision 1.336  2004/05/16 04:22:49  paulk
;; Move custom-set-default definition before library requires.
;;
;; Revision 1.335  2004/05/16 03:23:52  paulk
;; Change call to jdw-wiz-tostring to jde-wiz-tostring.
;;
;; Revision 1.334  2004/05/14 03:24:01  paulk
;; - Compatibility fix to make cflow abbreviations work in XEmacs.
;;   Thanks to Martin Schwamberger.
;; - Provide a new JDE->Code Generation->Modes menu with items for
;;   enabling abbrev mode and electric return mode.
;;
;; Revision 1.333  2004/05/04 04:42:20  paulk
;; jde-init-abbrev-table gives t as expansion. This avoids downcasing of abbreviations used in comments. Thanks
;; to Martin Schwamberger.
;;
;; Revision 1.332  2004/05/03 02:39:02  paulk
;; Cosmetic change.
;;
;; Revision 1.331  2004/05/03 02:37:11  paulk
;; Adds toString method wizard to Wizards menu.
;;
;; Revision 1.330  2004/04/30 05:44:18  paulk
;; Add Electric Return item to the code generation menu.
;;
;; Revision 1.329  2004/04/29 02:39:52  paulk
;; Fix regressions caused by enhancement to jde-expand-directory.
;;
;; Revision 1.328  2004/03/21 03:24:57  paulk
;; Added jde-import-all to the JDEE menu and to the key binding table.
;;
;; Revision 1.327  2004/03/04 05:22:19  paulk
;; Update the jde-key-bindings variable to bind the Return key in Java buffers
;; to jde-gen-embrace, a function that closes the opening brace at point.
;;
;; Revision 1.326  2004/03/03 03:54:34  paulk
;; Moved project-related stuff to jde-project-file.el.
;;
;; Revision 1.325  2004/02/24 05:59:25  paulk
;; Adds support for customization variable enhancements.
;;
;; Revision 1.324  2004/02/09 06:48:41  paulk
;; Moved project-file code to a new package: jde-project-file.el.
;;
;; Revision 1.323  2004/02/02 07:25:53  paulk
;; Added a Bean item to the JDE NEW menu. The item creates a skeleton Java bean in a new buffer.
;;
;; Revision 1.322  2003/12/28 14:47:06  paulk
;; Semantic 2.0 compatibility fix.
;;
;; Revision 1.321  2003/11/29 05:28:26  paulk
;; Change abbreviation for throws from throw to thro to avoid inadvertent completion of throw to throws.
;;
;; Revision 1.320  2003/11/25 06:48:02  paulk
;; Fix regression that was causing abbrev-mode to be enabled for JDEE buffers even when jde-enable-abbrev-mode was set to nil. Also, enable abbrev-mode in all open JDEE buffers for the current project as a side effect of setting jde-enable-abbrev-mode on.
;;
;; Revision 1.319  2003/10/23 03:53:22  paulk
;; Added fix for compatibility with old versions of XEmacs. Thanks to Len Trigg.
;;
;; Revision 1.318  2003/10/14 05:07:40  paulk
;; jde-count-open-java-buffers now saves match data before doing a string-match. This fixes compatibility problem with emacs-w3m.
;;
;; Revision 1.317  2003/09/28 05:09:38  paulk
;; Created a new buffer-local variable, jde-buffer-project-file, that stores
;; the path of the project file for the current Java buffer. Updated
;; jde-get-current-project-buffers to use this variable to test whether
;; a given buffer is part of the current project. This should improve
;; the performance of this function and hence loading of project files,
;; which depends on it.
;;
;; Revision 1.316  2003/09/24 05:13:15  paulk
;; Changed jde-normalize-path to support use of relative paths in .emacs
;; files.  Previously, if a relative path appeared in a variable, e.g.,
;; jde-global-path, that was set for the current project by the user's
;; .emacs file (as opposed to a project file), jde-normalize-path would
;; resolve the variable relative to the source buffer directory. Now
;; jde-normalize-path resolves it relative to the location of the nearest
;; project file in the source buffer directory tree. If the directory
;; tree does not contain a project file, jde-normalize-path resolves the
;; path relative to the source buffer directory.
;;
;; Revision 1.315  2003/08/25 05:33:13  paulk
;; Added item to the Documentation submenu to display the Javadoc Tool Reference.
;;
;; Revision 1.314  2003/07/11 14:35:54  paulk
;; Makes jde-cygpath a noop if system-type is not cygwin.
;;
;; Revision 1.313  2003/07/08 05:54:16  paulk
;; Add documentation for jde-jeval- functions.
;;
;; Revision 1.312  2003/07/01 05:10:58  paulk
;; Adds a jde-bsh-compile-mode-eval function. This function
;; is intended to be used by plugins to invoke Java development
;; utilities, such as source code style checkers, that emit
;; compiler-like error messages.  Displaying the output in a
;; compilation-mode buffer enables the user to use compilation-mode's
;; error message navigation and hyperlinking capabilities.
;;
;; Revision 1.311  2003/06/26 05:36:13  paulk
;; Fix regression introduced by changes to jde-root-dir-p in previous version.
;;
;; Revision 1.310  2003/06/26 04:45:36  paulk
;; - Now requires jde-autoloads for XEmacs installations that use
;;   the independently distributed version of the JDEE rather than
;;   the version that is packaged with XEmacs.
;;
;; - Provides a workaround for a bug in file-exists-p in the cygwin
;;   version of XEmacs that prevented the JDEE from finding project
;;   files. The bug is that file-exists-p denies the existence of
;;   files whose native Windows paths (file-truename) end in a slash.
;;
;; Revision 1.309  2003/06/24 04:08:44  paulk
;; - Update jde-symbol-p to recognize variables defined by autoloaded packages
;;   that have not yet been loaded. Thanks to Martin Schwamberger.
;;
;; - Fix jde-compile-jde to compile jde-xemacs.el only on Xemacs.
;;
;; Revision 1.308  2003/06/18 10:49:20  paulk
;; Enhance jde-autoload-update to save and kill jde-autload.el buffer.
;;
;; Revision 1.307  2003/06/17 06:11:45  paulk
;; - Change JDE->Documentation->Generate to Generate All.
;; - Add JDE->Documentation->Generate Buffer.
;;
;; Revision 1.306  2003/05/03 09:01:55  paulk
;; Require jde-class package.
;;
;; Revision 1.305  2003/04/28 04:49:57  paulk
;; jde-mode now installs plugin menu if any plugins are installed on the user's system and the plugins have menus. beanshell startup now includes plugin classpaths in the beanshell classpath.
;;
;; Revision 1.304  2003/04/17 16:56:39  jslopez
;; Points C-c C-v C-k from jde-bsh to jde-bsh-run.
;;
;; Revision 1.303  2003/04/16 04:15:26  paulk
;; Restore require for jde-plugins package. This package is now in the CVS repository.
;;
;; Revision 1.302  2003/04/11 22:53:08  jslopez
;; Removes require clause for missing library, jde-plugins. It was
;; breaking the byte compilation.
;;
;; Revision 1.301  2003/04/09 04:03:23  paulk
;; Moved jde-find-jde-data-directory to jde-util package.
;;
;; Revision 1.300  2003/04/06 07:34:43  paulk
;; Renamed jde-bsh to jde-bsh-run to avoid conflict with class name.
;;
;; Revision 1.299  2003/03/28 05:33:30  andyp
;; XEmacs optimizations for JDEbug and efc.
;;
;; Revision 1.298  2003/03/04 10:15:07  paulk
;; Updated to use the new bsh-get-process method provided
;; by bsh to get its process. This is necessary to insulate
;; clients from the fact that bsh now uses a bsh-buffer
;; object to wrap the Emacs Lisp buffer object that specifies
;; the associated process.
;;
;; Revision 1.297  2003/02/25 16:00:30  jslopez
;; Fixes typo.
;;
;; Revision 1.296  2003/02/25 06:58:07  paulk
;; - Moved jde-debug command to jde-db.el and reimplemented it so that
;;   it works for both jdb and JDEbug.
;;
;; - Changing jde-mode-abbreviations now updates all the Java source
;;   buffers in the current project.
;;
;; Revision 1.295  2003/02/18 02:09:39  jslopez
;; Fixes regression bugs.
;;
;; Revision 1.294  2003/02/17 08:13:05  paulk
;; Changes required to support new package-independent version of beanshell.el
;;
;; Revision 1.293  2003/02/10 13:59:35  jslopez
;; Fixes regression bug. Readds jde-debug method.
;;
;; Revision 1.292  2003/01/27 06:36:55  paulk
;; jde-get-jdk-dir now uses javac instead of java as the JDK program to look
;; for when searching exec-path for the JDK. This eliminates false hits on
;; instances of java that were not installed as part of a JDK.
;;
;; Revision 1.291  2003/01/26 16:40:37  paulk
;; Fix jde-get-global-classpath to normalize the value of CLASSPATH using
;; the standard interpretation of the relative path character (.) rather
;; than the project-relative interpretation.
;;
;; Revision 1.290  2003/01/26 08:09:46  paulk
;; Fix to jde-create-prj-values-str to handle the case where the CLASSPATH
;; has backslashes. Thanks to Martin Schwamberger.
;;
;; Revision 1.289  2003/01/21 14:21:57  jslopez
;; Adds jde-gen-change-listener to the listeners submenu.
;;
;; Revision 1.288  2003/01/21 05:37:01  paulk
;; - Adds jde-browse-class-at-point command. This command displays
;;   the class at point in the BeanShell class browser.
;;
;; - Replaces Speedbar menu item with a Browse submenu
;;   containing the items Browse->Source and
;;   Browse->Class at Point. The first displays the
;;   speedbar. The second invokes the
;;   jde-browse-class-at-point command.
;;
;; Revision 1.287  2003/01/02 07:37:19  paulk
;; Fixed typo in binding for jde-jdb-menu-debug-applet.
;; Thanks to Matt Watson.
;;
;; Revision 1.286  2002/12/30 05:25:24  paulk
;; Minor tweaks for JDEE 2.3.2
;;
;; Revision 1.285  2002/12/26 06:43:49  paulk
;; - Removed the obsolete function jde-set-compiler.
;; - Fixed jde-set-global-classpath to set jde-global-classpath correctly.
;;
;; Revision 1.284  2002/12/21 08:58:36  paulk
;; Fix docstring for jde-global-classpath.
;;
;; Revision 1.283  2002/12/19 06:35:59  paulk
;; Changed to permit autoloading of jde-package.el file.
;;
;; Revision 1.282  2002/12/06 03:47:33  ahyatt
;; Changes to support Mac OS X, which does not use tools.jar
;;
;; Revision 1.281  2002/12/03 04:49:21  paulk
;; Fixes infinite recursion bug when a project file specifies
;; jde-ant-build as the jde-build-function.
;;
;; Revision 1.280  2002/11/30 04:48:10  paulk
;; Bumped revision number to 2.3.0.
;;
;; Revision 1.279  2002/11/27 06:06:09  paulk
;; Replaced the alias jde-javadoc-generate-javadoc-template with
;; its real name: jde-javadoc-autodoc-at-line.
;;
;; Revision 1.278  2002/11/26 06:11:37  paulk
;; Fixed regression bug introduced in JDEE 2.2.9.2 that caused symbols to
;; be listed more than once in the jde-symbol-list variable under some
;; circumstances. This in turn caused a symbol to be set multiple times
;; in a prj.el file.
;;
;; Revision 1.277  2002/11/25 04:56:34  paulk
;; Updated jde-create-prj-values-str to use the CLASSPATH environment variable if
;; jde-global-classpath is nil.
;;
;; Revision 1.276  2002/11/23 07:58:39  ahyatt
;; Display helpful error message when no correct mapping is found in the
;; jde-jdk-registry
;;
;; Revision 1.275  2002/11/22 07:44:44  paulk
;; Correct the docstring for jde-get-jdk-dir.
;;
;; Revision 1.274  2002/11/21 04:05:38  paulk
;; The following changes provide the infrastructure to ensure registration
;; and initialization of customization variables defined by autoloaded
;; JDEE packages:
;;   - Provide an argument to jde-symbol-list to force regeneration of the list.
;;   - Define jde-update-autoloaded-symbols function.
;;
;; Revision 1.273  2002/11/14 06:07:56  paulk
;; Bumped JDEE version to 2.2.9.1 to accommodate some minor bug fixes.
;;
;; Revision 1.272  2002/11/06 04:52:45  paulk
;; Fixed bug that I introduced into jde-get-jdk-prog.
;;
;; Revision 1.271  2002/11/05 07:27:55  paulk
;; Define jde-get-jdk-prog function. Thanks to Andrew Hyatt.
;;
;; Revision 1.270  2002/10/25 05:02:42  paulk
;; Use file-truename in jde-reload-project-file to eliminate symbolic links. Thanks to
;; "mark a. foltz" <mfoltz@ai.mit.edu>.
;;
;; Revision 1.269  2002/10/22 05:04:16  paulk
;; Remove require for jde-make package (now autoloaded) and add a menu item for displaying the jde-make customization buffer to the JDEE menu.
;;
;; Revision 1.268  2002/10/21 04:49:34  paulk
;; - Remove require for ant package. This package is now autloaded.
;; - Defer version checking for speedbar until it is used.
;;
;; Revision 1.267  2002/10/16 05:04:24  paulk
;; Fixes regression where the JDEE was not honoring jde-project-context-switching-enabled-p when loading Java files. Thanks to Andy Piper.
;;
;; Revision 1.266  2002/10/11 06:17:56  paulk
;; Use add-to-list instead of setq to add the jde to auto-mode-alist. This avoids multiple entries. Thanks to Klaus Berndl for suggesting this change.
;;
;; Revision 1.265  2002/10/11 05:53:19  paulk
;; Added more packages to the list of packages that are demand loaded. This is intended to reduce the startup time for the JDEE.
;;
;; Revision 1.264  2002/09/30 05:21:07  paulk
;; Removed require forms for the following packages: jde-ejb, jde-javadoc,
;; and jde-checkstyle. These packages are now loaded only if the user
;; access one of the commands that they define.
;;
;; Revision 1.263  2002/09/26 06:17:34  paulk
;; Added templates to the JDE New menu for creating EJB session and entity beans.
;; The templates were created by David T. Smith.
;;
;; Revision 1.262  2002/09/16 05:05:56  paulk
;; Cygwin Emacs compatibility fix. Check for Cygwin Emacs when processing paths. Thanks
;; to Klaus Berndl.
;;
;; Revision 1.261  2002/09/11 03:40:05  paulk
;; * Adds jde-devel-debug variable. This variable is intended to enable
;;   features that facilitate debugging the JDEE itself.
;;
;; * Bumps JDEE's version number 2.2.9beta12
;;
;; Revision 1.260  2002/09/06 12:58:05  paulk
;; Adds a Check Style command to the JDE menu. This command requires that
;; checkstyle-all.jar be in the JDEE's java/lib directory.
;;
;; Revision 1.259  2002/09/02 05:16:55  paulk
;; Fixed a bug in jde-normalize-path whereby it was expanding a relative path
;; even when jde-resolve-relative-paths-p was false.
;;
;; Revision 1.258  2002/08/30 20:58:26  jslopez
;; Adds key bindings for jde-gen-try-finally-wrapper, jde-gen-try-catch-wrapper,
;; jde-wiz-extend-abstract-class, and jde-wiz-override-method.
;;
;; Revision 1.257  2002/08/27 05:28:02  paulk
;; Fixed cygwin support bug in jde-root-dir-p. Thanks to Andy Piper.
;;
;; Revision 1.256  2002/08/07 06:36:14  paulk
;; Removed code intended to eliminate spurious warnings when byte-compiling the JDEE. The
;; removed code now resides in a separate package, jde-compat.el. Thanks to Andy Piper
;; for suggesting this restructuring. Also fixed a number of compiler warnings caused
;; by genuine bugs.
;;
;; Revision 1.255  2002/06/21 05:15:19  paulk
;; Conditioned the :set-after option out of jde-jdk for versions of Emacs
;; earlier than Emacs 21. This is so users will not need Emacs 21 to run
;; the JDEE. Thanks to YAMANO Yuji.
;;
;; Revision 1.254  2002/06/21 04:52:27  paulk
;; XEmacs compatibility fix: set directory sep character to be a forward slash
;; for project files. This fixes a project file save bug that corrupted
;; project files under some circumstances. Thanks to Hanson Char <HChar@realm.com>
;; for diagnosing this bug.
;;
;; Revision 1.253  2002/06/17 07:41:37  paulk
;; Updated version number.
;;
;; Revision 1.252  2002/06/17 07:24:07  paulk
;; Updated the JDEE's applet debugging command to
;; work with its new jdb interface.
;;
;; Revision 1.251  2002/06/12 06:58:42  paulk
;; XEmacs compatibility fix: removed unsupported :set-after clause from
;; the definition of jde-jdk. The result may be that JDK registry settings
;; may not persist in some cases on XEmacs. I will restore this clause
;; as soon as XEmacs supports it.
;;
;; Revision 1.250  2002/06/04 20:13:26  mnl
;; JDE menus are now defined by custom-variables. This allows especially
;; project specific menus that are automatically switched when changing
;; projects (as is usual with jde-... custom variables).
;;
;; Revision 1.249  2002/05/31 19:02:27  mnl
;; Added new template to generate a new interface from scratch.
;;
;; Revision 1.248  2002/05/29 04:47:30  paulk
;; jde-cygwin-path-converter-internal now substitutes forward slashes for
;; backslashes in paths returned by mswindows-cygwin-to-win32-path.
;;
;; Revision 1.247  2002/05/26 12:44:16  jslopez
;; Rebinds C-C C-v C-y from jde-show-class-source to
;; jde-open-class-at-point.
;;
;; Revision 1.246  2002/05/23 05:28:47  paulk
;; Patch jde-root-dir-p to support the Cygwin version of Xemacs 21.4.6, which
;; accepts native Windows path names. Thanks to Michael Lipp.
;;
;; Revision 1.245  2002/05/20 05:03:31  paulk
;; Used defcustom's :set-after option to specify that jde-jdk should be
;; set after jde-jdk-registry. This is necessary to prevent jde-jdk-registry
;; from resetting jde-jdk to the first registered jdk rather than the JDK
;; selected by the user.
;;
;; Revision 1.244  2002/05/11 23:02:40  jslopez
;; Removes control characters.
;;
;; Revision 1.243  2002/04/18 04:21:07  paulk
;; Rebound completion key bindings.
;;
;; Revision 1.242  2002/04/16 03:17:05  jslopez
;; Integrates jde-open-source.
;;
;; Revision 1.241  2002/04/15 02:59:28  jslopez
;; Updates jde-complete-select for jde-complete.
;;
;; Revision 1.240  2002/04/02 06:44:54  paulk
;; Rebinds C-v C-c C-. to jde-complete-select.
;;
;; Revision 1.239  2002/03/31 07:49:47  paulk
;; Renamed jde-db-source-directories. The new name is jde-sourcepath.
;;
;; Revision 1.238  2002/03/24 05:34:55  paulk
;; Patched jde-save-close-buffer and jde-save-variable to use find-buffer-visiting
;; to find the buffer containing the prj.elf file instead of looping through
;; the buffer list.
;;
;; Revision 1.237  2002/03/22 05:28:59  paulk
;; Updated doc for jde-build-function variable.
;; Updated my email address.
;;
;; Revision 1.236  2002/03/21 12:34:53  paulk
;; Fixes the jde-build-function variable to default to jde-make.
;;
;; Revision 1.235  2002/03/21 12:30:51  paulk
;; Removed jde-java-build-function. It never worked reliably.
;;
;; Revision 1.234  2002/03/12 04:40:44  paulk
;; Corrected minimum eieio version number.
;; Removed initarg for class slots to silence eieio warning.
;;
;; Revision 1.233  2002/03/12 04:14:12  paulk
;; Updated min and max version numbers for required packages.
;;
;; Revision 1.232  2002/03/05 10:30:32  paulk
;; Replaces jde-lib-directory-name with jde-lib-directory-names. Allows
;; you to specify multiple jar/zip directories in a classpath.
;;
;; Revision 1.231  2002/02/25 20:12:39  jslopez
;; Updates to reflect changes made to beanshell.el
;;
;; Revision 1.230  2002/02/21 05:32:04  paulk
;; Adds the following enhancements to the JDEE's expression
;; search functionality.
;;
;;   - A new customization variable, jde-find-granularity,
;;     that allows you to specify the granularity of the
;;     search (character, word, or line).
;;
;;   - A new command, JDE->Search->Expression... (jde-find-dlg),
;;     that allows you to set all search options interactively
;;     in a popup dialog buffer.
;;
;; Revision 1.229  2002/02/17 13:42:20  paulk
;; jde-submit-problem-report command now includes versions of required packages.
;;
;; Revision 1.228  2002/02/15 02:48:49  jslopez
;; Updates jde-jeval to pass accept 3 arguments.
;;
;; Revision 1.227  2002/01/22 05:38:28  paulk
;; - Bumped jde-version to 2.2.0beta9.
;; - Updated jde-semantic-min/max-version and eieio-min/max-version
;; - Modified jde-build function to use call-interactively to call
;;   the build function.
;;
;; Revision 1.226  2002/01/19 06:42:22  paulk
;; Minor updates.
;;
;; Revision 1.225  2002/01/18 12:49:42  paulk
;; Added a new hook, jde-project-hooks. This variable lists functions to be run
;; when the JDEE switches to a new project.
;;
;; Revision 1.224  2002/01/15 05:53:58  paulk
;; Now requires comint.
;; .
;; .
;; .
;; Old entries deleted to save space.
;;
;; Revision 1.8  1997/06/18 17:20:00  paulk
;; Initial checkin.
;;

;; End of jde.el
