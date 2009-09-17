;;; jde.el -- Integrated Development Environment for Java.
;; $Id$

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes
;; Keywords: java, tools

;; Copyright (C) 1997-2008 Paul Kinnucan.
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
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;;;###autoload
(defconst jde-version "@@{project.version}@@"
  "JDE version number.")

(defconst jde-revision "$Revision$"
  "The subversion revision for this build.")

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

(defconst jde-emacs23p (and (string-match "\\bEmacs\\b" (emacs-version))
			    (>= emacs-major-version 23)))

(unless (fboundp 'custom-set-default)
   (defalias 'custom-set-default 'set-default))

;; Autoloads must be loaded first since move to sole `(require 'jde)' style.
(require 'jde-autoload)

(require 'jde-util)
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

;;;###autoload
(defun jde-version ()
  "Get the version of JDEE."
  (interactive)
  (message "JDEE %s" jde-version))

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
  "Get the version of Java used by the JDEE."
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
    (string-to-number
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
    (string-to-number
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
		    ["Goto Exception"  jde-exception-goto t]
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
	(list "Refactor"
	      [ "Rename Class" jde-rename-class t]
	      [ "Fully Qualify Class" jde-replace-fully-qualified-class-at-point t]
	      [ "Create HTML" jde-htmlize-code t]
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
	   (if separator
	       (substitute ?\: (string-to-char separator) path) path)))

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


;; Problem reporting functions contributed by
;; Phillip Lord <plord < at > hgmp.mrc.ac.uk>.
(defconst jde-problem-report-mail-address
  (concat "jdee-devel" (char-to-string ?@) "lists.sourceforge.net")
  "Send email to this address for JDEE problem reporting.")

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
	    (insert "\n\n\nThe contents of the *JDEBug* buffer were\n\n")
	    (insert-buffer-substring debug-buffer)
	    (insert "\n\n\nEnd Insert *JDEbug* buffer" ))
	(insert "\n\n\nThere was no *JDEBug* buffer" ))

      ;;insert the contents of the CLI buffer if it exists.
      (if cli-buffer
	  (progn
	    (insert "\n\n\nThe contents of the CLI buffer are\n\n")
	    (insert-buffer-substring cli-buffer)
	    (insert "\n\n\nEnd Insert CLI buffer" ))
	(insert "\n\n\nThere is no CLI buffer" ))


      ;;insert the contents of the locals buffer if it exists.
      (if locals-buffer
	  (progn
	    (insert "\n\n\nThe contents of the locals buffer are\n\n")
	    (insert-buffer-substring locals-buffer)
	    (insert "\n\n\nEnd Insert locals buffer" ))
	(insert "\n\n\nThere is no locals buffer" ))

      ;;insert the contents of the backtrace buffer if it is there.
      (if backtrace-buffer
	  (progn
	    (insert "\n\n\nThe contents of the *Backtrace* buffer were\n\n")
	    (insert-buffer-substring backtrace-buffer)
	    (insert "\n\n\nEnd Insert *Backtrace* buffer" ))
	(insert "\n\n\nThere was no *Backtrace* buffer" ))


      ;;insert the contents of the messages buffer if it is there.
      (if messages-buffer
	  (progn
	    (insert "\n\n\nThe contents of the *Messages* buffer were\n\n")
	    (insert-buffer-substring messages-buffer)
	    (insert "\n\n\nEnd Insert *Messages* buffer" ))
	(insert "\n\n\nThere was no *Messages* buffer" ))

      ;;insert the contents of the jde-log buffer if it is there.
      (if jde-log-buffer
	  (progn
	    (insert "\n\n\nThe contents of the *jde-log* buffer were\n\n")
	    (insert-buffer-substring jde-log-buffer)
	    (insert "\n\n\nEnd Insert *jde-log* buffer" ))
	(insert "\n\n\nThere was no *jde-log* buffer" )))

    (when process-environment
      (insert "\n\n\nProcess environment: \n\n")
      (insert (mapconcat (lambda (var) var) process-environment "\n")))

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
       "other files, please attempt to replicate the bug, using the\n")

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
	    (insert
	     (format "\n\n\nThe contents of the %s file was\n\n\n"
		     init-file-name))

	    (if (featurep 'xemacs)
		(insert-file-contents "~/.xemacs/init.el")
	      (insert-file-contents "~/.emacs"))
	    (goto-char (point-max))
	    (insert
	     (format "\n\n\n=====end inserted %s file"
		     init-file-name)))
	(insert
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
  "Displays JDE key bindings. Use `jde-bug-keys' to display JDEbug
keybindings."
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

(defun jde-assert-mode (&optional no-raise-p)
  "Maybe raise an error if the current buffer isn't a JDEE mode buffer.
NO-RAISE-P, if non-`nil', don't raise an error if this insn't a JDEE mode
buffer, otherwise, return whether or not it is a legitimate buffer."
  (if (and (not no-raise-p) (not (eq major-mode 'jde-mode)))
      (error "Not visiting a Java source file.")
    (eq major-mode 'jde-mode)))

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

;; End of jde.el
