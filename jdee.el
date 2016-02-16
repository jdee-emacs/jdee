;;; jdee.el --- Java Development Environment for Emacs

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes
;; Keywords: java, tools
;; URL: http://github.com/jdee-emacs/jdee
;; Version: 2.4.2
;; Package-Requires: ((emacs "24.3"))

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
;;
;; minimum Emacs version supported is 24.3

;;; Code:

(require 'beanshell)
(require 'browse-url)
(require 'cc-defs)
(require 'cc-mode)
(require 'cedet)
(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'cus-edit)
(require 'easymenu)
(require 'efc)
(require 'executable)  ;; in XEmacs' sh-script package
(require 'jdee-annotations)
(require 'jdee-bug)
(require 'jdee-bsh)
(require 'jdee-class)
(require 'jdee-compile)
(require 'jdee-complete)
(require 'jdee-custom)
(require 'jdee-db)
(require 'jdee-gen)
(require 'jdee-help)
(require 'jdee-import)
(require 'jdee-font-lock)
(require 'jdee-java-grammar)
(require 'jdee-jdb)
(require 'jdee-jdk-manager)
(require 'jdee-open-source)
(require 'jdee-project-file)
(require 'jdee-run)
(require 'jdee-util)
(require 'jdee-which-method)
(require 'jdee-wiz)
(require 'semantic)
(require 'thingatpt)

;;;###autoload
(defconst jdee-version "2.4.2"
  "JDEE version number.")

(defconst jdee-revision "$Revision$"
  "The subversion revision for this build.")

(defconst jdee-cedet-min-version "1.0beta2"
  "Cedet minimum version")
(defconst jdee-cedet-max-version "2.0"
  "Cedet maximum version")

(unless (fboundp 'custom-set-default)
  ;; FIXME: for xemacs?
  (defalias 'custom-set-default 'set-default))

(defgroup jdee nil
  "Java Development Environment"
  :group 'tools
  :prefix "jdee-")

(defcustom jdee-check-version-flag t
  "*Non-nil means to check versions of semantic, eieio, and speedbar.
That is if they meet the requirements for this version of the JDE.
If nil only check if semantic, eieio, and speedbar are available.
See also the function `jdee-check-versions'."
  :group 'jdee
  :type 'boolean)

;; (makunbound 'jdee-key-bindings)
(defcustom jdee-key-bindings
  (list
   (cons "[?\C-c ?\C-v ?\C-a]" 'jdee-run-menu-run-applet)
   (cons "[?\C-c ?\C-v ?\C-b]" 'jdee-build)
   (cons "[?\C-c ?\C-v ?\C-c]" 'jdee-compile)
   (cons "[?\C-c ?\C-v ?\C-d]" 'jdee-debug)
   (cons "[?\C-c ?\C-v ?\C-f]" 'jdee-find)
   (cons "[?\C-c ?\C-v ?\C-g]" 'jdee-open-class-at-point)
   (cons "[?\C-c ?\C-v ?\C-k]" 'jdee-bsh-run)
   (cons "[?\C-c ?\C-v ?\C-l]" 'jdee-gen-println)
   (cons "[?\C-c ?\C-v ?\C-n]" 'jdee-help-browse-jdk-doc)
   (cons "[?\C-c ?\C-v ?\C-p]" 'jdee-save-project)
   (cons "[?\C-c ?\C-v ?\C-q]" 'jdee-wiz-update-class-list)
   (cons "[?\C-c ?\C-v ?\C-r]" 'jdee-run)
   (cons "[?\C-c ?\C-v ?\C-s]" 'speedbar-frame-mode)
   (cons "[?\C-c ?\C-v ?\C-t]" 'jdee-jdb-menu-debug-applet)
   (cons "[?\C-c ?\C-v ?\C-w]" 'jdee-help-symbol)
   (cons "[?\C-c ?\C-v ?\C-x]" 'jdee-show-superclass-source)
   (cons "[?\C-c ?\C-v ?\C-y]" 'jdee-open-class-at-point)
   (cons "[?\C-c ?\C-v ?\C-z]" 'jdee-import-find-and-import)
   (cons "[?\C-c ?\C-v ?e]"    'jdee-wiz-extend-abstract-class)
   (cons "[?\C-c ?\C-v ?f]"    'jdee-gen-try-finally-wrapper)
   (cons "[?\C-c ?\C-v ?i]"    'jdee-wiz-implement-interface)
   (cons "[?\C-c ?\C-v ?j]"    'jdee-javadoc-autodoc-at-line)
   (cons "[?\C-c ?\C-v ?o]"    'jdee-wiz-override-method)
   (cons "[?\C-c ?\C-v ?t]"    'jdee-gen-try-catch-wrapper)
   (cons "[?\C-c ?\C-v ?z]"    'jdee-import-all)
   (cons "[?\C-c ?\C-v ?\C-[]" 'jdee-run-etrace-prev)
   (cons "[?\C-c ?\C-v ?\C-]]" 'jdee-run-etrace-next)
   (cons "[(control c) (control v) (control ?.)]" 'jdee-complete)
   (cons "[(control c) (control v) ?.]" 'jdee-complete-in-line)
   )
  "*Specifies key bindings for the JDE.
The value of this variable is an association list. The car of
each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To enter
a key with a modifier, type C-q followed by the desired modified
keystroke. For example, to enter C-s (Control s) as the key to be
bound, type C-q C-s in the key field in the customization buffer.
You can use the notation [f1], [f2], etc., to specify function keys."
  :group 'jdee-project
  :type '(repeat
	  (cons :tag "Key binding"
		(string :tag "Key")
		(function :tag "Command")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buf)
	     (save-excursion
	       (set-buffer buf)
	       (when (boundp 'jdee-mode-map)
		 ;; Unmap existing key bindings
		 (if (and (boundp 'jdee-key-bindings)
			  jdee-key-bindings)
		     (mapc
		      (lambda (binding)
			(let ((key (car binding)))
			  (if (string-match "\\[.+]" key)
			      (setq key (car (read-from-string key))))
			  (local-unset-key key)))
		      jdee-key-bindings))
		 ;; Map new key bindings.
		 (mapc
		  (lambda (binding)
		    (let ((key (car binding))
			  (fcn (cdr binding)))
		      (if (string-match "\\[.+]" key)
			  (setq key (car (read-from-string key))))
		      (define-key (current-local-map) key fcn)))
		  val))))
	   (jdee-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jdee-launch-beanshell-on-demand-p t
  "If non-nil, the JDEE launches the Beanshell the first time it is needed.
Otherwise, the JDEE launches the Beanshell, if it is not already running,
whenever you open a Java source file."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-java-environment-variables '("JAVA_VERSION" "JAVA_HOME")
  "This variable specifies the names of environment variables used to
specify the version and location of the JDK to be used by the JDE.
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
  "Specifies the version of the JDK to be used to develop the
current project.

This will be set to nil by default if the Java version
environment variable (see `jdee-java-enviroment-variables') is
set. Otherwise it defaults to the first JDK registered in
`jdee-jdk-registry'. If that variable is nil, then this will
default to nil.

The version must be one of the versions listed in the
`jdee-jdk-registry'. If you specify nil, the JDEE uses the
JDK specified by the Java version environment variable (see
`jdee-java-enviroment-variables'), if set; otherwise, the first JDK
located on the system command path specified by the PATH environment
variable is used (on Mac OS X the default Java installation is tried
first).

You must customize `jdee-jdk-registry' first, then `jdee-jdk'. After you
have customized jdee-jdk-registry, the customization buffer for`
jdee-jdk' presents you with a set of radio buttons, one for each
registered JDK.  Select the button of the JDK that you want to use for
the current project."
  :group 'jdee-project
  :type 'symbol
  :set-after '(jdee-jdk-registry))

;;;###autoload
(defun jdee-version ()
  "Get the version of JDEE."
  (interactive)
  (message "JDEE %s" jdee-version))

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
	(error "Cannot find the JDK directory. See `jdee-jdk'."))))))

(defun jdee-get-jdk-prog (progname)
   "Returns the full path of the program passed in.  By default, assume
   it's in the bin directory under `jdee-get-jdk-dir', but if not,
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
	    (jdee-get-jdk-dir))))
     (if (file-exists-p progpath)
       progpath
       (executable-find full-progname))))


(defun jdee-get-tools-jar ()
  "Gets the correct tools.jar or equivalent. Signals an
error if it cannot find the jar."
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
      (error (concat "Cannot find JDK's tools jar file (or equivalent)."
		     "Type M-x describe-function [RET] jdee-get-jdk-dir for more info.")))))

(defvar jdee-java-version-cache nil
"Cache to hold the version of Java being used.")

(defun jdee-java-version-via-java ()
  "Get the version of the java vm on the
system command path."
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
	  (re-search-forward "[1-9][.][1-9]" (point-max) t)
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
	  (if (jdee-bsh-running-p)
	      (progn
		(setq jdee-java-version-cache
		      (jdee-jeval-r "jde.util.JdeUtilities.getJavaVersion();"))
		(setq java-version jdee-java-version-cache))
	    (setq java-version (jdee-java-version-via-java)))))
    (if (called-interactively-p 'interactive)
	(message java-version)
      java-version)))

(defun jdee-java-major-version ()
  "Returns an integer representing
the major version of the JDK being used
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
  "Returns an integer representing
the minor version of the JDK being used
by the current project."
  (let ((version (jdee-java-version)))
    (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)"
		version)
    (string-to-number
     (substring
     version
     (match-beginning 2)
     (match-end 2)))))


;;(makunbound 'jdee-jdk-doc-url)
(defcustom jdee-jdk-doc-url ""
  "*URL of JDK documentation.
This can point to a remote or local copy of the documentation. If the value
of this variable is the null string, the JDEE looks for the JDK documentation
in the docs subdirectory of the directory returned by `jdee-get-jdk-dir'."
  :group 'jdee-project
  :type 'file)

;;(makunbound 'jdee-global-classpath)
(defcustom jdee-global-classpath nil
  "Specify a common classpath for compile, run, and debug commands.
Use this variable if you want to the JDEE to use the same classpath for
compiling, running,and debugging an application. Note that the value
of this variable is a list of strings, each of which specifies a
path. The JDEE converts this list to a colon- or semicolon-separated
list before inserting in the compiler or vm command line.

The path may start with a tilde (~) or period (.) and may include
environment variables. The JDEE replaces a ~ with your home directory.
If `jdee-resolve-relative-paths-p' is nonnil, the JDEE replaces the
. with the path of the current project file. The JDEE replaces each
instance of an environment variable with its value before inserting it
into the command line.

You can specify different classpaths for compiling, running and
debugging applicaitons. Use `jdee-compile-option-classpath' to specify
the compilation classpath, `jdee-run-option-classpath' to specify the
run classpath, and/or `jdee-db-option-classpath' to specify the debug
classpath. You can use these variables together. For example, suppose
that you need to use one classpath for compilation and other for
running and debugging. You could do this by setting
`jdee-compile-option-classpath' to the compile classpath and
`jdee-global-classpath' to the run and debug classpath. If you set
`jdee-global-classpath', the JDEE uses it to construct the classpath for
any operation for which you do not set the operation-specific
classpath variable (e.g., `jdee-compile-option-classpath').

If you do not set `jdee-global-classpath', the JDEE uses the operation-specific
classpath if it is set. If neither the global nor the
operation-specific classpath is set, the JDEE does not generate a
-classpath argument for the operation, e.g., compile or run a Java
class. In this case, the operation uses the value of the CLASSPATH variable
if specified."
  :group 'jdee-project
  :type '(repeat (file :tag "Path")))

(defcustom jdee-quote-classpath t
  "*Quote the classpath argument.
Set this option on when using the bash shell with Windows 95 or NT.
The semicolons in the classpath confuse the shell."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-expand-classpath-p t
  "Replace each occurence of a directory named `jdee-lib-directory-names'
 in the classpath with paths to the jar and zip files in that directory."
  :group 'jdee-project
  :type 'boolean)

;; (makunbound 'jdee-lib-directory-names)
(defcustom jdee-lib-directory-names (list "/lib$" "/jar$")
  "Regular expressions that matches names of jar/zip directories for
the current project. See `jdee-expand-classpath-p' and
`jdee-expand-classpath' for more information"
  :group 'jdee-project
  :type '(repeat (string :tag "Name")))

(defcustom jdee-lib-excluded-file-names nil
   "Regular expressions that matches names of jar or zip files that should
 be excluded when expanding a library specified by `jdee-lib-directory-names'."
   :group 'jdee-project
   :type '(repeat (string :tag "Name")))


;; (makunbound 'jdee-sourcepath)
;; FIXME: use compilation-search-path instead?
(defcustom jdee-sourcepath nil
  "*List of source directory paths.  The JDEE uses this list to locate
source files corresponding to class files.  When entering paths in the
custom buffer, enter each path as a separate item in a separate edit
field. Do NOT put more than one path in the same edit field. You'll
only confuse JDE.  Paths may contain environment variables or wildcards."
  :group 'jdee-project
  :type '(repeat (file :tag "Path")))

(defcustom jdee-build-function 'jdee-make
  "*Function that will be invoked by the `jdee-build' command.
The `jdee-make' function uses a make
program to rebuild the project. The `jdee-ant-build' function
uses the Apache Ant program to build the project. You may also
specify a custom function to use. The custom function must
be an interactive function that can be called by
`call-interactively'."
  :group 'jdee-project
  :type '(radio
	  (const :tag "Make" jdee-make)
	  (const :tag "Ant" jdee-ant-build)
	  (function :tag "Custom function" identity)))

;;(makunbound 'jdee-debugger)
(defcustom jdee-debugger (list "jdb")
  "Specify the debugger you want to use to debug Java programs.
Select jdb, if you want to use the default version of jdb for the JDK
used by the current project (see `jdee-jdk'). Select old jdb, if you
are using JDK 1.2.2 or later and want to use the the old (e.g., pre-JPDA)
version of jdb instead of the new (JPDA-based) version of jdb."
  :group 'jdee-project
  :type '(list
	  (radio-button-choice
	  (item "jdb")
	  (item "old jdb")))
  :set '(lambda (sym val)
	  (mapc
	   (lambda (buff)
	     (save-excursion
	       (set-buffer buff)
	       (if (string= (car val) "JDEbug")
		   (progn
		     (jdee-jdb-minor-mode -1)
		     (jdee-bug-minor-mode 1))
		 (progn
		   (jdee-jdb-minor-mode 1)
		   (jdee-bug-minor-mode -1)))))
	   (jdee-get-java-source-buffers))
	  (set-default sym val)))

(defcustom jdee-devel-debug nil
  "If true, use the JDEE Java classes in the jde/java/classes
directory instead of the jde.jar. This variable is intended for
use in testing the JDEE's java classes."
  :group 'jdee-project
  :type 'boolean)

(defcustom jdee-enable-abbrev-mode nil
"*Enable expansion of abbreviations in jdee-mode.
See `jdee-mode-abbreviations' for more information."
  :group 'jdee-project
  :type 'boolean
  :set '(lambda (sym val)
	  (set-default sym val)
	  (if (featurep 'jdee) ;; skip initial set.
	      (mapc
	       (lambda (buf)
		(with-current-buffer buf
		  (setq abbrev-mode val)
		  (when abbrev-mode
		    (setq local-abbrev-table (make-abbrev-table))
		    (jdee-init-abbrev-table))))
	       (jdee-get-project-source-buffers)))))

(defcustom jdee-mode-abbreviations
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
`jdee-enable-abbrev-mode'). To use an abbreviation, enter the
abbreviation followed by a white-space character. To suppress
expansion, enter C-q white-space."
  :group 'jdee-project
  :type '(repeat
	  (cons :tag "jdee-mode abbreviation"
	      (string :tag "Abbreviation")
	      (string :tag "Expansion")))
  :set '(lambda (sym val)
	  (set-default sym val)
	  (if (and
	       (featurep 'jdee)
	       jdee-enable-abbrev-mode)
	      (progn
		(mapc
		 (lambda (buf)
		   (with-current-buffer buf
		     (setq local-abbrev-table (make-abbrev-table))
		     (jdee-init-abbrev-table)))
		 (jdee-get-project-source-buffers))))))

(defun jdee-init-abbrev-table ()
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
	 t
	 (lambda ()
	   (unless (jdee-parse-comment-or-quoted-p)
	     (delete-char (- (length abbrev))) ; remove abbreviation and
	     (insert expansion)))                   ; insert expansion
	 0)))
   jdee-mode-abbreviations)

  (if jdee-gen-cflow-enable
      (jdee-gen-load-abbrev-templates))

  (setq abbrevs-changed nil))

;; The next two functions contributed by s.nicolas@videotron.ca
(defun jdee-abbrev-mode ()
"*Toggle abbreviation mode in JDEE without altering project settings.
See `jdee-mode-abbreviations' for more information."
 (interactive)
  (setq jdee-enable-abbrev-mode (not jdee-enable-abbrev-mode))
  (setq abbrev-mode jdee-enable-abbrev-mode)
  (when jdee-enable-abbrev-mode
     ;; Define abbreviations.a
    (jdee-init-abbrev-table))
  (if jdee-enable-abbrev-mode
      (message "abbreviation mode on")
    (message "abbreviation mode off")))

(defun jdee-show-abbrevs ()
"*Shows a popup menu containing all available expansions.
See `jdee-mode-abbreviations' for more information."
  (interactive)
  (when (fboundp 'imenu--mouse-menu)
    ;; not all users want imenu loaded
    (let* ((expansions
	    (mapcar
	     (lambda(x) (cons (cdr x) (car x)))
	     jdee-mode-abbreviations))
	   (expansion (car (imenu--mouse-menu expansions t "Abbreviations"))))
      (insert expansion))))


(defvar jdee-classpath-separator (if (member system-type '(cygwin32 cygwin))
				    ";" path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")


;;;###autoload
(defun jdee-set-global-classpath (classpath)
  "Set the value of `jdee-global-classpath'.
It specifies the -classpath argument for the Java compiler and
interpreter."
  (interactive
   "sEnter classpath: ")
  (custom-set-variables
   '(jdee-global-classpath (split-string classpath jdee-classpath-separator) t)))

(defun jdee-show-run-options ()
  "Show the JDEE Run Options panel."
  (interactive)
  (customize-apropos "jdee-run-options" 'groups))

(defun jdee-show-debug-options ()
  "Show the JDEE Debug Options panel."
  (interactive)
  (customize-apropos "jdee-db-options" 'groups))

(defun jdee-show-project-options ()
  "Show the JDEE Debug Options panel."
  (interactive)
  (customize-apropos "jdee-project" 'groups))

(defun jdee-show-autocode-options ()
  "Show the JDEE Autocode panel."
  (interactive)
  (customize-apropos "jdee-gen" 'groups))

(defun jdee-show-wiz-options ()
  "Show the JDEE Wizards Options panel."
  (interactive)
  (customize-apropos "jdee-wiz" 'groups))

(defun jdee-show-complete-options ()
  "Show the JDEE Complete Options panel."
  (interactive)
  (customize-apropos "jdee-complete" 'groups))

;;;###autoload
(defun jdee-build ()
  "Rebuild the entire project.
This command invokes the function defined by `jdee-build-function'."
  (interactive)
  (call-interactively jdee-build-function))

(defvar jdee-monitor-post-command-hook-timer nil
"Timer that runs `jdee-monitor-post-command-hook' during
idle moments.")

; (define-derived-mode
;   jdee-mode java-mode "JDEE"
;   "Major mode for developing Java applications and applets.
;   \\{jdee-mode-map}"
;   (jdee-mode-internal)
; )

;; The following is the expansion of the above macro.
;; We include the expansion to permit autoload on jdee-mode.
(derived-mode-init-mode-variables 'jdee-mode)
(put 'jdee-mode 'derived-mode-parent 'java-mode)

;;;###autoload
(defun jdee-mode ()
  "Major mode for developing Java applications and applets.
\\{jdee-mode-map}"
  (interactive)
  (condition-case err
      (progn
	(jdee-check-versions)
        (require 'jdee-plugins)

	(add-to-list 'semantic-new-buffer-setup-functions
		     '(jdee-mode . jdee-parse-semantic-default-setup))

	(java-mode)
	(if (get 'java-mode 'special)
	    (put 'jdee-mode 'special t))
	(setq major-mode 'jdee-mode)
	(setq mode-name "JDEE")
	(derived-mode-set-keymap 'jdee-mode)
	(derived-mode-set-syntax-table 'jdee-mode)
	(derived-mode-set-abbrev-table 'jdee-mode)

	;; Define buffer-local variables.
	(make-local-variable 'jdee-project-name)
	(make-local-variable 'jdee-run-applet-document)

	(setq jdee-current-project
	      (or (jdee-find-project-file default-directory)
		  "")) ;; Avoid setting startup values twice!

	(setq jdee-buffer-project-file jdee-current-project)

	;; Load the project file for this buffer. The project file
	;; defines JDEE options for a project.
	(if (and (not (jdee-debugger-running-p)) jdee-project-context-switching-enabled-p)
	    (jdee-load-project-file))

	;; Enable support for automatic project switching.
	;; This feature loads the appropriate project settings whenever
	;; a user switches from a Java buffer belonging to one project
	;; to a buffer belonging to another.
	(add-hook 'post-command-hook
		  'jdee-detect-java-buffer-activation
		  nil
		  t ;; XEmacs ignores this argument if symbol is not local.
		  )

	(unless jdee-monitor-post-command-hook-timer
	  (setq
	   jdee-monitor-post-command-hook-timer
	   (run-with-idle-timer 1 t 'jdee-monitor-post-command-hook)))

	(unless (member 'jdee-clean-up-after-jde kill-buffer-hook)
	  (add-hook 'kill-buffer-hook 'jdee-clean-up-after-jde))

        ;; Define underscore as a word constituent. This is needed
	;; to support coding styles the begin fields with an underscore.
	(modify-syntax-entry ?_ "w")

	(when jdee-enable-abbrev-mode
	  ;; Define abbreviations.
	  (jdee-init-abbrev-table)
	  (abbrev-mode 1))

	;; Reset the key bindings in case jdee-mode-keymap
	;; was not bound at startup.
	(custom-initialize-reset 'jdee-key-bindings nil)

	(make-local-variable 'mode-line-format)
	(setq mode-line-format jdee-mode-line-format)

	;; When looking for a tag that has multiple matches
	;; in the TAGS file, prefer (find first) the
	;; occurrence in the _current_ buffer.
	;; Contributed by Charles Rich, Mitsubishi Electric Research Laboratories,
	;; Cambridge, MA>
	(when (boundp 'tags-table-format-functions)
	  (make-local-variable 'tags-table-format-functions)
	  (add-hook 'tags-table-format-functions 'jdee-etags-recognize-tags-table nil t))

	(if (and
	     (not jdee-launch-beanshell-on-demand-p)
	     (not (jdee-bsh-running-p)))
	    (bsh-launch (oref-default 'jdee-bsh the-bsh)))

	(jdee-wiz-set-bsh-project)

	(wisent-java-default-setup)
	(semantic-mode 1)

	;; Install debug menu.
	(if (string= (car jdee-debugger) "JDEbug")
	    (jdee-bug-minor-mode 1)
	  (jdee-jdb-minor-mode 1))

	(when (boundp 'jdee-mode-map)
	  (let ((key (car (read-from-string "[return]"))))
            (if jdee-electric-return-mode
                (define-key (current-local-map) key 'jdee-electric-return))))

	;; Set up indentation of Java annotations.
	(jdee-annotations-setup)


	;; The next form must be the last executed
	;; by jdee-mode.
	(derived-mode-run-hooks 'jdee-mode))
    (error
     (message "%s" (error-message-string err)))))


(defconst jdee-check-versions-message
   "JDEE requires a version of CEDET between %s and %s (found %s)")

(defun jdee-check-versions ()
  "Check for correct versions of CEDET provided packages.
Signal an error if CEDET is not installed.
When `jdee-check-version-flag' is non-nil, signal an error if the
version of CEDET currently installed doesn't meet the requirements for
this version of the JDEE."
  ;; Check that CEDET is installed.
  (or (boundp 'cedet-version)
      (error jdee-check-versions-message
	     jdee-cedet-min-version
	     jdee-cedet-max-version
	     "none"))
  ;; Check version requirement when requested.
  (or (not jdee-check-version-flag)
      (jdee-check-version cedet-version
			 jdee-cedet-min-version
			 jdee-cedet-max-version)
      (error jdee-check-versions-message
	     jdee-cedet-min-version
	     jdee-cedet-max-version
	     cedet-version)))


(defun jdee-check-version (current-version min-version max-version)
  "Return non-nil if CURRENT-VERSION >= MIN-VERSION or <= MAX-VERSION."
  (and (or (jdee-earlier-versionp current-version
				 max-version)
	   (string= current-version
		    max-version))
       (or (jdee-earlier-versionp min-version
				 current-version)
	   (string= current-version
		    min-version))))

(defun jdee-earlier-versionp (ver1 ver2)
  "Return non-nil if VER1 is earlier than VER2"
  (let ((ver1n (jdee-replace-in-string ver1 "beta" "zb"))
	(ver2n (jdee-replace-in-string ver2 "beta" "zb")))
    (setq ver1n (jdee-replace-in-string ver1n "pre" "zp"))
    (setq ver2n (jdee-replace-in-string ver2n "pre" "zp"))
    (if (string-match "z" ver1n)
	(unless (string-match "z" ver2n)
	  (setq ver2n (concat ver2n "zz")))
      (if (string-match "z" ver2n)
	  (setq ver1n (concat ver1n "zz"))))
    (string< ver1n ver2n)))


(defcustom jdee-log-max 500
  "*Maximum number of lines to keep in the JDEE log buffer.
If nil, disable logging.  If t, don't truncate the buffer."
  :group 'jdee-project
  :type '(choice (integer :tag "Number of lines to keep")
		 (boolean :tag "Disable/Unlimited")))

(defun jdee-log-msg (msg &rest args)
  "Log message MSG to the *jdee-log* buffer.
Optional ARGS are used to `format' MSG.
Does nothing if `jdee-log-max' is nil."
  (if jdee-log-max
      (save-match-data
	(with-current-buffer (get-buffer-create "*jdee-log*")
	  (goto-char (point-max))
	  (insert (apply 'format msg args))
	  (insert "\n")
	  (if (integerp jdee-log-max)
	      (let ((line-cnt 0))
		(while (search-backward "\n" nil t)
		  (setq line-cnt (1+ line-cnt)))
		(goto-char (point-min))
		(while (> line-cnt jdee-log-max)
		  (delete-region (point) (search-forward "\n" nil t))
		  (setq line-cnt (1- line-cnt)))))))))

(defun jdee-log-msg-t (msg &rest args)
  "Log message MSG to the *jdee-log* buffer, and return t.
Optional ARGS are used to `format' MSG.
Does nothing but return t if `jdee-log-max' is nil."
  (jdee-log-msg msg args)
  t)

(defun jdee-log-msg-nil (msg &rest args)
  "Log message MSG to the *jdee-log* buffer, and return nil.
Optional ARGS are used to `format' MSG.
Does nothing but return nil if `jdee-log-max' is nil."
  (jdee-log-msg msg args)
  nil)

;; Make jdee-mode the default mode for Java source code buffers.
;; Prepend the jdee-mode entry so that it shadows the java-mode
;; entry already in the list.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.java\\'" . jdee-mode))

(defcustom jdee-menu-definition
  (list "JDEE"
	["Compile"           jdee-compile t]
	;; ["Run App"           jdee-run (not (jdee-run-application-running-p))]
	["Run App"           jdee-run t]
	["Debug App"         jdee-debug t]
	"-"
	;;["-"                 ignore nil]
	["Run Applet"        jdee-run-menu-run-applet t]
	["Debug Applet"      jdee-debug-applet t]
	"-"
	["Build"             jdee-build t]
	(list "Find"
	      ["Expression"    jdee-find
               (and
                (executable-find
                 (if (eq system-type 'windows-nt) "find.exe" "find"))
                (executable-find
                 (if (eq system-type 'windows-nt) "grep.exe" "grep")))]
	      ["Expression..."  jdee-find-dlg
               (and
                (executable-find
                 (if (eq system-type 'windows-nt) "find.exe" "find"))
                (executable-find
                 (if (eq system-type 'windows-nt) "grep.exe" "grep")))]
              ["Symbol Definition" jdee-open-class-at-point t]
              ["Class"  jdee-show-class-source t]
              ["Super Class"  jdee-show-superclass-source t]
              ["Interface"  jdee-show-interface-source t]
	      )
	(list "Interpreter"
	      ["Start"         jdee-bsh-run t]
	      ["Exit"          jdee-bsh-exit t]
	      "-"
	      ["Help"          jdee-help-beanshell t]
              )
	(list "Documentation"
	      ["Add"             jdee-javadoc-autodoc-at-line (jdee-javadoc-enable-menu-p)]
	      ["Remove"          jdee-javadoc-remdoc-at-line (jdee-javadoc-enable-menu-p)]
	      ["Check This"      jdee-javadoc-checkdoc-at-line (jdee-javadoc-enable-menu-p)]
	      ["Check All"           jdee-javadoc-checkdoc t]
	      ["Generate All"        jdee-javadoc-make t]
	      ["Generate Buffer"     jdee-javadoc-make-buffer t]
	      "-"
	      ["Javadoc Reference"     jdee-javadoc-browse-tool-doc t]
	      "-"
	      [ "Create HTML"    jdee-htmlize-code t]
              )
	"-"
	(list "Code Generation"
	      (list "Templates"
		    ["Get/Set Pair..."  jdee-gen-get-set t]
		    ["Println..."       jdee-gen-println t]
		    (list "Listener"
			  ["Action"          jdee-gen-action-listener t]
			  ["Change"          jdee-gen-change-listener t]
			  ["Window"          jdee-gen-window-listener t]
			  ["Mouse"           jdee-gen-mouse-listener t]
			  )
		    ["Other..."        jdee-gen-code t]
		    )
	      (list "Import"
		    ["Class..."                jdee-import-\find-and-import t]
		    ["All"                     jdee-import-all t]
		    ["All Unique"              jdee-import-all-unique t]
		    "-"
		    ["Expand Package Imports"  jdee-import-expand-imports t]
		    ["Collapse Class Imports"  jdee-import-collapse-imports t]
		    ["Delete Unneeded"         jdee-import-kill-extra-imports t]
		    ["Organize Imports"        jdee-import-organize t]
		    ["Show Unimported Classes" jdee-import-all-show t]
		    )
	      (list "Wizards"
		    ["Override Method"             jdee-wiz-override-method t]
		    ["Implement Interface..."      jdee-wiz-implement-interface t]
		    ["Generate Get/Set Methods"    jdee-wiz-get-set-methods t]
		    ["Generate toString Method"    jdee-wiz-tostring t]
		    ["Update Package Statement"    jdee-package-update t]
		    ["Implement Event Source..."   jdee-wiz-implement-event-source t]
		    ["Extend Abstract Class..."    jdee-wiz-extend-abstract-class t]
		    ["Delegate Methods..."         jdee-wiz-delegate t]
		    "-"
		    ["Update Class List"   jdee-wiz-update-class-list t]
		    )
	      (list "Modes"
		    (vector "Abbrev"
			    'jdee-abbrev-mode
			    :enable t
			    :style 'toggle
			    :selected 'jdee-enable-abbrev-mode)
		    (vector "Electric Return"
			    'jdee-electric-return-mode
			    :enable t
			    :style 'toggle
			    :selected 'jdee-electric-return-mode)
                    ))
	(list "Browse"
	      ["Source Files"          jdee-show-speedbar t]
	      ["Class at Point"        jdee-browse-class-at-point t]
              )
	["Check Style"  jdee-checkstyle]
	(list "Project"
	      (vector "Auto Switch"
		      'jdee-toggle-project-switching
		      :enable t
		      :style 'toggle
		      :selected 'jdee-project-context-switching-enabled-p)
	      (list "Options"
		    ["General"         jdee-show-project-options t]
		    ["Compile"         jdee-compile-show-options-buffer t]
		    ["Run"             jdee-show-run-options t]
		    ["Debug"           jdee-show-debug-options t]
		    ["Goto Exception"  jdee-exception-goto t]
		    ["Autocode"        jdee-show-autocode-options t]
		    ["Javadoc"         jdee-javadoc-customize t]
		    ["Make"            jdee-make-show-options t]
		    ["Ant"             jdee-ant-show-options t]
		    ["Complete"        jdee-show-complete-options t]
		    ["JUnit"           jdee-junit-show-options t]
		    ["Wiz"             jdee-show-wiz-options t]
		    )
	      (list "Project File"
		    ["Create New" jdee-create-new-project t]
		    ["Save"     jdee-save-project t]
		    ["Load"     jdee-load-project-file t]
		    ["Load All" jdee-load-all-project-files t]
		    ;; FIXME: need edit, show
		    )
	      )
	(list "Refactor"
	      [ "Rename Class" jdee-rename-class t]
	      [ "Fully Qualify Class" jdee-replace-fully-qualified-class-at-point t]
	      )
	(list "Help"
	      ["JDEE Users Guide"      jdee-show-help t]
	      ["JDK"                   jdee-help-browse-jdk-doc t]
	      ["JDEE Key Bindings"     jdee-keys t]
	      "-"
	      ["Class..."              jdee-help-class t]
	      ["Class Member..."       jdee-help-class-member t]
	      ["Symbol at Point"       jdee-help-symbol t]
	      "-"
	      ["Submit problem report" jdee-submit-problem-report t]
	      "-"
	      (concat "JDEE " jdee-version)
	      )
	)
  "*The JDEE main menu"
  :group 'jdee-project
  :type 'sexp
  :set '(lambda (sym val)
	  (set-default sym val)
          ;; Define JDEE menu for FSF Emacs.
	  (easy-menu-define jdee-menu
            jdee-mode-map
            "Menu for JDEE."
            val)))

(defcustom jdee-new-buffer-menu
  (list
   "JDEE New"
   ["Class..."         jdee-gen-class-buffer t]
   ["Interface..."     jdee-gen-interface-buffer t]
   ["Console..."       jdee-gen-console-buffer t]
   ["Bean..."          jdee-gen-bean-buffer t]
   ["Unit Test..."     jdee-junit-test-class-buffer t]
   (list
    "EJB"
    ["Session Bean"    jdee-ejb-session-bean-buffer t]
    ["Entity Bean"     jdee-ejb-entity-bean-buffer t])
   (list
    "Build file..."
    ["Makefile"        jdee-gen-makefile-buffer t]
    ["Ant buildfile"   jdee-gen-ant-buildfile-buffer t])
   ["Other..."         jdee-gen-buffer t]
   )
  "*The JDEE New buffer menu"
  :group 'jdee-project
  :type 'sexp
  :set '(lambda (sym val)
	  (set-default sym val)
	  (let* ((menu (if (fboundp 'easy-menu-create-menu)
                           (easy-menu-create-menu
                            (car val) (cdr val))))
                 (menu-name (car val)))
            (define-key-after menu-bar-file-menu [jdee-new]
              (cons menu-name menu)
              'open-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Classpaths                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	(error "Cannot find cygpath executable."))
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
  "Convert cygwin style PATH to a form acceptable to java vm.  Basically
converts paths of the form: '//C/dir/file' or '/cygdrive/c/dir/file' to
'c:/dir/file'.  This function will not modify standard unix style paths
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
  "Convert cygwin style PATH to a form acceptable to java vm, using
the conversion function specified by `jdee-cygwin-path-converter'."
  (interactive "sPath: ")
  (funcall (car jdee-cygwin-path-converter)
	   (if separator
	       (cl-substitute ?\: (string-to-char separator) path) path)))

(defcustom jdee-resolve-relative-paths-p t
  "If this variable is non-nil, the JDEE converts relative paths to
absolute paths. The JDEE does this by appending the relative path to the path
of the project file for the current source buffer, if such
a file exists. Otherwise, the JDEE appends the relative path to the path
of the current directory."
  :group 'jdee-project
  :type 'boolean)

(defun jdee-normalize-path (path &optional symbol)
  "This function performs the following transformation on PATH:

  * Replaces environment variables of the form $VAR or ${VAR} with
    their values. Note that you must use the Unix notation for
    environment variables on the native Windows versions of Emacs and
    XEmacs.

  * Replaces the tilde character with the value of the home directory,
    typically specified by the HOME environment variable.

  * Converts Cygwin style paths to DOS notation on Windows.

  * Converts relative paths to absolute paths if
    `jdee-resolve-relative-paths-p' is non-nil.  Paths are resolved
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
	 jdee-resolve-relative-paths-p
	 (> len 0)
	 (eq (aref p 0) ?.))
	(let* (prj-file-path
	       (dir (file-name-directory (or (buffer-file-name)
					     default-directory))))
	  ;; find the deepest originating project for the symbol
	  ;; based on the current directory, and resolve to that
	  ;; project's directory
	  (if symbol
	      (let ((prjs (get symbol 'jdee-project))
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
			  (jdee-find-project-file dir))))
	    (setq prj-file-path
		  (jdee-find-project-file dir)))
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
      ;; jdee-resolve-relative-paths-p is false.
      (if (not
	   (or
	    (string= p ".")
	    (string-match "[.]/" p)))
	  (setq p (expand-file-name p))))
    (setq p (jdee-convert-cygwin-path p))
    p))

(defcustom jdee-expand-wildcards-in-paths-p t
  "Expands entries in the 'jdee-sourcepath which are wildcards patterns into a list of matching files or directories which are interpolated into the sourcepath list."
   :group 'jdee-project
   :type 'boolean)

(defmacro jdee-normalize-paths (pathlist &optional symbol)
  "Normalize all paths of the list PATHLIST and return a list with the
expanded paths.  SYMBOL is passed to `jdee-normalize-path' to expand
relative paths."
  `(mapcar (lambda (path)
			 (jdee-normalize-path path ,symbol))
		   ,pathlist))

(defun jdee-expand-wildcards-and-normalize (path &optional symbol)
  "Expand any entries with wildcard patterns in path and interpolate them into the result"
  (if jdee-expand-wildcards-in-paths-p
      (cl-mapcan
       (lambda (path)
	 (let ((exp-paths (file-expand-wildcards path)))
	   (if exp-paths exp-paths (list path))))
       (jdee-normalize-paths path symbol))
    (jdee-normalize-paths path symbol)
    ))


(defun jdee-directory-files-recurs (dir &optional include-regexp)
  "Get all the files in DIR, and any subdirectories of DIR, whose
names match INCLUDE-REGEXP."
  (let (files)
    (loop for file in (directory-files dir) do
	  (if (not (member file '("." "..")))
	      (let ((file (concat dir "/" file)))
	      (if (file-directory-p file)
		  (setq files (append files (jdee-directory-files-recurs file include-regexp)))
		(if (or (not include-regexp)
			(string-match include-regexp file))
		      (setq files (append files (list file))))))))
    files))

(defun jdee-expand-directory (dir include-regexp exclude-regexps symbol)
  "Get all the files in DIR whose names match INCLUDE-REGEXP except those whose
root names match EXCLUDE-REGEXPS. Return the files normalized against SYMBOL."
  (mapcar
   (lambda (included-file)
     (jdee-normalize-path included-file symbol))
   (cl-remove-if
    (lambda (file-path)
      (let ((file-name
	      (file-name-nondirectory file-path)))
	(catch 'match
	    (loop for regexp in exclude-regexps do
		  (if (string-match regexp file-name)
		      (throw 'match t))))))
    (jdee-directory-files-recurs dir include-regexp))))


(defun jdee-expand-classpath (classpath &optional symbol)
  "If `jdee-expand-classpath-p' is nonnil, replaces paths to
directories that match `jdee-lib-directory-names' with paths to jar or
zip files in those directories, excepting those specified by
`jdee-lib-excluded-file-names'. This function assumes that the
existing paths are already normalized."
  (if jdee-expand-classpath-p
      (let (paths)
	(loop for path in classpath do
	      (if (and
		   (file-exists-p path)
		   (file-directory-p path)
		   (cl-member-if
		    (lambda (lib-name) (string-match lib-name path))
		    jdee-lib-directory-names))
		  (progn
		    (setq paths
			  (append
			   paths
			   (jdee-expand-directory
			    path
			    "\\.jar$"
			    jdee-lib-excluded-file-names
			    symbol)))
		    (setq paths
			  (append
			   paths
			   (jdee-expand-directory
			    path
			    "\\.zip$"
			    jdee-lib-excluded-file-names
			    symbol))))
		(setq paths (append paths (list path)))))
	paths)
    classpath))


(defun jdee-build-classpath (paths &optional symbol quote-path-p)
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
    (jdee-expand-classpath
     (mapcar
      (lambda (path)
	(jdee-normalize-path path symbol))
      paths)
     symbol)
   jdee-classpath-separator))

(defun jdee-global-classpath ()
  "Builds a classpath string from the path entries in
`jdee-global-classpath'."
  (jdee-build-classpath 'jdee-global-classpath))


(defun jdee-build-path-arg (arg path-list &optional quote symbol)
"Build a command-line path argument from a list of paths."
  (let ((path (jdee-build-classpath path-list symbol)))
    (if quote
	(setq path (concat "\"" path "\"")))
    (setq path (concat arg " " path))))


(defun jdee-build-classpath-arg (path-list &optional quote symbol)
"Build a classpath from a list of paths."
 (jdee-build-path-arg "-classpath" path-list quote symbol))

(defun jdee-root-dir-p (dir)
  "Return nonnil if DIR is a root directory."
  (let ((parent (expand-file-name  ".." dir)))
    (cond
     ((and
       (fboundp 'ange-ftp-ftp-name)
       (fboundp 'ange-ftp-get-file-entry)
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

(defun jdee-get-global-classpath ()
  "Return the value of `jdee-global-classpath', if defined, otherwise
the value of the CLASSPATH environment variable converted to a list,
of normalized paths, i.e., with . and ~ characters expanded and backslashes
replaces with slashes."
  (if jdee-global-classpath
      jdee-global-classpath
    (let ((cp (getenv "CLASSPATH")))
      (if (stringp cp)
	  (mapcar
	   (lambda (path)
	     (let ((directory-sep-char ?/))
		   (expand-file-name path)))
	   (split-string cp jdee-classpath-separator))))))

(defvar jdee-entering-java-buffer-hook
  '(jdee-reload-project-file
    jdee-which-method-update-on-entering-buffer)
   "*Lists functions to run when entering a jdee-mode source buffer from another
jdee-mode buffer. Note that these functions do not run when reentering the same
jdee-mode buffer from a non-jdee-mode buffer. You should use this hook only for
functions that need to be run when you switch from one jdee-mode buffer to
a different jdee-mode buffer. Use `jdee-mode-hook' if the function needs to run
only once, when the buffer is created.")

(defvar jdee-current-buffer (current-buffer)
  "*Internal JDEE variable that holds the current active buffer.")

(defun jdee-detect-java-buffer-activation ()
  "Detects when a user activates a buffer.
If the activated buffer is a Java buffer, runs the
`jdee-entering-java-buffer-hook' hooks."
  (let ((curr-buff (current-buffer)))
    (unless (equal curr-buff jdee-current-buffer)
      (setq jdee-current-buffer curr-buff)
      (if (eq major-mode 'jdee-mode)
	  (condition-case err
	      (run-hooks 'jdee-entering-java-buffer-hook)
	    (error
	     (message "jdee-entering-java-buffer-hook error: %s"
		      (error-message-string err))))))))

(defun jdee-monitor-post-command-hook ()
  "Checks whether `post-command-hook' includes all hooks required
by JDEE. If not, it adds the required hooks."
  (if (eq major-mode 'jdee-mode)
      (dolist (hook (list 'jdee-detect-java-buffer-activation))
	(when (not (member hook post-command-hook))
	  (add-hook 'post-command-hook hook)))))

(defun jdee-count-open-java-buffers ()
  "Returns non-nil if any java buffers are open."
  (cl-count
   ".java"
   (buffer-list)
   :test
   (lambda (file-type buffer)
     (let ((file-name (buffer-file-name buffer)))
       (if file-name
	   (save-match-data
	    (string-match file-type file-name)))))))


(defun jdee-clean-up-after-jde ()
  "Removes `jdee-detect-java-buffer-activation-hook' when
all Java source buffers have been closed."
  (if  (eq major-mode 'jdee-mode)
      (unless  (> (jdee-count-open-java-buffers) 1)
	(remove-hook 'post-command-hook 'jdee-detect-java-buffer-activation)
	(when jdee-monitor-post-command-hook-timer
	  (cancel-timer jdee-monitor-post-command-hook-timer)
	  (setq jdee-monitor-post-command-hook-timer nil))
	(remove-hook 'kill-buffer-hook 'jdee-clean-up-after-jde))))


;; JDEE help
(defun jdee-find-jdee-doc-directory ()
  "Return the path of the JDEE documentation directory.
Returns  nil if the directory cannot be found. At some
point, XEmacs will include the JDE. Versions of XEmacs
that include JDEE will store the JDEE doc in a data
directory called jde. On all other Emacs versions, the JDEE
expects to find the documentation in a subdirectory
named doc of the directory that contains the file
jde.el."
  (jdee-find-jdee-data-directory))

;;;###autoload
(defun jdee-show-help ()
  "Displays the JDEE User's Guide in a browser."
  (interactive)
  (let* ((jdee-dir (jdee-find-jdee-doc-directory))
	 (jdee-help
	  (if jdee-dir
	      (expand-file-name "doc/html/jdee-ug/jdee-ug.html" jdee-dir))))
    (if (and
	 jdee-help
	 (file-exists-p jdee-help))
	(browse-url (concat "file://" (jdee-convert-cygwin-path jdee-help)))
      (signal 'error '("Cannot find JDEE help file.")))))


;; Problem reporting functions contributed by
;; Phillip Lord <plord < at > hgmp.mrc.ac.uk>.
(defconst jdee-problem-report-mail-address
  (concat "jdee-devel" (char-to-string ?@) "lists.sourceforge.net")
  "Send email to this address for JDEE problem reporting.")

(defun jdee-submit-problem-report()
  "Submit a problem report for the JDEE."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a problem report on the JDEE? ")
   (progn
     (message "Preparing problem report...")
     ;;prepare the basic buffer
     (reporter-submit-bug-report
      jdee-problem-report-mail-address
      (format "JDEE version %s\nRequired packages: cedet-%s\n"
	      jdee-version cedet-version)
      (jdee-problem-report-list-all-variables)
      nil
      'jdee-problem-report-post-hooks
      "Please enter the details of your bug report here" )
     (message "Preparing bug report...done"))))

(defun jdee-problem-report-post-hooks()
  "Function run the reporter package done its work.
It looks for a JDEBug buffer and inserts the contents of that, and then prompts
for insertion of the .emacs file"
  (save-excursion
    (goto-char (point-max))
    (let* ((debug-buffer (get-buffer "*JDEbug*"))
	   (messages-buffer
	    (get-buffer "*Messages*"))
	   (backtrace-buffer (get-buffer "*Backtrace*"))
	   (jdee-log-buffer (get-buffer "*jdee-log*"))
	   (process
	    (let ((proc (jdee-dbs-get-target-process)))
	      (if (not proc)
		  (let ((dead-proc-alist
			 (oref jdee-dbs-the-process-morgue proc-alist)))
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

      ;;insert the contents of the jdee-log buffer if it is there.
      (if jdee-log-buffer
	  (progn
	    (insert "\n\n\nThe contents of the *jdee-log* buffer were\n\n")
	    (insert-buffer-substring jdee-log-buffer)
	    (insert "\n\n\nEnd Insert *jdee-log* buffer" ))
	(insert "\n\n\nThere was no *jdee-log* buffer" )))

    (when process-environment
      (insert "\n\n\nProcess environment: \n\n")
      (insert (mapconcat (lambda (var) var) process-environment "\n")))

    (let* ((init-file-name ".emacs")
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

	    (insert-file-contents "~/.emacs")
	    (goto-char (point-max))
	    (insert
	     (format "\n\n\n=====end inserted %s file"
		     init-file-name)))
	(insert
	 (format "\n\n\nThe user choose not to insert their %s file\n"
		 init-file-name)))

      ;;clean up the prompt buffer
      (kill-buffer buf))))

(defun jdee-problem-report-list-all-variables()
  "List all variables starting with `jde' or `bsh'."
  (let (vars)
    (mapatoms
     (lambda (symbol)
       (if  (jdee-symbol-p symbol)
	 (setq vars (cons symbol vars)))))
    vars))

;; jdee-describe-map is Ehud Karni's describe map with jde prepended.
(defun jdee-keymap-test (var)           ; internal function for keymap checking
       (and (boundp var)
	    (keymapp (symbol-value var))))

(defun jdee-describe-map (map)          ; display map binding
 "Display binding for MAP which must be a quoted keymap variable"
  (interactive
       (let ((map (intern (completing-read "Key map: " obarray 'jdee-keymap-test 1))))
	   (list map)))
       (let ((val (symbol-value map)))
	   (or (keymapp val)
	       (error "%s is not a keymap !" (symbol-name map)))
	   (with-output-to-temp-buffer "*Help*"
	       (princ (format "Binding for keymap %s is:\n" (symbol-name map)))
	       (princ (substitute-command-keys "\\{val}" ))
	       (help-print-return-message))))

(defun jdee-keys ()
  "Displays JDEE key bindings. Use `jdee-bug-keys' to display JDEbug
keybindings."
  (interactive)
  (jdee-describe-map 'jdee-mode-map))


;; Contributed by John Ciolfi, jciolfi@mathworks.com.
(defun jdee-compile-file-if-necessary (file)
  "Compile the JDEE file FILE if necessary.
This is done if FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (if (and (string= (file-name-extension file) "el")
           (not (string= ".dir-locals.el" file)))
      (let* ((root (file-name-sans-extension file))
	     (elc-file (concat root ".elc")))
	(if (and
	     (or (not (file-exists-p elc-file))
                 (file-newer-than-file-p file  elc-file)))
	    (progn
	      (message (format "Byte-compiling %s..."
			       (file-name-nondirectory file)))
	      (byte-compile-file file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Find command                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom jdee-find-case-sensitive nil
  "*Specifies whether the jdee-find command performs a case-sensitive search.
If non-nil, the search is case-sensitive; otherwise, the search ignores case."
  :group 'jdee-project
  :type 'boolean
  )

;; (makunbound 'jdee-find-granularity)
(defcustom jdee-find-granularity '("Character")
  "*Specifies the granularity of the expression search
conducted by `jdee-find': Character (expression starting
on any character), Word (match words only), Line
(match lines only)."
  :group 'jdee-project
  :type  '(list
	   (radio-button-choice
	    :format "%t \n%v"
	    :tag "Search granularity:"
	    (item "Character")
	    (item "Word")
	    (item "Line"))))


(defcustom jdee-find-file-regexp '("*.java")
  "*Specifies the regular expression that the jdee-find command uses
to select files to be searched. You can use any regular expression
supported by the -name option of the GNU find command."
  :group 'jdee-project
  :type '(repeat (string :tag "Find regexp"))
)

(defclass jdee-find-dialog (efc-dialog)
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
   (the-dialog     :type (or null jdee-find-dialog)
		   :allocation :class
		   :initform nil
		   :documentation
		   "The only instance of the find expression dialog buffer."))
  "Dialog displayed by `jdee-find' command.")

(defmethod initialize-instance ((this jdee-find-dialog) &rest fields)
  "Find options dialog constructor."
  (oset this title "Find Dialog")
  (call-next-method))

(defmethod efc-dialog-create ((this jdee-find-dialog))

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
		      (jdee-normalize-path p 'jdee-sourcepath))
		    jdee-sourcepath))
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


(defmethod efc-dialog-show ((this jdee-find-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (call-next-method)
  (recursive-edit))


(defmethod efc-dialog-ok ((this jdee-find-dialog))
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

(defmethod efc-dialog-cancel ((this jdee-find-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this ok nil)
  (exit-recursive-edit))



(defvar jdee-find-root-history nil
  "History of directory trees searched in this session.")

(defvar jdee-find-regexp-history nil
  "History of search expressions used in this session.")

(defun jdee-find-grep-internal (regexp files &optional dirs no-case grain)
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


(defun jdee-find (&optional regexp)
  "Find a regular expression REGEXP in all of the files in the
current JDEE project. Tests each of the following path variables
`jdee-sourcepath', `jdee-compile-option-sourcepath',
`jdee-compile-option-classpath', or `jdee-global-classpath' and uses the
directories specified by the first path variable that has a nonnil
value. The `jdee-find-case-sensitive' variable controls case
sensitivity, `jdee-find-granularity' determines the granularity of the
search (character, word, line), and `jdee-find-file-regexp' determines
the type of files to be searched. Use `jdee-find-dlg' if you want to
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
	    (if (boundp 'jdee-find-regexp-history)
		(car jdee-find-regexp-history)
	      nil)
	    nil nil 'jdee-find-regexp-history)))
	(search-path
	 (read-from-minibuffer
	   "Search directories: "
	   (cons
	    (mapconcat
	     (lambda (x) x)
	     (cond
	      (jdee-sourcepath
	       (mapcar
		(lambda (path)
		  (jdee-normalize-path path 'jdee-sourcepath))
		jdee-sourcepath))
	      (jdee-compile-option-sourcepath
	       (mapcar
		(lambda (path)
		  (jdee-normalize-path path 'jdee-compile-option-sourcepath))
		jdee-compile-option-sourcepath))
	      (jdee-compile-option-classpath
	       (mapcar
		(lambda (path)
		  (jdee-normalize-path path 'jdee-compile-option-classpath))
		jdee-compile-option-classpath))
	      (jdee-global-classpath
	       (mapcar
		(lambda (path)
		  (jdee-normalize-path path 'jdee-global-classpath))
		jdee-global-classpath))
	      (t
	       (list default-directory)))
	     " ")
	    0)
	   nil nil 'jdee-find-root-history)))
    (jdee-find-grep-internal
     regexp
     jdee-find-file-regexp
     search-path
     (not jdee-find-case-sensitive)
     (car jdee-find-granularity))))

(defun jdee-find-dlg ()
  "Displays a dialog buffer that allows you to set all search options
interactively. Pressing the dialog's OK button initiates the
search. Use `jdee-find' if you need to set only the expression to be
found and the directories to be searched and prefer using the
minibuffer."
  (interactive)
  (let ((dialog
	 (progn
	   (if (not (oref-default 'jdee-find-dialog the-dialog))
	       (oset-default 'jdee-find-dialog the-dialog (jdee-find-dialog "find dialog")))
	   (oref-default 'jdee-find-dialog the-dialog))))
    (efc-dialog-show dialog)
    (when (oref dialog ok)
      (jdee-find-grep-internal
       (oref dialog expression)
       (oref dialog files)
       (mapconcat
	'jdee-normalize-path
	(oref dialog dirs)
	" ")
       (oref dialog ignore-case-p)
       (oref dialog grain)))))

(defun jdee-create-prj-values-str ()
  "Create Java expression that updates the JDEE's class list
to include all the classes on `jdee-global-classpath', if
defined, otherwise the classpath specified by the CLASSPATH
environment variable."
  (let* ((directory-sep-char ?/)  ;; Override NT/XEmacs setting
	 (classpath
	      (jdee-build-path-arg nil (jdee-get-global-classpath) t 'jdee-global-classpath)))
    (format "jde.util.JdeUtilities.setProjectValues(\"%s\", %s);"
	    jdee-current-project
	    classpath)))

(defun jdee-show-speedbar ()
  "Show the speedbar after first checking whether the correct
version of speedar is installed."
  (interactive)
  (require 'speedbar)
  (speedbar-frame-mode))

(defun jdee-browse-class-at-point ()
  "Displays the class of the object at point in the BeanShell Class
Browser. Point can be in a variable name, class name, method name, or field name).
This command has the  same requirements to work as the field/method-completion
feature in JDEE (see `jdee-complete-at-point')."
  (interactive)
  (if (jdee-open-functions-exist)
      (let* ((thing-of-interest (thing-at-point 'symbol))
	     (pair (save-excursion (end-of-thing 'symbol)
				   (jdee-parse-java-variable-at-point)))
	     (class-to-open (jdee-open-get-class-to-open
			     pair thing-of-interest)))
	(if (and class-to-open (stringp class-to-open))
	    (progn
	      (bsh-eval
	       (oref-default 'jdee-bsh the-bsh)
	       (concat "exploreClass(\"" class-to-open "\");")))
	  (error "Can not parse the thing at point!")))
    (message "You need JDEE >= 2.2.6 and Senator for using this feature!")))

(defun jdee-assert-mode (&optional no-raise-p)
  "Maybe raise an error if the current buffer isn't a JDEE mode buffer.
NO-RAISE-P, if non-`nil', don't raise an error if this insn't a JDEE mode
buffer, otherwise, return whether or not it is a legitimate buffer."
  (if (and (not no-raise-p) (not (eq major-mode 'jdee-mode)))
      (error "Not visiting a Java source file.")
    (eq major-mode 'jdee-mode)))

(eval-when-compile
  ;; This code will not appear in the compiled (.elc) file
  ;; FIXME: move these to the test directory, with other tests
  (defun jdee-self-test ()
    "Runs jde self tests."
    (interactive)
    (require 'jdee-junit)
    (jdee-junit-self-test)
    (jdee-dbs-self-test)))


;; This must come after all JDEE customization variables have been
;; defined.
(jdee-custom-adjust-groups)

(provide 'jdee)

;;; jdee.el ends here
