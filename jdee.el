;;; jdee.el --- Java Development Environment for Emacs

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes
;; Keywords: java, tools
;; URL: http://github.com/jdee-emacs/jdee
;; Version: 2.4.2
;; Package-Requires: ((emacs "24.3") (flycheck "30") (memoize "1.0.1") (dash "2.13.0"))

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
;; This file bootstraps JDEE.

;;; Code:

(require 'browse-url)
(require 'cc-defs)
(require 'cc-mode)
(require 'cedet)
(require 'cl-lib)
(require 'comint)
(require 'compile)
(require 'cus-edit)
(require 'easymenu)
(require 'executable)
(require 'jdee-log)
(require 'jdee-abbrev)
(require 'jdee-annotations)
(require 'jdee-bug)
(require 'jdee-class)
(require 'jdee-compile)
(require 'jdee-complete)
(require 'jdee-custom)
(require 'jdee-cygwin) ;;TODO require conditionally
(require 'jdee-db)
(require 'jdee-deps)
(require 'jdee-find)
(require 'jdee-gen)
(require 'jdee-help)
(require 'jdee-import)
(require 'jdee-issues)
(require 'jdee-font-lock)
(require 'jdee-java-grammar)
(require 'jdee-jdb)
(require 'jdee-jdk-manager)
(require 'jdee-keys)
(require 'jdee-open-source)
(require 'jdee-project-file)
(require 'jdee-refactor)
(require 'jdee-run)
(require 'jdee-stacktrace)
(require 'jdee-archive)
(require 'jdee-test)
(require 'jdee-util)
(require 'jdee-which-method)
(require 'jdee-wiz)
(require 'semantic)
(require 'thingatpt)

;;;###autoload
(defconst jdee-version "2.4.2"
  "JDEE version number.")

(unless (fboundp 'custom-set-default)
  ;; FIXME: for xemacs?
  (defalias 'custom-set-default 'set-default))

(defgroup jdee nil
  "Java Development Environment for Emacs"
  :group 'tools
  :prefix "jdee-")

(defcustom jdee-launch-beanshell-on-demand-p t
  "If non-nil, the JDEE launches the Beanshell the first time it is needed.
Otherwise, the JDEE launches the Beanshell, if it is not already running,
whenever you open a Java source file."
  :group 'jdee-project
  :type 'boolean)

;;;###autoload
(defun jdee-version ()
  "Get the version of JDEE."
  (interactive)
  (message "JDEE %s" jdee-version))

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
  :safe (lambda (val) (member val '(jdee-make jdee-ant-build jdee-maven-build)))
  :type '(radio
	  (const :tag "Make" jdee-make)
	  (const :tag "Ant" jdee-ant-build)
	  (const :tag "Maven" jdee-maven-build)
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
  "Timer that runs `jdee-monitor-post-command-hook' during idle moments.")

;; The following is the expansion of the above macro.
;; We include the expansion to permit autoload on jdee-mode.
(derived-mode-init-mode-variables 'jdee-mode)
(put 'jdee-mode 'derived-mode-parent 'java-mode)

;;;###autoload
(defun jdee-mode ()
  "Major mode for developing Java applications.
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

	(when (not jdee-launch-beanshell-on-demand-p)
          (jdee-backend-launch))

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

        ;; Setup flycheck mode
        (when (featurep 'flycheck)
          (require 'jdee-flycheck)
          (jdee-flycheck-mode))

	;; The next form must be the last executed
	;; by jdee-mode.
	(derived-mode-run-hooks 'jdee-mode))
    (error
     (message "%s" (error-message-string err)))))

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
	["Run Unit Test"     jdee-test-unittest t]
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
	      ["Start"         jdee-backend-run t]
	      ["Exit"          jdee-backend-exit t]
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
	      ["Copy Fully Qualified Class Name"        jdee-fqn-to-kill-ring t]
              ["Stack Trace Buffer"        jdee-stacktrace-buffer t]
              ["Location of Class"         jdee-archive-which t]
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
	      ["JDEE Users Guide"      jdee-help-show-jdee-doc t]
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

(defun jdee-fqn ()
  "Return the fully qualified class name at point.  If not in a
class, use the buffer name."
  (interactive)
  (let* ((pkg (jdee-db-get-package))
         (class (or (jdee-db-get-class)
                    (caar (semantic-find-tags-by-type "class" (current-buffer)))))
         (rtnval  (if pkg
                      (format "%s%s" pkg class)
                    class)))
    rtnval))

(defun jdee-fqn-to-kill-ring ()
  "Copy the qualified class name of class containing point to the kill ring.
Return the fully qualified name."
  (interactive)
  (let* ((fqn (jdee-fqn)))
    (kill-new fqn)
    (when (called-interactively-p 'any)
      (message "%s added to kill ring" fqn))
    fqn))

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
	    (jdee-backend-explore-class class-to-open)
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
