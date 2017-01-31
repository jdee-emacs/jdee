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
(require 'jdee-activator)
(require 'jdee-classpath)
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

(defcustom jdee-flycheck-enable-p t
  "If non-nil, JDEE will start Flycheck in every Java buffer."
  :group 'jdee-project
  :type 'boolean)

;;;###autoload
(defun jdee-version ()
  "Get the version of JDEE."
  (interactive)
  (message "JDEE %s" jdee-version))

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

        (jdee-activator-init)

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
        (when (and (featurep 'flycheck)
                   jdee-flycheck-enable-p)
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
                (jdee-find-get-find-exec)
                (jdee-find-get-grep-exec))]
	      ["Expression..."  jdee-find-dlg
               (and
                (jdee-find-get-find-exec)
                (jdee-find-get-grep-exec))]
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
		    ["Class..."                jdee-import-find-and-import t]
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
	      ["Copy Fully Qualified Class Name"        jdee-parse-fqn-to-kill-ring t]
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
  "The JDEE main menu."
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
  "The JDEE New buffer menu."
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

(defun jdee-show-speedbar ()
  "Show the speedbar after first checking whether the correct
version of speedar is installed."
  (interactive)
  (require 'speedbar)
  (speedbar-frame-mode))

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
