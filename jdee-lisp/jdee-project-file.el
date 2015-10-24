;;; jdee-project-file.el -- Project files management

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2004 Paul Kinnucan.
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

(require 'cl-lib)

;; FIXME: refactor
(declare-function jdee-wiz-set-bsh-project "jdee-wiz" ())
(declare-function jdee-root-dir-p "jdee" (dir))
(declare-function jdee-log-msg "jdee" (msg &rest args))

(defconst jdee-project-file-version "1.0"
  "*The current JDEE project file version number.")

(defgroup jdee-project nil
  "JDEE Project Options"
  :group 'jdee
  :prefix "jdee-")

(defcustom jdee-project-context-switching-enabled-p t
  "*Enable project context switching.
If non-nil, the JDEE reloads a buffer's project file when you switch to the buffer from
another buffer belonging to another project. You can disable this feature if you prefer
to load project files manually. The debugger uses this variable to disable context-switching
temporarily when stepping through code."
  :group 'jdee-project
  :type 'boolean)

(defun jdee-toggle-project-switching ()
  "Toggles project switching on or off."
  (interactive)
  (setq jdee-project-context-switching-enabled-p
	(not jdee-project-context-switching-enabled-p)))

(defcustom jdee-project-name "default"
"Specifies name of project to which the current buffer belongs."
  :group 'jdee-project
  :type 'string)

(defcustom jdee-project-file-name "prj.el"
  "*Specify name of JDEE project file.
When it loads a Java source file, the JDEE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDEE options on a
project-by-project basis."
  :group 'jdee-project
  :type 'string)

(defcustom jdee-project-hooks nil
  "Specifies a list of functions to be run when a project
becomes active. The JDEE runs the project hooks after
the jdee-mode hooks."
  :group 'jdee-project
  :type '(repeat (function :tag "Function")))

(defvar jdee-loaded-project-file-version nil
  "*Temporary var that holds the project file version of the project
being loaded.")


(defun jdee-project-file-version (ver)
  (setq jdee-loaded-project-file-version ver))

(defvar jdee-current-project ""
  "Path of the project file for the current project.")

(defun jdee-find-project-file (dir)
  "Finds the next project file upwards in the directory tree
from DIR. Returns nil if it cannot find a project file in DIR
or an ascendant directory."
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (file (cl-find jdee-project-file-name
			(directory-files dir) :test 'string=)))
    (if file
	(expand-file-name file dir)
      (if (not (jdee-root-dir-p dir))
	  (jdee-find-project-file (expand-file-name ".." dir))))))

(defvar jdee-buffer-project-file ""
  "Path of project file associated with the current Java source buffer.")
(make-variable-buffer-local 'jdee-buffer-project-file)

(defun jdee-find-project-files (dir)
  "Return all the project files in the current directory tree,
starting with the topmost."
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (file (jdee-find-project-file dir))
	 current-dir files)
    (while file
      (setq files (append (list file) files))
      (setq current-dir (file-name-directory file))
      (setq
       file
       (if (not (jdee-root-dir-p current-dir))
	   (jdee-find-project-file
	    (expand-file-name ".." current-dir)))))
    files))

(defvar jdee-loading-project nil
  "Used by project loading system.")

(defvar jdee-loading-project-file nil
  "Used by project loading system.")

(defun jdee-load-project-file ()
  "Load the project file(s) for the Java source file in the current
buffer. Search for all the project file first in the directory
tree containing the current source buffer. If any files are found,
first reset all variables to their startup values. Then load
the project files starting with the topmost in the tree.
If no project files are found, set the JDEE variables to their
Emacs startup values."
  (interactive)
  (setq jdee-loading-project t)
  (let ((prj-files
	 (jdee-find-project-files
	  ;; Need to normalize path to work around bug in the
	  ;; cygwin version of XEmacs.
	  (expand-file-name "." default-directory))))
    (if prj-files
	(progn
	  (jdee-set-variables-init-value)
	  (loop for file in prj-files do
	    (setq jdee-loading-project-file file)
	    (jdee-log-msg "jdee-load-project-file: Loading %s" file)
	    ;; reset project file version
	    (setq jdee-loaded-project-file-version nil)
	    (load-file file)
	    (setq jdee-loading-project-file nil))
	  (run-hooks 'jdee-project-hooks))
      (jdee-set-variables-init-value t)))
  (setq jdee-loading-project nil))


(defun jdee-load-all-project-files ()
  "Load the project file associated with each Java source buffer."
  (interactive)
  (mapc
   (lambda (java-buffer)
     (with-current-buffer java-buffer
       (message "Loading project file for %s ..."
		(buffer-file-name java-buffer))
       (jdee-load-project-file)))
   (jdee-get-java-source-buffers)))

;;;###autoload
(defun jdee-open-project-file ()
  "Open the project file for the Java source file in the current buffer."
  (interactive)
  (let ((prj-file (jdee-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "Project file not found."))))


(defun jdee-save-delete (symbol buffer)
  "Delete the call to SYMBOL from project file in BUFFER.
Leave point at the location of the call, or after the last expression."
  (with-current-buffer buffer
    (goto-char (point-min))
    (catch 'found
      (while t
	(let ((sexp (condition-case nil
			(read (current-buffer))
		      (end-of-file (throw 'found nil)))))
	  (when (and (listp sexp)
		     (eq (car sexp) symbol))
	    (delete-region (save-excursion
			     (backward-sexp)
			     (point))
			   (point))
	    (throw 'found nil)))))
    (unless (bolp)
      (princ "\n"))))

(defun jdee-symbol-p (symbol)
  "Return non-nil if SYMBOL is a JDEE variable."
  (and (or
	(get symbol 'custom-type)
	(get symbol 'jdee-project))
       (or (string-match "^bsh-" (symbol-name symbol))
	   (string-match "^jdee-" (symbol-name symbol)))))

(defvar jdee-symbol-list nil
  "*A list of jde variables which are processed by `jdee-save-project'.")

(defun jdee-symbol-list (&optional force-update)
  "Return a list of variables to be processed by `jdee-save-project'.
The first time this is called, the list is saved in `jdee-symbol-list'.
If nonnil, FORCE-UPDATE forces regeneration of `jdee-symbol-list'.
This is useful for updating customization variables defined by
packages loaded after startup of the JDEE."
  (if force-update
      (setq jdee-symbol-list nil))
  (unless jdee-symbol-list
    (mapatoms
     (lambda (symbol)
       (if (jdee-symbol-p symbol)
	   (setq jdee-symbol-list (cons symbol jdee-symbol-list))))))
  jdee-symbol-list)

(defun jdee-set-project-name (name)
  (put 'jdee-project-name 'customized-value (list name))
  (setq jdee-project-name name))

(defun jdee-put-project (symbol project value)
  "Store a new VALUE for SYMBOL in PROJECT, or overwrites any
existing value."
  (let ((proj-alist (get symbol 'jdee-project)))
    (if (null proj-alist)
	(put symbol 'jdee-project (list (cons project (list value))))
      (if (assoc project proj-alist)
	  (setcdr (assoc project proj-alist) (list value))
	(put symbol 'jdee-project (pushnew (cons project (list value)) proj-alist))))))

(defun jdee-get-project (symbol project)
  "Gets the value for SYMBOL that is associated with PROJECT, or nil if none.
To test if SYMBOL has any value for PROJECT, use
`jdee-project-present-p'."
  (car-safe (cdr-safe (assoc project (get symbol 'jdee-project)))))

(defun jdee-project-present-p (symbol project)
  "Returns non-nil if SYMBOL has a value for PROJECT."
  (assoc project (get symbol 'jdee-project)))

(defun jdee-save-open-buffer (project)
  "Create a new buffer or open an existing buffer for PROJECT."
  (let ((auto-insert nil)	; turn off auto-insert when
	buffer standard-output)	; creating a new file
    (setq buffer (find-file-noselect project))
    (setq standard-output buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (jdee-save-delete 'jdee-project-file-version buffer)
      (delete-blank-lines)
      (jdee-save-delete 'jdee-set-variables buffer)
      (delete-blank-lines)
      (jdee-save-delete 'jdee-set-project-name buffer)
      (delete-blank-lines))
    (princ "(jdee-project-file-version ")
    (prin1 jdee-project-file-version)
    (princ ")\n")
    (princ "(jdee-set-variables")
    (jdee-log-msg "jdee-save-open-buffer: Opening buffer for %s" project)
    buffer))

(defun jdee-save-close-buffer (project)
  "Save and close the buffer associated with PROJECT."
  (let* ((buffer
	  (find-buffer-visiting project))
	 (standard-output buffer))
    (if buffer
        (progn
          (princ ")\n")
          (with-current-buffer buffer
            (save-buffer))
          (jdee-log-msg "jdee-save-close-buffer: Closing buffer for %s" project)
          (kill-buffer buffer))
      (jdee-log-msg "jdee-save-close-buffer: Unable to find buffer for %s" project))))

(defun jdee-save-variable (symbol projects)
  "Save all of the values of SYMBOL for each project file in PROJECTS."
  (mapc
   (lambda (project)
     (if (and (not (string= (car project) "default"))
	      (member (car project) projects))
	 (let ((buffer
		(find-buffer-visiting (car project)))
	       standard-output)
	   (if (null buffer)
	       (setq standard-output (setq buffer (jdee-save-open-buffer (car project))))
	     (setq standard-output buffer))
	   (jdee-log-msg "jdee-save-variable: Saving %S in %s" symbol (car project))
	   (princ "\n '(")
	   (princ symbol)
	   (princ " ")
	   (prin1 (custom-quote (car (cdr project))))
	   (princ ")"))))
   (get symbol 'jdee-project)))

(defun jdee-save-needs-saving-p (symbol projects)
  "Determine whether or not to save a SYMBOL in a project file.  If there
are settings to be saved, this function also resolves which project
should receive the customized values."
  (unless (= (length projects) 0)
    (let ((value (symbol-value symbol))
	  val-to-save
	  current-proj proj-iter)
      (setq current-proj (car projects))
      (cond
      ;; CASE: current value changed from saved value in current
       ;; project
       ((and (jdee-project-present-p symbol current-proj)
	     (not (null (get symbol 'customized-value))) ;; not decustomized.
	     (not (equal value (jdee-get-project symbol current-proj))))
	(jdee-log-msg "jdee-save-needs-saving-p: changed value for %S in project `%s'"
		     symbol current-proj)
	(jdee-put-project symbol current-proj value)
	t)
       ;; CASE: no value for symbol in current project - check all
       ;; parent projects (plus default) to see if value has changed
       ((and (not (jdee-project-present-p symbol current-proj))
	     (progn
	       (setq val-to-save value)
	       (setq proj-iter (cdr projects))
	       (while (and proj-iter
			   (not (jdee-project-present-p symbol (car proj-iter))))
		 (setq proj-iter (cdr proj-iter)))
	       (if proj-iter
		   (not (equal value
			       (jdee-get-project symbol (car proj-iter))))
		 (setq val-to-save (eval (car (get symbol 'customized-value))))
		 (and (not (null (get symbol 'customized-value))) ;; has been customized.
		      (or                               ;; either
		       (null (get symbol 'saved-value)) ;; not saved in .emacs file, or
		       (not (equal val-to-save          ;; different from value in .emacs file
				   (eval (car (get symbol 'saved-value))))))))))
	(jdee-log-msg "jdee-save-needs-saving-p: override value %S from parent `%s' in project `%s'"
		     symbol (car proj-iter) current-proj)
	(jdee-put-project symbol current-proj val-to-save)
	t)
       ;; CASE: current value same as value in the deepest project that
       ;; holds that value - re-save it
       ((progn
	  (setq proj-iter projects)
	  (while (and proj-iter
		      (not (jdee-project-present-p symbol (car proj-iter))))
	    (setq proj-iter (cdr proj-iter)))
	  (if proj-iter
	      (equal value (jdee-get-project symbol (car proj-iter)))))
	(jdee-log-msg "jdee-save-needs-saving-p: original value for %S in project `%s'"
		     symbol (car proj-iter))
	t)))))

(defun jdee-save-project-internal (projects)
  (let ((projects-reversed (nreverse projects)))
    (jdee-log-msg "jdee-save-project-internal: projects: %S" projects-reversed)
    (mapc 'jdee-save-open-buffer projects-reversed)
    (mapc (lambda (symbol)
	    (if (jdee-save-needs-saving-p symbol projects-reversed)
		(jdee-save-variable symbol projects-reversed)))
	  (jdee-symbol-list))
    (mapc 'jdee-save-close-buffer projects-reversed)))

;;;###autoload
(defun jdee-save-project ()
  "Save source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project.  Simply open a source file, set the desired
options, using the JDEE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file."
  (interactive)
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	(project-file-paths (jdee-find-project-files default-directory)))
    (if (not project-file-paths)
	(setq project-file-paths
	      (list (expand-file-name jdee-project-file-name
				      (read-file-name "Save in directory: "
						      default-directory
						      default-directory)))))
    (jdee-save-project-internal project-file-paths)))

;;;###autoload
(defun jdee-create-new-project (new-dir)
  "Create a new JDEE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jdee-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects."
  (interactive "DCreate new project in directory: ")
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (prj-file (expand-file-name jdee-project-file-name new-dir))
	 (projects (jdee-find-project-files new-dir)))
    (if (not (member prj-file projects))
	;; create empty project file if none found
	(let* ((auto-insert nil)	; disable auto-insert
	       (standard-output (find-file-noselect prj-file))
	       (message-log-max nil))	; disable message log
	  (princ "(jdee-project-file-version ")
	  (prin1 jdee-project-file-version)
	  (princ ")\n(jdee-set-variables)\n")
	  (with-current-buffer standard-output
	    (save-buffer))
	  (kill-buffer standard-output)
	  (setq projects (nconc projects (list prj-file)))))
    (jdee-save-project-internal projects)))


(defvar jdee-dirty-variables nil
  "JDEE customization variables that have project-specific customizations.")

(defun jdee-set-variables (&rest args)
  "Initialize JDEE customization variables using ARGS.

Takes a variable number of arguments.  Each argument
should be of the form:

  (SYMBOL VALUE)

The value of SYMBOL is set to VALUE.
This function is used in JDEE project files."
  (while args
    (let ((entry (car args)))
      (if (listp entry)
	  (let* ((symbol (nth 0 entry))
		 (value (nth 1 entry))
		 (customized (nth 2 entry))
		 (set (or (and (local-variable-if-set-p symbol nil) 'set)
			  (get symbol 'custom-set)
			  'set-default)))

	    (add-to-list 'jdee-dirty-variables symbol)

	    (if (or customized
		    jdee-loaded-project-file-version)
		(put symbol 'customized-value (list value)))
	    (if jdee-loading-project-file
		(progn
		  (jdee-log-msg "jdee-set-variables: Loading %S from project %s" symbol
			       jdee-loading-project-file)
		  (jdee-put-project symbol
				   jdee-loading-project-file
				   (eval value)))
	      (jdee-log-msg "jdee-set-variables: Loading %S from unknown project" symbol))
	    (when (default-boundp symbol)
	      ;; Something already set this, overwrite it
	      (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defsubst jdee-set-variable-init-value(symbol)
  "Set a variable  to the value it has at Emacs startup."
 (let ((val-to-set (eval (car (or (get symbol 'saved-value)
				   (get symbol 'standard-value)))))
	(set (or (get symbol 'custom-set) 'set-default)))
    (if (or (get symbol 'customized-value)
	    (get symbol 'jdee-project))
	(funcall set symbol val-to-set))
    (put symbol 'customized-value nil)
    (put symbol 'jdee-project nil)
    (jdee-put-project symbol "default" val-to-set)))

(defun jdee-set-variables-init-value (&optional msg)
  "Set each JDEE variable that has a project-specific customization
to the value it has at Emacs startup (i.e., before any projects
have been loaded)."
  (interactive)
  (if (or (called-interactively-p 'interactive) msg)
      (message "Setting customized JDEE variables to startup values..."))
  (if jdee-dirty-variables
      (mapcar
       'jdee-set-variable-init-value
       jdee-dirty-variables)))

;; Code to update JDEE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(defun jdee-reload-project-file ()
  "If project context-switching is enabled (see
`jdee-project-context-switching-enabled-p') and a debugger
is not running (see `jdee-debugger-running-p'), reloads the project file
for a newly activated Java buffer when the new buffer's project
differs from the old buffer's."
  (condition-case err
      (let ((project-file-path (jdee-find-project-file default-directory)))
	(if (not project-file-path) (setq project-file-path ""))
	(if (and
	     jdee-project-context-switching-enabled-p
	     (not (jdee-debugger-running-p))
	     (not (string=
		   (file-truename jdee-current-project)
		   (file-truename project-file-path))))
	    (progn
	      (setq jdee-current-project project-file-path)
	      (jdee-load-project-file)
	      (jdee-wiz-set-bsh-project))))
    (error (message
	    "Project file reload error: %s"
	    (error-message-string err)))))

(defun jdee-update-autoloaded-symbols ()
  "Regenerate `jdee-symbol-list' and reload
the project files for the current project. Insert
this function at the end of autoloaded JDEE packages
to register and  initialize customization variables
defined by the current project's project file."
  (jdee-symbol-list t)
  (jdee-custom-adjust-groups)
  (jdee-load-project-file))


(provide 'jdee-project-file)

;;; jdee-project-file.el ends here
