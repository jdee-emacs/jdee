;;; jde-project-file.el -- Integrated Development Environment for Java.
;; $Id$

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

(defconst jde-project-file-version "1.0"
  "*The current JDE project file version number.")

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-")

(defcustom jde-project-context-switching-enabled-p t
  "*Enable project context switching.
If non-nil, the JDE reloads a buffer's project file when you switch to the buffer from
another buffer belonging to another project. You can disable this feature if you prefer
to load project files manually. The debugger uses this variable to disable context-switching
temporarily when stepping through code."
  :group 'jde-project
  :type 'boolean)

(defun jde-toggle-project-switching ()
  "Toggles project switching on or off."
  (interactive)
  (setq jde-project-context-switching-enabled-p
	(not jde-project-context-switching-enabled-p)))

(defcustom jde-project-name "default"
"Specifies name of project to which the current buffer belongs."
  :group 'jde-project
  :type 'string)

(defcustom jde-project-file-name "prj.el"
  "*Specify name of JDE project file.
When it loads a Java source file, the JDE looks for a lisp file of
this name (the default is prj.el in the source file hierarchy. If it
finds such a file, it loads the file. You can use this file to set the
classpath, compile options, and other JDE options on a
project-by-project basis."
  :group 'jde-project
  :type 'string)

(defcustom jde-project-hooks nil
  "Specifies a list of functions to be run when a project
becomes active. The JDE runs the project hooks after
the jde-mode hooks."
  :group 'jde-project
  :type '(repeat (function :tag "Function")))

(defvar jde-loaded-project-file-version nil
  "*Temporary var that holds the project file version of the project
being loaded.")


(defun jde-project-file-version (ver)
  (setq jde-loaded-project-file-version ver))

(defvar jde-current-project ""
  "Path of the project file for the current project.")

(defun jde-find-project-file (dir)
  "Finds the next project file upwards in the directory tree
from DIR. Returns nil if it cannot find a project file in DIR
or an ascendant directory."
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (file (find jde-project-file-name
		     (directory-files dir) :test 'string=)))
    (if file
	(expand-file-name file dir)
      (if (not (jde-root-dir-p dir))
	  (jde-find-project-file (expand-file-name ".." dir))))))

(defvar jde-buffer-project-file ""
  "Path of project file associated with the current Java source buffer.")
(make-variable-buffer-local 'jde-buffer-project-file)

(defun jde-find-project-files (dir)
  "Return all the project files in the current directory tree,
starting with the topmost."
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (file (jde-find-project-file dir))
	 current-dir files)
    (while file
      (setq files (append (list file) files))
      (setq current-dir (file-name-directory file))
      (setq
       file
       (if (not (jde-root-dir-p current-dir))
	   (jde-find-project-file
	    (expand-file-name ".." current-dir)))))
    files))

(defvar jde-loading-project nil
  "Used by project loading system.")

(defvar jde-loading-project-file nil
  "Used by project loading system.")

(defun jde-load-project-file ()
  "Load the project file(s) for the Java source file in the current
buffer. Search for all the project file first in the directory
tree containing the current source buffer. If any files are found,
first reset all variables to their startup values. Then load
the project files starting with the topmost in the tree.
If no project files are found, set the JDE variables to their
Emacs startup values."
  (interactive)
  (setq jde-loading-project t)
  (let ((prj-files
	 (jde-find-project-files
	  ;; Need to normalize path to work around bug in the
	  ;; cygwin version of XEmacs.
	  (expand-file-name "." default-directory))))
    (if prj-files
	(progn
	  (jde-set-variables-init-value)
	  (loop for file in prj-files do
	    (setq jde-loading-project-file file)
	    (jde-log-msg "jde-load-project-file: Loading %s" file)
	    ;; reset project file version
	    (setq jde-loaded-project-file-version nil)
	    (load-file file)
	    (setq jde-loading-project-file nil))
	  (run-hooks 'jde-project-hooks))
      (jde-set-variables-init-value t)))
  (setq jde-loading-project nil))


(defun jde-load-all-project-files ()
  (interactive)
  "Loads the project file associated with each Java source buffer."
  (mapc
   (lambda (java-buffer)
     (save-excursion
       (set-buffer java-buffer)
       (message "Loading project file for %s ..."
		(buffer-file-name java-buffer))
       (jde-load-project-file)))
   (jde-get-java-source-buffers)))

;;;###autoload
(defun jde-open-project-file ()
  "Opens the project file for the Java source file in the
current buffer."
  (interactive)
  (let ((prj-file (jde-find-project-file default-directory)))
    (if prj-file
	(find-file prj-file)
      (message "%s" "Project file not found."))))


(defun jde-save-delete (symbol buffer)
  "Delete the call to SYMBOL from project file in BUFFER.
Leave point at the location of the call, or after the last expression."
  (save-excursion
    (set-buffer buffer)
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

(defun jde-symbol-p (symbol)
  "Returns non-nil if SYMBOL is a JDE variable."
  (and (or
	(get symbol 'custom-type)
	(get symbol 'jde-project))
       (or (string-match "^bsh-" (symbol-name symbol))
	   (string-match "^jde-" (symbol-name symbol)))))

(defvar jde-symbol-list nil
  "*A list of jde variables which are processed by `jde-save-project'.")

(defun jde-symbol-list (&optional force-update)
  "Return a list of variables to be processed by `jde-save-project'.
The first time this is called, the list is saved in `jde-symbol-list'.
If nonnil, FORCE-UPDATE forces regeneration of `jde-symbol-list'.
This is useful for updating customization variables defined by
packages loaded after startup of the JDEE."
  (if force-update
      (setq jde-symbol-list nil))
  (unless jde-symbol-list
    (mapatoms
     (lambda (symbol)
       (if (jde-symbol-p symbol)
	   (setq jde-symbol-list (cons symbol jde-symbol-list))))))
  jde-symbol-list)

(defun jde-set-project-name (name)
  (put 'jde-project-name 'customized-value (list name))
  (setq jde-project-name name))

(defun jde-put-project (symbol project value)
  "Stores a new value for SYMBOL in PROJECT, or overwrites any
existing value."
  (let ((proj-alist (get symbol 'jde-project)))
    (if (null proj-alist)
	(put symbol 'jde-project (list (cons project (list value))))
      (if (assoc project proj-alist)
	  (setcdr (assoc project proj-alist) (list value))
	(put symbol 'jde-project (pushnew (cons project (list value)) proj-alist))))))

(defun jde-get-project (symbol project)
  "Gets the value for SYMBOL that is associated with PROJECT, or nil
if none. To test if SYMBOL has any value for PROJECT, use
`jde-project-present-p'."
  (car-safe (cdr-safe (assoc project (get symbol 'jde-project)))))

(defun jde-project-present-p (symbol project)
  "Returns non-nil if SYMBOL has a value for PROJECT."
  (assoc project (get symbol 'jde-project)))

(defun jde-save-open-buffer (project)
  "Creates a new buffer or opens an existing buffer for PROJECT."
  (let ((auto-insert nil)	; turn off auto-insert when
	buffer standard-output)	; creating a new file
    (setq buffer (find-file-noselect project))
    (setq standard-output buffer)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (jde-save-delete 'jde-project-file-version buffer)
      (delete-blank-lines)
      (jde-save-delete 'jde-set-variables buffer)
      (delete-blank-lines)
      (jde-save-delete 'jde-set-project-name buffer)
      (delete-blank-lines))
    (princ "(jde-project-file-version ")
    (prin1 jde-project-file-version)
    (princ ")\n")
    (princ "(jde-set-variables")
    (jde-log-msg "jde-save-open-buffer: Opening buffer for %s" project)
    buffer))

(defun jde-save-close-buffer (project)
  "Saves and closes the buffer associated with PROJECT."
  (let* ((buffer
	  (if jde-xemacsp
	      (get-file-buffer project)
	    (find-buffer-visiting project)))
	 (standard-output buffer))
    (if buffer
      (progn
	(princ ")\n")
	(save-excursion
	  (set-buffer buffer)
	  (save-buffer))
	(jde-log-msg "jde-save-close-buffer: Closing buffer for %s" project)
	(kill-buffer buffer))
      (jde-log-msg "jde-save-close-buffer: Unable to find buffer for %s" project))))

(defun jde-save-variable (symbol projects)
  "Saves all of the values of SYMBOL for each project file mentioned
in PROJECTS."
  (mapc
   (lambda (project)
     (if (and (not (string= (car project) "default"))
	      (member (car project) projects))
	 (let ((buffer
		(if jde-xemacsp
		    (get-file-buffer (car project))
		  (find-buffer-visiting (car project))))
	       standard-output)
	   (if (null buffer)
	       (setq standard-output (setq buffer (jde-save-open-buffer (car project))))
	     (setq standard-output buffer))
	   (jde-log-msg "jde-save-variable: Saving %S in %s" symbol (car project))
	   (princ "\n '(")
	   (princ symbol)
	   (princ " ")
	   (prin1 (custom-quote (car (cdr project))))
	   (princ ")"))))
   (get symbol 'jde-project)))

(defun jde-save-needs-saving-p (symbol projects)
  "Function used internally by the project saving mechanism to
determine whether or not to save a symbol in a project file.  If there
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
       ((and (jde-project-present-p symbol current-proj)
	     (not (null (get symbol 'customized-value))) ;; not decustomized.
	     (not (equal value (jde-get-project symbol current-proj))))
	(jde-log-msg "jde-save-needs-saving-p: changed value for %S in project `%s'"
		     symbol current-proj)
	(jde-put-project symbol current-proj value)
	t)
       ;; CASE: no value for symbol in current project - check all
       ;; parent projects (plus default) to see if value has changed
       ((and (not (jde-project-present-p symbol current-proj))
	     (progn
	       (setq val-to-save value)
	       (setq proj-iter (cdr projects))
	       (while (and proj-iter
			   (not (jde-project-present-p symbol (car proj-iter))))
		 (setq proj-iter (cdr proj-iter)))
	       (if proj-iter
		   (not (equal value
			       (jde-get-project symbol (car proj-iter))))
		 (setq val-to-save (eval (car (get symbol 'customized-value))))
		 (and (not (null (get symbol 'customized-value))) ;; has been customized.
		      (or                               ;; either
		       (null (get symbol 'saved-value)) ;; not saved in .emacs file, or
		       (not (equal val-to-save          ;; different from value in .emacs file
				   (eval (car (get symbol 'saved-value))))))))))
	(jde-log-msg "jde-save-needs-saving-p: override value %S from parent `%s' in project `%s'"
		     symbol (car proj-iter) current-proj)
	(jde-put-project symbol current-proj val-to-save)
	t)
       ;; CASE: current value same as value in the deepest project that
       ;; holds that value - re-save it
       ((progn
	  (setq proj-iter projects)
	  (while (and proj-iter
		      (not (jde-project-present-p symbol (car proj-iter))))
	    (setq proj-iter (cdr proj-iter)))
	  (if proj-iter
	      (equal value (jde-get-project symbol (car proj-iter)))))
	(jde-log-msg "jde-save-needs-saving-p: original value for %S in project `%s'"
		     symbol (car proj-iter))
	t)))))

(defun jde-save-project-internal (projects)
  (let ((projects-reversed (nreverse projects)))
    (jde-log-msg "jde-save-project-internal: projects: %S" projects-reversed)
    (mapc 'jde-save-open-buffer projects-reversed)
    (mapc (lambda (symbol)
	    (if (jde-save-needs-saving-p symbol projects-reversed)
		(jde-save-variable symbol projects-reversed)))
	  (jde-symbol-list))
    (mapc 'jde-save-close-buffer projects-reversed)))

;;;###autoload
(defun jde-save-project ()
  "Saves source file buffer options in one or more project files.
This command provides an easy way to create and update a project file
for a Java project. Simply open a source file, set the desired
options, using the JDE Options menu, then save the settings in the
project file, using this command.  Now, whenever you open a source
file from the same directory tree, the saved settings will be restored
for that file."
  (interactive)
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	(project-file-paths (jde-find-project-files default-directory)))
    (if (not project-file-paths)
	(setq project-file-paths
	      (list (expand-file-name jde-project-file-name
				      (read-file-name "Save in directory: "
						      default-directory
						      default-directory)))))
    (jde-save-project-internal project-file-paths)))

;;;###autoload
(defun jde-create-new-project (new-dir)
  "Creates a new JDE project file in directory NEW-DIR, saving any
current customized variables.  If a project file already exists in the
given directory, the project is simply re-saved.  This functions the
same as `jde-save-project' when no project files can be found for the
current source file.  But, if there already exist projects somewhere
along the path, this command unconditionally creates a project file in
the directory specified, thus allowing the user to create and maintain
hierarchical projects."
  (interactive "DCreate new project in directory: ")
  (let* ((directory-sep-char ?/) ;; Override NT/XEmacs setting
	 (prj-file (expand-file-name jde-project-file-name new-dir))
	 (projects (jde-find-project-files new-dir)))
    (if (not (member prj-file projects))
	;; create empty project file if none found
	(let* ((auto-insert nil)	; disable auto-insert
	       (standard-output (find-file-noselect prj-file))
	       (message-log-max nil))	; disable message log
	  (princ "(jde-project-file-version ")
	  (prin1 jde-project-file-version)
	  (princ ")\n(jde-set-variables)\n")
	  (save-excursion
	    (set-buffer standard-output)
	    (save-buffer))
	  (kill-buffer standard-output)
	  (setq projects (nconc projects (list prj-file)))))
    (jde-save-project-internal projects)))


(defvar jde-dirty-variables nil
  "JDEE customization variables that have project-specific customizations.")

(defun jde-set-variables (&rest args)
  "Initialize JDE customization variables.

Takes a variable number of arguments. Each argument
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

	    (add-to-list 'jde-dirty-variables symbol)

	    (if (or customized
		    jde-loaded-project-file-version)
		(put symbol 'customized-value (list value)))
	    (if jde-loading-project-file
		(progn
		  (jde-log-msg "jde-set-variables: Loading %S from project %s" symbol
			       jde-loading-project-file)
		  (jde-put-project symbol
				   jde-loading-project-file
				   (eval value)))
	      (jde-log-msg "jde-set-variables: Loading %S from unknown project" symbol))
	    (when (default-boundp symbol)
	      ;; Something already set this, overwrite it
	      (funcall set symbol (eval value)))
	    (setq args (cdr args)))))))

(defsubst jde-set-variable-init-value(symbol)
  "Set a variable  to the value it has at Emacs startup."
 (let ((val-to-set (eval (car (or (get symbol 'saved-value)
				   (get symbol 'standard-value)))))
	(set (or (get symbol 'custom-set) 'set-default)))
    (if (or (get symbol 'customized-value)
	    (get symbol 'jde-project))
	(funcall set symbol val-to-set))
    (put symbol 'customized-value nil)
    (put symbol 'jde-project nil)
    (jde-put-project symbol "default" val-to-set)))

(defun jde-set-variables-init-value (&optional msg)
  "Set each JDEE variable that has a project-specific customization
to the value it has at Emacs startup (i.e., before any projects
have been loaded)."
  (interactive)
  (if (or (interactive-p) msg)
      (message "Setting customized JDE variables to startup values..."))
  (if jde-dirty-variables
      (mapcar
       'jde-set-variable-init-value
       jde-dirty-variables)))

;; Code to update JDE customization variables when a user switches
;; from a Java source buffer belonging to one project to a buffer
;; belonging to another.

(defun jde-reload-project-file ()
  "If project context-switching is enabled (see
`jde-project-context-switching-enabled-p') and a debugger
is not running (see `jde-debugger-running-p'), reloads the project file
for a newly activated Java buffer when the new buffer's project
differs from the old buffer's."
  (condition-case err
      (let ((project-file-path (jde-find-project-file default-directory)))
	(if (not project-file-path) (setq project-file-path ""))
	(if (and
	     jde-project-context-switching-enabled-p
	     (not (jde-debugger-running-p))
	     (not (string=
		   (file-truename jde-current-project)
		   (file-truename project-file-path))))
	    (progn
	      (setq jde-current-project project-file-path)
	      (jde-load-project-file)
	      (jde-wiz-set-bsh-project))))
    (error (message
	    "Project file reload error: %s"
	    (error-message-string err)))))

(defun jde-update-autoloaded-symbols ()
  "Regenerate `jde-symbol-list' and reload
the project files for the current project. Insert
this function at the end of autoloaded JDEE packages
to register and  initialize customization variables
defined by the current project's project file."
  (jde-symbol-list t)
  (jde-load-project-file))


(provide 'jde-project-file)

;; Change History
;;
;; $Log: jde-project-file.el,v $
;; Revision 1.7  2004/11/13 14:16:16  jslopez
;; Fixes typo. The description for the jde-project was showing in two lines
;; instead of one.
;;
;; Revision 1.6  2004/07/06 01:47:42  paulk
;; - Move jde-get-java-source-buffers and allied functions to jde-util.el.
;; - Create jde-get-project-source-buffers.
;; - Replace jde-get-current-project-buffers with jde-get-project-source-buffers.
;;
;; Revision 1.5  2004/06/07 03:32:56  paulk
;; Catch errors in jde-reload-project-file to avoid nullifying other entering Java buffer
;; hooks.
;;
;; Revision 1.4  2004/06/04 13:49:19  paulk
;; Fixed the following bugs:
;;  - Variable from default value to nil not saved in project file.
;;  - Variable whose customization has been erased is not removed from project file.
;;
;; Revision 1.3  2004/03/03 03:55:45  paulk
;; Moved project-related stuff to this file from jde.el.
;;
;; Revision 1.2  2004/02/24 05:51:21  paulk
;; Cosmetic change.
;;
;; Revision 1.1  2004/02/09 06:46:07  paulk
;; When switching projects, the JDEE now reinitializes only those variables that have
;; project-specific values, i.e., that have been set in project files. This dramatically
;; decreases project switching time. Thanks to Phillip Lord.
;;
;;
;;
