;;; jdee-project.el -- Project creation dialogs.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2000 Paul Kinnucan.
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
;; This file provides basic project creation wizard.

;;; Code:

(require 'efc)
(require 'eieio)
(require 'jdee-classpath)

(defgroup jdee-project nil
  "JDEE Project Options"
  :group 'jdee
  :prefix "jdee-project-")


(defvar jdee-project-menu-definition
  (list "JDEEPrj"
	["New"   jdee-project-create-project t])
  "Defines the JDEE project menu.")

(defvar jdee-project-keymap (make-sparse-keymap)
  "JDEE Project keymap.")

(easy-menu-define
  jdee-project-menu jdee-project-keymap
  "JDEE Project menu" jdee-project-menu-definition)

(defcustom jdee-project-key-bindings nil
  "Specifies key bindings for JDEE's project-related commands."
  :group 'jdee-project)

(if (featurep 'infodock)
    (define-key-after (cdr (assq 'menu-bar global-map))
      [jdee-project]
      (cons (car jdee-project-menu-definition) jdee-project-menu) 'mule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Project Class                                                             ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-project ()
  ((name     :initarg :name
             :type string
             :documentation
             "Name of project")
   (dir      :initarg :dir
             :type string
             :documentation
             "Path of directory that contains this project.")
   (prj-file :initarg :prj-file
             :type string
             :documentation
             "Project file for this project.")
   (src      :initarg :src
             :type string
             :documentation
             "Path of directory that contains the source for this project"))
  (:allow-nil-initform t)
  "Class of JDEE projects.")


(defclass jdee-project-create-dialog (efc-dialog)
  ((project    :initarg :project
               :documentation
               "Project that this dialog creates.")
   (name-field :initarg :name-field
               :documentation
               "Field for entering project name.")
   (dir-field  :initarg :dir-field
               :documentation
               "Field for entering project root directory."))
  "Dialog for entering information required to create a project.")

(defmethod efc-dialog-create ((this jdee-project-create-dialog))

  (widget-insert "Create Project\n\n")

  (oset this name-field
        (widget-create
         'editable-field
         :format "  %t:  %v\n  %h \n\n"
         :size 40
         :tag "Project Name"
         :doc "Name of project."))

  (oset this dir-field
        (widget-create
         'directory
         :format "  %t:  %v\n  %h \n\n"
         :size 40
         :tag "Project Directory"
         :value default-directory
         :doc "Root directory for project. Use M-tab to complete.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Application Create Dialog Class                                           ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-project-application-create-dialog (jdee-project-create-dialog)
  ()
 "Create a jdee-project-app-create-dialog.")

(defmethod efc-dialog-ok ((this jdee-project-application-create-dialog))
  "Callback function executed when the user presses the OK button in
the Application Project Creation dialog."
  (let* ((project (oref this :project))
         (name (widget-value (oref this name-field)))
         (dir (widget-value (oref this dir-field)))
         (proj-dir (expand-file-name name dir)))
    (oset project :name name)
    (oset project :dir proj-dir)
    (jdee-project-create project)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Application Project Class                                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jdee-project-application (jdee-project)
  ()
  "Class of JDEE application projects")

(defmethod jdee-project-create ((this jdee-project-application))
  (if (not (file-exists-p (oref this dir)))
      (if (yes-or-no-p
           (format "%s does not exist. Should I create it?" (oref this dir)))
          (make-directory (oref this dir))
        (error "Cannot create project")))

  ;; Make source directory
  (let ((dir (expand-file-name "src" (oref this dir))))
    (if (not (file-exists-p dir)) (make-directory dir)))

  ;; Make classes directory
  (let ((dir (expand-file-name "classes" (oref this dir))))
    (when (not (file-exists-p dir))
      (make-directory dir))))

(defmethod jdee-project-show-creation-dialog ((this jdee-project-application))
  "Shows the dialog for creating a Java application project."
  (let ((dialog
         (jdee-project-application-create-dialog
          "project create dialog"
          :project this)))
    (efc-dialog-show dialog)))


;;; utility functions

;;;###autoload
(defun jdee-project-create-project ()
  "Create a JDEE project."
  (interactive)
  (let ((project (jdee-project-application "Application")))
    (jdee-project-show-creation-dialog project)))

;;;###autoload
(defun jdee-describe-path (path-type &optional buf)
  "Print and give file existance for each path.
PATH-TYPE is either `global classpath' for `jdee-global-classpath' or
`source path' for `jdee-sourcepath'."
  (interactive
   (list (completing-read "Path: " '("global classpath" "source path") nil t)))
  (let ((user-home (expand-file-name "~/"))
	path-name path desc)
    (if (equal "source path" path-type)
	(setq path-name "Source Path"
              path jdee-sourcepath)
      (setq path-name "Global Classpath"
            path jdee-global-classpath))
    (with-current-buffer
	(or buf (get-buffer-create (format "*JDEE %s*" path-name)))
      (setq truncate-lines t)
      (erase-buffer)
      (insert (format "%s:
d:      directory
f:      file
blank:  path doesn't exist
--------------------------\n" path-name))
      (dolist (file path)
	(setq desc (cond ((file-directory-p file) "d")
			 ((file-exists-p file) "f")
			 (t " ")))
	(setq file (replace-regexp-in-string "~/" user-home file nil t))
	(insert (format "[%s]  %s\n" desc file)))
      (goto-char (point-min))
      (unless buf (pop-to-buffer (current-buffer))))))

(provide 'jdee-project)

;;; jdee-project.el ends here
