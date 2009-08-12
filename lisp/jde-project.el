;;; jde-project.el -- Integrated Development Environment for Java.
;; $Id$

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

;;; Code:

(require 'eieio)

(defgroup jde-project nil
  "JDE Project Options"
  :group 'jde
  :prefix "jde-project")


(defvar jde-project-menu-definition
  (list "JDEPrj"
	["New"   jde-project-create-project t]
	)
  "Defines the JDE project menu")

(defvar jde-project-keymap (make-sparse-keymap)
  "JDE Project keymap.")

(easy-menu-define
 jde-project-menu jde-project-keymap
 "JDE Project menu" jde-project-menu-definition)


(defcustom jde-project-key-bindings nil
  "Specifies key bindings for JDE's project-related commands.")

(if (and
     (or
      (not jde-xemacsp)
      (featurep 'infodock)))
    (define-key-after (cdr (assq 'menu-bar global-map))
      [jde-project]
      (cons (car jde-project-menu-definition) jde-project-menu) 'mule))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                           ;;
;; Project Class                                                             ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass jde-project ()
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
  "Class of JDE projects.")


(defclass jde-project-create-dialog (efc-dialog)
  ((project    :initarg :project
	      :type 'jde-project
	      :documentation
	      "Project that this dialog creates.")
  (name-field :initarg :name-field
	      :documentation
	      "Field for entering project name.")
   (dir-field :initarg :dir-field
	      :documentation
	      "Field for entering project root directory."))
 "Dialog for entering information required to create a project.")

(defmethod efc-dialog-create ((this jde-project-create-dialog))

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
;; Application Project Class                                                 ;;
;;                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application Project Class

(defclass jde-project-application (jde-project)
  ()
  "Class of JDE application projects")


(defmethod jde-project-create ((this jde-project-application))
    (if (not (file-exists-p proj-dir))
	(if (yes-or-no-p
	      (format "%s does not exist. Should I create it?" proj-dir))
	    (make-directory proj-dir)
	  (error "Cannot create project.")))

    ;; Make source directory
    (setq dir (expand-file-name "src" proj-dir))
    (if (not (file-exists-p dir)) (make-directory dir))

    ;; Make classes directory
    (setq dir (expand-file-name "classes" proj-dir))
    (if (not (file-exists-p dir)) (make-directory dir))
)


(defmethod jde-project-show-creation-dialog ((this jde-project-application))
  "Shows the dialog for creating a Java application project."
  (let ((dialog
	 (jde-project-application-create-dialog
	  "project create dialog"
	  :project this)))
    (efc-dialog-show dialog)))


(defclass jde-project-application-create-dialog (jde-project-create-dialog)
  ()
 "Create a jde-project-app-create-dialog.")

(defmethod efc-dialog-ok ((this jde-project-application-create-dialog))
  "Callback function executed when the user presses the OK button in
the Application Project Creation dialog."
  (let* ((project (oref this :project))
	 (name (widget-value (oref this name-field)))
	 (dir (widget-value (oref this dir-field)))
	 (proj-dir (expand-file-name name dir)))
    (oset project :name name)
    (oset project :dir proj-dir)
    (jde-project-create project)
    (call-next-method)))


(defun jde-project-create-project ()
  "Creates a JDE project."
  (interactive)
  (let ((project (jde-project-application "Application")))
    (jde-project-show-creation-dialog project)))


(provide 'jde-project)

;; End of jde-project.el
