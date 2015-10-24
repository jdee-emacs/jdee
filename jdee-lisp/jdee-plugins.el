;;; jdee-plugins.el -- Support for JDEE plugins

;; Author: Paul Kinnucan <pkinnucan@attbi.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 2003, 2004 Paul Kinnucan.
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

(require 'cl-lib)
(require 'eieio)
(require 'executable)
(require 'jdee-util);; jdee-root
(require 'semantic/util-modes);; semantic-add-minor-mode

(defcustom jdee-plugins-directory (expand-file-name "plugins" (jdee-root))
  "Location of the JDEE's plugins directory."
  :group 'jdee
  :type 'file)

(defclass jdee-plugin ()
  ((bsh-cp    :initarg :bsh-cp
	      :type list
	      :documentation "Beanshell classpath for this plugin.")
   (menu-spec :initarg :menu-spec
	      :type list
	      :documentation "Specifies menu for this plugin.")
   (plugins   :type list
	      :allocation :class
	      :initform nil
	      :documentation
	     "Installed plugins."))
"Class of plugins.")


(defun jdee-pi-register (plugin)
  "Register PLUGIN, which must be an object of
type `jdee-plugin'."
  (oset-default
   'jdee-plugin
   plugins
   (cons plugin (oref-default 'jdee-plugin plugins))))


(defun jdee-pi-get-plugin-dir (plugin)
  "Returns the path of the directory containing PLUGIN."
  (expand-file-name plugin jdee-plugins-directory))


(defun jdee-pi-load-plugin (plugin)
  "Loads the plugin named PLUGIN. This function assumes that the plugin resides
in a subdirectory of the JDEE's plugins directory named PLUGIN and that this
subdirectory contains a subdirectory name lisp that contains
a file named jdee-PLUGIN.el. This function loads jdee-PLUGIN.el."
  (let* ((plugin-dir (expand-file-name plugin jdee-plugins-directory))
	 (plugin-lisp-dir (expand-file-name "lisp" plugin-dir))
	 (plugin-lisp-package-name (concat "jdee-" plugin))
	 (plugin-lisp-file-name (concat plugin-lisp-package-name ".el"))
	 (plugin-lisp-file
	  (expand-file-name
	   plugin-lisp-file-name
	   plugin-lisp-dir)))
    (if (file-exists-p plugin-lisp-file)
	(progn
	  (add-to-list 'load-path plugin-lisp-dir)
	  (require (intern plugin-lisp-package-name)))
      (error "JDEE plugin Lisp file %s missing" plugin-lisp-file-name))))


(defun jdee-pi-load-plugins ()
  "Loads the plugins in the JDEE's plugins directory."
  (interactive)
  (if (file-exists-p jdee-plugins-directory)
      (let ((plugins
	     (delq
	      nil
	      (mapcar
	       (lambda (file)
		 (let ((file-name (file-name-nondirectory file)))
		   (if (and
			(file-directory-p file)
			(not (string= file-name "."))
			(not (string= file-name ".."))
			(not (string= file-name "CVS"))
			(not (string= file-name "RCS")))
		       file-name)))
	       (directory-files jdee-plugins-directory t)))))
	(loop for plugin in plugins do
	  (jdee-pi-load-plugin plugin)))))

(jdee-pi-load-plugins)

(defun jdee-pi-get-bsh-classpath ()
  "Get the plugin directories and jar files to include in the Beanshell classpath."
  (let ((plugins (oref-default 'jdee-plugin plugins))
	classpath)
    (loop for plugin in plugins do
	  (setq classpath (append classpath (oref plugin bsh-cp))))
    classpath))


(defun jdee-pi-install-plugins ()
  "This command installs any plugin distributables that it
finds in the JDEE's plugins directory. It assumes that
the distributables are in jar or zip format and that the
jar program is on the system path."
  (interactive)

  (assert (executable-find "jar") nil
    "Cannot find the jar program on the system path.")

  (let ((zip-files
	 (directory-files jdee-plugins-directory nil ".*[.]\\(zip\\|jar\\)")))

    (when zip-files
      (let ((buf (get-buffer-create "*plugins*")))
	  (with-current-buffer buf
	    (erase-buffer)
	    (insert "JDEE Plugin Installation Log")
	    (pop-to-buffer buf)
	    (cd jdee-plugins-directory)
	    (loop for zip-file in zip-files do
		  (let ((result
			 (shell-command-to-string
			(concat "jar xvf " zip-file))))
		    (insert "\n\n")
		    (insert (format "Installing %s ..."
				    (file-name-sans-extension zip-file)))
		    (insert "\n\n")
		    (insert result)))
	    (insert "\n\nInstallation complete"))))))


(defun jdee-plugin-make-menu-spec ()
  (if (oref-default 'jdee-plugin plugins)
      (append
       (list "JDEpi")
       (delq
	nil
	(cl-mapcan
	 (lambda (plugin)
	   (oref plugin menu-spec))
	 (oref-default 'jdee-plugin plugins))))))

(defvar jdee-plugin-mode-map
  (let ((km (make-sparse-keymap))
	(menu-spec (jdee-plugin-make-menu-spec)))
    (if menu-spec
	(easy-menu-define jdee-plugin-menu km "JDEE Plugin Minor Mode Menu"
	  menu-spec))
    km)
  "Keymap for JDEE plugin minor mode.")


(define-minor-mode jdee-plugin-minor-mode nil
                   :keymap jdee-plugin-mode-map)

(semantic-add-minor-mode 'jdee-plugin-minor-mode " plugin")


(provide 'jdee-plugins)

;;; jdee-plugins.el ends here
