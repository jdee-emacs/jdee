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

(defcustom jdee-plugins-directory (expand-file-name "plugins" (jdee-root))
  "Location of the JDEE's plugins directory."
  :group 'jdee
  :type 'file)

(defclass jdee-plugin ()
  ((bsh-cp    :initarg :bsh-cp
	      :type list
              :initform nil
	      :documentation "Beanshell classpath for this plugin.")
   (menu-spec :initarg :menu-spec
	      :type list
              :initform nil
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
   (cons plugin (cl-mapcan (lambda (p) (if (not (equal (eieio-object-name-string p)
                                                       (eieio-object-name-string plugin)))
                                           (list p)))
                           (oref-default 'jdee-plugin plugins)))))


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
  (if (and jdee-plugins-directory (file-directory-p jdee-plugins-directory))
      (mapc (lambda (f)
              (if (and (not (member (file-name-nondirectory f) '("." ".." ".git" "CVS" "RCS")))
                       (file-directory-p f))
                  (jdee-pi-load-plugin (file-name-nondirectory f))))
            (directory-files jdee-plugins-directory t)))
  ;; Update the menu after the plugins have been loaded
  (let* ((menu (jdee-pi-make-menu-spec))
         (p (assoc (car menu) jdee-menu-definition))
         (new-menu (cond ((null menu) (if p (remove p jdee-menu-definition)))
                         ((null p) (append jdee-menu-definition (list menu)))
                         (t (append (remove p jdee-menu-definition) (list menu))))))
    (if new-menu
        (custom-set-variables `(jdee-menu-definition ',new-menu t))))
  )

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


(defun jdee-pi-make-menu-spec ()
  (if (oref-default 'jdee-plugin plugins)
      (append
       (list "Plug-Ins")
       (delq
	nil
	(cl-mapcan
	 (lambda (plugin)
	   (oref plugin menu-spec))
	 (oref-default 'jdee-plugin plugins))))))

(provide 'jdee-plugins)

;; Only load the plugins after the provide, to avoid potential recursive require issues.
(jdee-pi-load-plugins)

;;; jdee-plugins.el ends here
