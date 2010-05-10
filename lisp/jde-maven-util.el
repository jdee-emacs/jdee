;;; jde-maven-utils -- Functionality provided by maven
;; $Id: $

;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords:  utils maven java

;; Copyright (C) 2010 by Paul Landes

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
;; Create dependencies from maven.  You need not specify a POM, but instead
;; uses the maven system to create a list of jar file names.

;;; Bugs/Issues:
;; Currently all dendencies mentioned (i.e. where the pom.xml is stored) must
;; have a .jar file since for some reason maven considers it a problem that the
;; file doesn't exist.  If you need a dependency that doesn't have a .jar
;; (i.e. you want to get them all by transitivity), a work around is to just
;; create a .jar in the dependency dir (again, where the pom.xml) lives.  It
;; doesn't matter what is in the jar but it must be valid.
;; Please email me a fix if you have it to:
;; (concat "landes" (char-to-string ?@) "mailc" (char-to-string ?.) "net")

;;; Code:

(defvar jde-maven-util-classpath-program "pomcp"
  "*Program to use to create a class path from POM information.
This is used by `jde-maven-util-classpath'.")

(defclass jde-maven-dependency ()
  ((groupid :initarg :groupid
	    :type string
	    :documentation "Group ID identifies the project \
\(i.e. `org.apache.pdfbox')."
	    )
   (artifactid :initarg :artifactid
	       :type string
	       :documentation "Artifact ID (i.e. `pdfbox')."
	       )
   (version :initarg :version
	    :type string
	    :documentation "Version of the POM (i.e. `1.0.0')."
	      )
   (scope :initarg :scope
	  :type string
	  :documentation "Scope of jars to include \
\(`compile', `runtime', or `test')."
	    )
   ))

;;;###autoload
(defun jde-maven-util-classpath (dependencies &optional forkp)
  "Create a classpath using a maven dependency.

DEPENDENCIES is a list of `jde-maven-dependency'.
FORKP is non-`nil' to indicate using `jde-maven-util-classpath-program' to an
external program to generate the classpath.  Otherwise we use beanshell to do
it instead."
  (let (dep-args)
    (dolist (dep dependencies)
      (with-slots (groupid artifactid version scope) dep
	(setq dep-args
	      (append dep-args (list groupid artifactid version scope)))))
    (if forkp
	(let ((path (shell-command-to-string
		     (mapconcat 'identity
				(append (list jde-maven-util-classpath-program)
					dep-args)
				" "))))
	  (if (> (length path) 0)
	      (with-temp-buffer
		(insert path)
		(goto-char (point-max))
		;; kill the last newline
		(backward-char 1)
		(delete-char 1)
		(beginning-of-line)
		;; delete any logging
		(if (not (bobp)) (backward-char 1))
		(delete-region (point-min) (point))
		(setq path (buffer-string))))
	  (split-string path path-separator))
      (let* ((bsh-fmt "\
import jde.util.MavenUtils;
String[] args = {%s};
print(new MavenUtils().getDependencyPath(\"(path . (\\\"\", \"\\\"))\", \"\\\" \\\"\", args));")
	     (bsh-args (mapconcat
			'(lambda (arg) (format "\"%s\"" arg))
			dep-args ", "))
	     (bsh (format bsh-fmt bsh-args)))
	(with-temp-buffer
	  (insert (jde-jeval bsh))
	  (goto-char (point-min))
	  (re-search-forward "^(path")
	  (beginning-of-line)
	  (cdr (read (buffer-substring (point) (point-max)))))))))

(provide 'jde-maven-util)

;; End of jde-maven-util.el
