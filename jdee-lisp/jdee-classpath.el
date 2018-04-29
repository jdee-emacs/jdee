;;; jdee-classpath.el --- Classpath handling

;; Maintainer: jdee-devel

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
;; This file contains functions for working with classpaths.
;; TODO: This is a temporary solution to group functionalities.

;;; Code:

(require 'jdee-files)

(defvar jdee-classpath-separator
  (if (member system-type '(cygwin32 cygwin))
      ";"
    path-separator)
  "The separator to use in a classpath.
This is usually the same as `path-separator'")

(defcustom jdee-expand-classpath-p t
  "Replace each occurence of a directory named `jdee-lib-directory-names'
 in the classpath with paths to the jar and zip files in that directory."
  :group 'jdee-project
  :type 'boolean)

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

(defun jdee-global-classpath ()
  "Builds a classpath string from the path entries in `jdee-global-classpath'."
  (jdee-build-classpath 'jdee-global-classpath))

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

;;;###autoload
(defun jdee-set-global-classpath (classpath)
  "Set the value of `jdee-global-classpath' to `CLASSPATH'.
It specifies the -classpath argument for the Java compiler and interpreter."
  (interactive "sEnter classpath: ")
  (custom-set-variables
   '(jdee-global-classpath (split-string classpath jdee-classpath-separator) t)))

;; (makunbound 'jdee-sourcepath)
;; FIXME: use compilation-search-path instead?
(defcustom jdee-sourcepath nil
  "List of source directory paths.
The JDEE uses this list to locate source files corresponding to class files.
When entering paths in the custom buffer, enter each path as a separate item
in a separate edit field.  Do NOT put more than one path in the same edit field.
You'll only confuse JDEE.  Paths may contain environment variables or wildcards."
  :group 'jdee-project
  :type '(repeat (file :tag "Path")))

(provide 'jdee-classpath)
;;; jdee-classpath.el ends here
