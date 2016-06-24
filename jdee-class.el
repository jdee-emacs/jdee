;;; jdee-class.el --- Class usage commands for the JDEE.

;; Copyright (C) 2003 Andrew Hyatt
;; Copyright (C) 2009 by Paul Landes

;; Author: Andrew Hyatt <andy_jde@thehyatts.net>
;; Maintainers: Andrew Hyatt and Paul Landes
;; Keywords: java, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; ) or from the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a package that contains various utility classes that are
;; useful when dealing with class files.  There are several macros to
;; enable one to write code that looks through all compiled class
;; files, such as `with-all-class-files', which calls some code for
;; every class files in the `jdee-built-class-path'.  There is also
;; `with-all-class-infos-when', which tests each class file with some
;; predicate, and if it passes then makes the class-info available to
;; the code in the body.  `with-all-corresponding-class-infos' will
;; execte for classes that are compiled from a certain Java source file.
;;
;; There are also a few helper utilities here.

;;; Code:

(require 'cl-lib)
(require 'jdee-bytecode)
(require 'jdee-file-util)

;; FIXME: refactor
(declare-function jdee-normalize-path "jdee" (path &optional symbol))
(declare-function jdee-get-jdk-prog "jdee" (progname))

(defcustom jdee-built-class-path nil
  "Similar to `jdee-global-classpath', except this path should only
have those places where compile files live.  This list of paths could
contain both directories and jar files.  Each of these should
correspond to the root of the build tree, in other words the
directories under it should correpond to packages."
  :group 'jdee-project
  :type '(repeat (file :tag "Path")))

(defmacro with-all-class-files (spec &rest body)
  "Call BODY for every class file found in `jdee-built-class-path'.
Pass in the variable that the class filename will be substituted for,
and optionally a value to use as the return value (similar to
`dotimes'), otherwise `nil' will be returned.  Another optional
argument in the SPEC is the package to restrict processing to.

\(fn (VAR [RESULT] [PACKAGE]) BODY...)"

  (let ((class-var-sym (car spec))
	(old-dir-sym (cl-gensym "--with-all-class-files-old-dir"))
	(normalized-path-sym (cl-gensym "--with-all-class-files-npath"))
	(dir-sym (cl-gensym "--with-all-class-files-dir-sym"))
	(dir2-sym (cl-gensym "--with-all-class-files-dir2-sym"))
	(path-sym (cl-gensym "--with-all-class-files-path"))
	(buf-sym (cl-gensym "--with-all-class-files-buf"))
	(rec-descend (cl-gensym "--with-all-class-files-rec-descend"))
	(process-files (cl-gensym "--with-all-class-files-process-files"))
	(process-class (cl-gensym "--with-all-class-files-process-class"))
	(child-path (cl-gensym "--with-all-class-files-child-path"))
	(package (nth 2 spec)))
    `(labels ((,process-class (,class-var-sym)
                              (when (string-match "\.[Cc][Ll][Aa][Ss][Ss]$" ,class-var-sym)
                                ,@body))
              (,process-files (,dir2-sym)
                              (when (file-exists-p ,dir2-sym)
                                (dolist (,class-var-sym (directory-files ,dir2-sym
                                                                         t "[^.]$"))
                                  (,process-class ,class-var-sym))))
              (,rec-descend (,dir2-sym)
                            (if (file-directory-p ,dir2-sym)
                                (dolist (,child-path (directory-files ,dir2-sym
                                                                      t "[^.]$"))
                                  (,rec-descend ,child-path))
                              (,process-class ,dir2-sym))))
       (let ((,old-dir-sym default-directory))
         (unwind-protect
             (save-excursion
               (dolist (,path-sym jdee-built-class-path)
                 (let ((,normalized-path-sym (jdee-normalize-path ,path-sym)))
                   (unless (file-exists-p ,normalized-path-sym)
                     (error (concat "Could not find file or directory "
                                    ,normalized-path-sym)))
                   (if (file-directory-p ,normalized-path-sym)
                       (if ,package
                           (,process-files
                            (concat ,normalized-path-sym "/" (subst-char-in-string ?. ?/ ,package)))
                         (,rec-descend ,normalized-path-sym))
                     ;; we're not a directory, assume we are a jar file
                     (let ((,dir-sym (concat (jdee-temp-directory) "/"
                                             (make-temp-name "jdee-classes-temp"))))
                       (make-directory ,dir-sym)
                       (cd ,dir-sym)
                       (let ((,buf-sym (get-buffer-create "*Jar output*")))
                         (unless (eq (call-process (jdee-get-jdk-prog 'jar) nil
                                                   ,buf-sym nil "-xf"
                                                   (expand-file-name
                                                    ,normalized-path-sym)) 0)
                           (error
                            (concat "Could not unjar file "
                                    (expand-file-name ,normalized-path-sym)
                                    ".  See *Jar output* buffer for details")))
                         (kill-buffer ,buf-sym))
                       (unwind-protect
                           (if ,package
                               (,process-files
                                (concat ,dir-sym "/" (subst-char-in-string ?. ?/ ,package)))
                             (,rec-descend ,dir-sym))
                         (jdee-file-util-remove-all-matching ,dir-sym "[^\\.]$")))))))
           (cd ,old-dir-sym)))
       ;; return val
       ,(nth 1 spec))))

(defmacro with-all-class-infos-when (spec pred &rest body)
  "Call BODY with the parsed class information of each file found in
`jdee-built-class-path' which passes PRED.  PRED is called on the file
name, not the info.  Also, in contrast to `with-all-classes', the BODY
won't get called on the same class twice.  Pass in the variable that
the class info will be substituted for, and optionally a value to use
as the return value (similar to `dotimes').  Otherwise `nil' will be
returned.  The second optional parameter is the optional package
parameter, to restrict processing to a particular package.
Example:(with-all-class-infos-when (info) (lambda (x)
 (some-pred-p x)) (do-stuff info))"

  (let ((parsed-class-sym (cl-gensym "--with-all-class-infos-pclasses"))
	(class-file-sym (cl-gensym "--with-all-class-infos-cfile"))
	(var-sym (car spec)))
    `(let ((,parsed-class-sym '()))
       (with-all-class-files (,class-file-sym ,@(cdr spec))
			     (when (and (not (jdee-class-path-in-classes-p ,class-file-sym ,parsed-class-sym))
					(funcall ,pred ,class-file-sym))
			       (let ((,var-sym (jdee-bytecode ,class-file-sym)))
				 ,@body
				 (add-to-list (quote ,parsed-class-sym)
					      (jdee-bytecode-extract-classname info)))))
       ,(cadr spec))))

(defmacro with-all-corresponding-class-infos (spec &rest body)
  "Do BODY with all the class files that correspond to the given
source file.  SPEC is a list of the variable name to store the class
info, the package name of the source file, the source name of the source file, and the optional return val.
\((with-all-corresponding-class-infos (VAR PACKAGE FILENAME [RESULT]) BODY...)"
  `(with-all-class-infos-when (,(nth 0 spec) ,(nth 3 spec) ,(nth 1 spec))
                              (lambda (class-file)
                                (string-match ,(nth 1 spec)
                                              (replace-regexp-in-string "/" "." (file-name-directory class-file))))
                              (when (equal (jdee-bytecode-extract-sourcefile info) ,(nth 2 spec))
                                ,@body)))

(defun jdee-class-path-in-classes-p (path classes)
  "Returns true if the PATH looks like it represents a class in CLASSES"
  (jdee-class-partial-match-member
   (replace-regexp-in-string "\\.[Cc][Ll][Aa][Ss][Ss]$" ""
			     (replace-regexp-in-string "/\\|\\$" "." path))
   classes))

(defun jdee-class-partial-match-member (str list)
  "Like `member' but works with strings and will return true if any of
the strings in LIST exist at the end of STR"
  (cl-member-if (lambda (item) (string-match (concat (regexp-quote item) "$")
                                        str)) list))

(defun append-to-list (var list &optional accept-nil)
  "Appends everything in LIST to the list in VAR.  Use similar to
add-to-list, but instead of adding one things, adds a bunch.
ACCEPT-NIL determines if 'nil's are to be added.  By default, they
will not be."
  (dolist (item list)
    (when (or accept-nil item)
      (add-to-list var item))))

(defun jdee-class-get-all-classes-used-by-source (package source-file)
  (let ((primitives '("boolean" "int" "void" "float" "double"))
	(classes '()))
    (with-all-corresponding-class-infos (info package source-file classes)
					;;a. super class type
					(add-to-list 'classes (jdee-bytecode-extract-superclass info))
					;;b. super interfaces type
					(append-to-list 'classes (jdee-bytecode-extract-interfaces info))
					;;c. types of declared fields
					;;d. local variable types
					;; (all called types should wrk for this...)
					(append-to-list 'classes (mapcar (lambda(item) (caadr item))
									 (jdee-bytecode-extract-method-calls info)))
					;;e. method return type
					;;f. method parameter type
					(dolist (sig (jdee-bytecode-extract-method-signatures info))
					  (when (and (nth 1 sig) (not (member (nth 1 sig) primitives)))
					    (add-to-list 'classes (nth 1 sig)))
					  (append-to-list 'classes
							  (mapcar (lambda (c) (when (and c (not (member c primitives)) c)))
								  (nth 2 sig))))
					;;g. method exception types
					(dolist (exceptions (mapcar (lambda (method-exceptions) (nth 1 method-exceptions))
								    (jdee-bytecode-extract-thrown-exception-types info)))
					  (append-to-list 'classes exceptions))
					;;h. type of exceptions in 'catch' statements.
					(dolist (exceptions (mapcar (lambda (method-exceptions) (nth 1 method-exceptions))
								    (jdee-bytecode-extract-caught-exception-types info)))
					  (append-to-list 'classes exceptions)))))

(provide 'jdee-class)

;;; jdee-class.el ends here
