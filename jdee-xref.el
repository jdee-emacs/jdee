;; jdee-xref.el --- Class cross-reference commands for the JDEE.
;;
;; Copyright (C) 2002, 2003 Andrew Hyatt
;; Copyright (C) 2009 by Paul Landes

;; Author: Andrew Hyatt <andy_jde@thehyatts.net>
;; Maintainers: Andrew Hyatt and Paul Kinnucan
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

;; LCD Archive Entry:
;; jdee-xref|Andrew Hyatt|
;; |Java class cross-referencing commands for the JDEE
;; |$Date$|$Revision$|~/packages/jdee-xref.el

;;; Commentary:

;; This file stores all caller-related functionality, from the
;; functions to find all callers of a Java method, to those that will
;; do more sophisticated things like creating a call-tree
;;
;; Everything works off the database, which is split into a series of
;; files, one file per package.  Each package consists of three tables.
;; The first is a mapping of class-function-args specifiers to a list
;; of all the classes, method & linenumbers that call it, such as:
;;
;; (("org.foo.MyClass" "getName" nil "String") .
;;   (("org.foo.MyOtherClass" "doProcessing" nil nil 42)
;;    ("org.foo.YetAnotherClass" "doMoreProcessing" nil nil 32)))
;;
;; The specifier for a method (the first part of the above is a: fully
;; qualified classname , function name, return type (nil for "void")
;; and non-fully qualified arglist.  We don't want to fully qualify
;; the arglist, so that we don't have to fully qualify it when doing a
;; lookup.  The changes of collisions are minor, I think.
;;
;; The next table is a table of which class has which interfaces.
;; This is used so that we can consider a call to IFoo.doSomething as
;; a call to Foo.doSomething, is Foo implements IFoo.  This
;; functionality is not always wanted, so the functions that show
;; caller information can either use it or not.
;;
;; The third table is a table of which class has which methods and
;; fields.  This is necessary to determine which references to
;; subclasses really are references to a superclass because the fields
;; or methods have not been overriden.
;;
;; The fourth table is a table of classes to superclasses, necessary
;; to figure out when a call to superclass might be a call to a
;; subclass (when the user asks about calls to the subclass).
;;
;; There is also a global table across all packages, which is a
;; map of which classes are subclasses of which other classes.  This
;; makes it possible to investigate a class's subclasses when looking
;; for references.

;;; Code:

(require 'cl-lib)
(require 'etags) ; find-tag-marker-ring
(require 'jdee-class)
(require 'jdee-parse)
(require 'jdee-bytecode)
(require 'tree-widget)

;; FIXME: refactor
(defvar jdee-sourcepath);; jde
(declare-function jdee-normalize-path "jdee" (path &optional symbol))
(declare-function jdee-expand-wildcards-and-normalize "jdee" (path &optional symbol))
(declare-function jdee-get-jdk-prog "jdee" (progname))

(defconst jdee-xref-version "1.5")

(defgroup jdee-xref nil
  "JDEE Class Cross-Reference (Refactoring) Options"
  :group 'jdee
  :prefix "jdee-xref-")


(defcustom jdee-xref-db-base-directory "."
  "The path to store the directory which contains the database of
  which function calls which.  The data directory will be called
  \"xrefdb\" and will reside in the directory pointed to at this
  location "
  :group 'jdee-xref
  :type 'directory)

(defcustom jdee-xref-store-prefixes nil
  "A list of what prefixes to specify what references should be
  tracked in the caller database.  Such as: '(\"org.apache\" \"jde\"),
  to keep track of all references to classes that start with
  \"org.apache\" or \"jde\"."
  :group 'jdee-xref
  :type '(repeat (string :tag "Prefix")))

(defcustom jdee-xref-cache-size 3
  "How much package info to cache in memory at once.  The higher the
  variable is the more memory will be used, but the faster things
  should be."
  :group 'jdee-xref
  :type 'integer)

(defvar jdee-xref-stack nil
  "A list of the callers of a function, to be popped one at a time
  and shown to the user")

(defvar jdee-xref-modified-classes nil
  "A list of modified classes, to be used in updating the caller table
  when a recompile happens")

(defvar jdee-xref-parsed-classes nil
  "A global variable that is used to hold which classes have been parsed")

(defvar jdee-xref-cache nil
  "A cache holding package information that will grow to size
  `jdee-xref-cache-size'")

(defvar jdee-xref-subclasses nil
  "A hashtable containing a list of which classes subclass which other
subclasses.")

(defun jdee-xref-pickle-hash (hash filename)
  "Store HASH in the file FILENAME.  The hash can be retrieved by
calling `jdee-xref-unpickle-hash'."
  (when (file-exists-p filename)
    (delete-file filename))
  (save-excursion
    (let ((buf (find-file-noselect (jdee-normalize-path filename))))
      (set-buffer buf)
      (goto-char (point-min))
      (erase-buffer)
      (insert "(")
      (maphash (lambda (key val)
		 (when val
		   (insert (concat "(" (prin1-to-string key) " . "
				   (prin1-to-string val) ")\n" ))))
	       hash)
      (insert ")")
      (save-buffer)
      (kill-buffer buf))))

(defun jdee-xref-unpickle-hash (hash filename)
  "Populate a hash created by loading the contents of FILENAME to HASH.
FILENAME must be created by `jdee-xref-pickle-hash'"
  (unless (file-exists-p filename)
    (error (concat "Cannot unpickle - file " filename " does not exist.  "
		   "The xref database may need to be recreated.")))
  (dolist (item (with-temp-buffer
	  (insert-file-contents-literally filename)
	  (read (current-buffer))))
    (puthash (car item) (cdr item) hash)))

(defun jdee-xref-get-db-directory ()
  (concat (jdee-normalize-path 'jdee-xref-db-base-directory) "/xrefdb"))

(defun jdee-xref-guess-and-set-prefixes ()
  (let ((prefixes (jdee-xref-guess-prefixes)))
    (when prefixes
      (setq jdee-xref-store-prefixes prefixes))))

(defun jdee-xref-guess-prefixes ()
  "Try to guess what the prefixes are.  Return the prefix list if
  correctly guessed, otherwise return NULL.  This works by looking at
  the sourcepath, and putting all the top-level packages in the list,
  where toplevel is defined as being a package from which all the
  other packages branch out from."

  (cl-labels ((get-prefix
	       (base-path package-path)
	       ;; if the directory contains just one directory (or two,
	       ;; one being CVS), then we can recurse down it to build
	       ;; up a proper prefix before the package tree really
	       ;; branches out
	       (let ((files (cl-remove-if-not
			     (lambda (dir)
			       (and (file-directory-p
				     (concat base-path "/" package-path "/" dir))
				    (not (equal "CVS" dir))))
			     (directory-files
			      (concat base-path "/" package-path)
			      nil "[^.]$"))))
		 (if (eq (length files) 1)
		     (get-prefix base-path (concat package-path "/"
						   (car files)))
		   (subst-char-in-string ?/ ?. package-path)))))
    (when (and (eq major-mode 'jdee-mode) jdee-sourcepath)
      (let ((first-prefix (car (split-string (jdee-parse-get-package-name)
					     "\\."))) (prefixes))
	(dolist (path (cl-remove-if-not
		       (lambda (path)
			 (file-exists-p path))
		       (jdee-expand-wildcards-and-normalize jdee-sourcepath
													   'jdee-sourcepath))
				  prefixes)
	  (when (member first-prefix (directory-files path nil "[^.]$"))
	    (message (concat "path = " path))
	    (add-to-list 'prefixes (get-prefix path first-prefix))))))))

;;;###autoload
(defun jdee-xref-make-xref-db ()
  "Create a database of caller to callee (and the reverse) from the
classes in `jdee-built-class-path' and store the data in the location
specified by `jdee-xref-db-base-directory'"
  (interactive)
  (when (null jdee-xref-db-base-directory)
    (error "The variable `jdee-xref-db-base-directory' must be defined to make a caller database"))
  (when (null jdee-built-class-path)
    (error "The variable `jdee-built-class-path' must be defined to make a caller database"))
  (when (null jdee-xref-store-prefixes)
    (error "The variable `jdee-xref-store-prefixes' must be defined to make a caller database"))
  (unless (file-exists-p (jdee-xref-get-db-directory))
    (make-directory (jdee-xref-get-db-directory) t))
  (jdee-xref-update-xref-db)
  (message "Finished creating xref database")
  (add-hook 'after-save-hook 'jdee-xref-file-saved))

(defun jdee-xref-substring-member (str prefixlist)
  "Like `member' but works with strings and will return true if any of
  the prefixes in PREFIXLIST match STR"
  (cl-member-if
   (lambda (item)
     (string=
      (substring str 0 (min (length item)
			    (length str)))
      item))
   prefixlist))

(defun jdee-xref-get-package-data ()
  (let ((data (make-hash-table :test 'equal :size 10))
	(caller-files (directory-files (jdee-xref-get-db-directory)
				       t "caller$")))
    (dolist (caller-file caller-files)
      (let* ((package (mapconcat (lambda (x) x)
				 (butlast
				  (split-string (car (last
						      (split-string caller-file
								    "/")))
						"-")) "-"))
	     (package-data (jdee-xref-load-package-hashes package)))
	(puthash package package-data data)))
    data))

(defun jdee-xref-update-xref-db (&optional only-classes)
  (let ((package-data (if only-classes
			(jdee-xref-get-package-data)
			(make-hash-table :test 'equal :size 10)))
	(subclasses (make-hash-table :test 'equal :size 500)))
    ;; Remove all occurances of classes to be updated from the package-data's caller-hashes
    (when only-classes
      (maphash (lambda (package single-package-data)
		 (maphash (lambda (callee callers)
			    (puthash callee
				     (cl-remove-if (lambda (item)
						  (member (car item)
							  only-classes))
						callers)
				     (nth 0 single-package-data)))
			  (nth 0 single-package-data)))
	       package-data))
    (with-all-class-infos-when (info)
			       (lambda (class-file)
				 (or (null only-classes)
				     (jdee-class-path-in-classes-p
				      class-file only-classes)))
			       (jdee-xref-add-class-info-to-db info package-data
							      subclasses))
    (setq jdee-xref-parsed-classes nil)
    (jdee-xref-pickle-hash subclasses (jdee-xref-get-subclass-file))
    (setq jdee-xref-subclasses subclasses)
    (maphash (lambda (package data)
	       (jdee-xref-pickle-hash (nth 0 data)
				     (jdee-xref-get-caller-file package))
	       (jdee-xref-pickle-hash (nth 1 data)
				     (jdee-xref-get-interface-file package))
	       (jdee-xref-pickle-hash (nth 2 data)
				     (jdee-xref-get-member-file package))
	       (jdee-xref-pickle-hash (nth 3 data)
				     (jdee-xref-get-superclass-file package)))
	     package-data)
    (setq jdee-xref-cache nil)))

(defun jdee-xref-create-package-hashes (&optional fake)
  "Returns a list of the three hashes that are in a package's data.
The hashes are for the caller-hash, the interface-hash, the
member-hash, and the superclass hash.  FAKE determines if we are just
creating them so that there is something to check against.  In those
circumstance we just create tiny hashes to conserve memory."
  (list (make-hash-table :test 'equal :size (if fake 1 100))
	(make-hash-table :test 'equal :size (if fake 1 20))
	(make-hash-table :test 'equal :size (if fake 1 100))
	(make-hash-table :test 'equal :size (if fake 1 20))))

(defun jdee-xref-load-package-hashes (package)
  (let ((data (jdee-xref-create-package-hashes)))
    (jdee-xref-unpickle-hash (nth 0 data)
			    (jdee-xref-get-caller-file package))
    (jdee-xref-unpickle-hash (nth 1 data)
			    (jdee-xref-get-interface-file package))
    (jdee-xref-unpickle-hash (nth 2 data)
			    (jdee-xref-get-member-file package))
    (jdee-xref-unpickle-hash (nth 3 data)
			    (jdee-xref-get-superclass-file package))
    data))

(defun jdee-xref-append-hash (key value hash)
  "Like `puthash' but appends VALUE to the HASH at KEY"
  (puthash key (append (gethash key hash) (if (listp value)
					    value
					    (list value))) hash))

(defun jdee-xref-add-class-info-to-db (info package-data subclasses)
  (message (concat "Parsing class " (jdee-bytecode-extract-classname info)))
  (add-to-list 'jdee-xref-parsed-classes
	       (jdee-bytecode-extract-classname info))
  (let ((package (jdee-parse-get-package-from-name
		  (jdee-bytecode-extract-classname info))))
    ;; If there is no existing package data
    (unless (gethash package package-data)
      (puthash package
	       ;; package-data's values are (caller-hash
	       ;; interface-hash method-and-field-hash)
	       (jdee-xref-create-package-hashes)
	       package-data))
    (destructuring-bind (caller-hash interface-hash
				     method-and-field-hash superclass-hash)
	(gethash package package-data)
      (puthash (jdee-bytecode-extract-classname info)
	       (jdee-bytecode-extract-interfaces info)
	       interface-hash)
      (puthash (jdee-bytecode-extract-classname info)
	       (append (jdee-bytecode-extract-method-signatures info)
		       (jdee-bytecode-extract-field-signatures info))
	       method-and-field-hash)
      (puthash (jdee-bytecode-extract-classname info)
	       (jdee-bytecode-extract-superclass info)
	       superclass-hash)
      (jdee-xref-append-hash
       (jdee-bytecode-extract-superclass info)
       (jdee-bytecode-extract-classname info) subclasses)
      (dolist (call (nreverse
		     (jdee-bytecode-extract-method-calls info)))
	(let ((calls (car call))
	      (called (cadr call)))
	  (if (or (not jdee-xref-store-prefixes)
		  (and
		   (jdee-xref-substring-member (car calls)
                                               jdee-xref-store-prefixes)
		   (jdee-xref-substring-member (car called)
                                               jdee-xref-store-prefixes)))
	      (let* ((dqcalled (list (car called)
				     (nth 1 called)
				     (when (nth 2 called)
				       (jdee-parse-get-unqualified-name
					(nth 2 called)))
				     ;; We don't want to need to
				     ;; know the constructor args
				     ;; for anonymous classes
				     (unless (jdee-xref-is-class-anonymous (car called))
				       (mapcar 'jdee-parse-get-unqualified-name
					       (nth 3 called)))))
		     (called-package (jdee-parse-get-package-from-name
				      (car dqcalled))))
		;; Create the package data if needed
		(unless (gethash called-package package-data)
		  (puthash called-package (jdee-xref-create-package-hashes)
			   package-data))
		(let* ((called-package-hashes
			(gethash called-package package-data))
		       (called-package-caller-hash
			(car called-package-hashes)))
		  ;; add things to the table - making sure there are no duplicates
		  (puthash dqcalled
			   (if (member calls (gethash
					      dqcalled
					      called-package-caller-hash))
			       (gethash dqcalled called-package-caller-hash)
			     (cons calls
				   (gethash dqcalled
					    called-package-caller-hash)))
			   called-package-caller-hash)))))))))

(defun jdee-xref-class-and-token-to-signature (class token)
  (let ((ttype  (semantic-tag-type token))
	(tclass (semantic-tag-class token))
	(tname  (semantic-tag-name token)))
    (list tclass
	  class
	  (if (equal tname (jdee-parse-get-unqualified-name class))
	      "<init>"
	    tname)
	  (when (eq tclass 'function)
	    (if (or (not ttype) (equal ttype "void"))
		nil
	      (jdee-parse-get-unqualified-name ttype)))
	  (if (eq tclass 'function)
	      (mapcar (lambda (arg)
			(jdee-parse-get-unqualified-name
			 (semantic-tag-type arg)))
		      (semantic-tag-function-arguments token))
	    (list (jdee-parse-get-unqualified-name ttype))))))

(defun jdee-xref-get-current-class ()
  (let ((package-name (jdee-parse-get-package-name)))
    (concat package-name (when  package-name ".") (replace-regexp-in-string "\\." "$" (jdee-parse-get-class-at-point)))))

(defun jdee-xref-get-current-signature ()
  (unless (member
	   (semantic-tag-class (semantic-current-tag))
		'(function variable))
    (error "The cursor must be in a function or class variable to get the callers"))
  (jdee-xref-class-and-token-to-signature
   (jdee-xref-get-current-class)
   (semantic-current-tag)))

;;;###autoload
(defun jdee-xref-first-caller (strict)
  "Put the list of who calls the current function on the stack and
display the first caller.  Subsequent callers are displayed through
`jdee-xref-show-next-caller'.  STRICT should be true if the callers of
interfaces to a function, or calls to a superclass which may result in
a virtual function call to the subclass should not be considered.  In
other words, if STRICT is true, then only calls that are definitely to
the requested function are considered."
  (interactive "P")
  (jdee-xref-load-subclasses-table-if-necessary)
  (setq jdee-xref-stack (jdee-xref-get-callers
			    (jdee-xref-get-current-signature) strict))
  (if jdee-xref-stack
      (progn
	(ring-insert find-tag-marker-ring (point-marker))
	(jdee-xref-next-caller))
    (message "No calls")))

(defun jdee-xref-goto-caller (caller)
  (jdee-find-class-source (car caller))
  (goto-char (point-min))
  (forward-line (1- (nth 4 caller))))

;;;###autoload
(defun jdee-xref-next-caller ()
  "If there are items still on the caller stack, pop the first one off
and show it"
  (interactive)
  (if (not jdee-xref-stack)
      (message "No more calls")
    (unless (listp (car jdee-xref-stack))
      (pop jdee-xref-stack)) ;; skip over called classname
    (jdee-xref-goto-caller (pop jdee-xref-stack))))


(defun jdee-xref-load-subclasses-table-if-necessary ()
  (unless jdee-xref-subclasses
    (setq jdee-xref-subclasses (make-hash-table :test 'equal :size 500))
    (jdee-xref-unpickle-hash jdee-xref-subclasses
			    (jdee-xref-get-subclass-file))
    ;; if subclasses were empty, then it's the first time this is run,
    ;; so do our one-time initializations
    (add-hook 'after-save-hook 'jdee-xref-file-saved)))

(defun jdee-xref-signature-to-string (sig)
  (concat (or (nth 3 sig) "void") " " (cadr sig) "."
	  (if (equal (nth 2 sig) "<init>")
	    (jdee-parse-get-unqualified-name (cadr sig))
	    (nth 2 sig))
	  (when (eq (car sig) 'function)
	    (concat "("
		    (mapconcat (lambda (x) x) (nth 4 sig) ",") ")"))))

(defun jdee-xref-find-package-in-cache (package cache)
  (when cache
    (if (equal (caar cache) package)
      (cdar cache)
      (jdee-xref-find-package-in-cache package (cdr cache)))))

(defun jdee-xref-get-caller-file (package)
  (concat (jdee-xref-get-db-directory) "/" package "-caller"))

(defun jdee-xref-get-interface-file (package)
  (concat (jdee-xref-get-db-directory) "/" package "-interfaces"))

(defun jdee-xref-get-member-file (package)
  (concat (jdee-xref-get-db-directory) "/" package "-members"))

(defun jdee-xref-get-superclass-file (package)
  (concat (jdee-xref-get-db-directory) "/" package "-superclasses"))

(defun jdee-xref-get-subclass-file ()
  (concat (jdee-xref-get-db-directory) "/subclasses"))

(defun jdee-xref-find-or-create-package-in-cache (package)
  (unless jdee-xref-db-base-directory
    (error "The variable `jdee-xref-db-base-directory' must be specified to load the xref db"))
  (if (file-exists-p (jdee-xref-get-caller-file package))
    (or (jdee-xref-find-package-in-cache package jdee-xref-cache)
	;; Or we need to get the new package and put it in the cache
	(let ((data (jdee-xref-load-package-hashes package)))
	(setq jdee-xref-cache (cons (cons package data)
				   (if (> (length jdee-xref-cache)
					  jdee-xref-cache-size)
				       (cdr jdee-xref-cache)
				     jdee-xref-cache)))
	data))
    (jdee-xref-create-package-hashes t)))

(defun jdee-xref-get-caller-hash (package)
  (nth 0 (jdee-xref-find-or-create-package-in-cache package)))

(defun jdee-xref-get-interface-hash (package)
  (nth 1 (jdee-xref-find-or-create-package-in-cache package)))

(defun jdee-xref-get-member-hash (package)
  (nth 2 (jdee-xref-find-or-create-package-in-cache package)))

(defun jdee-xref-get-superclass-hash (package)
  (nth 3 (jdee-xref-find-or-create-package-in-cache package)))

(defun jdee-xref-get-basic-caller (sig)
  (gethash (cdr sig) (jdee-xref-get-caller-hash (jdee-parse-get-package-from-name
						(nth 1 sig)))))

(defun jdee-xref-get-members (class)
  (gethash class (jdee-xref-get-member-hash (jdee-parse-get-package-from-name
					    class))))

(defun jdee-xref-get-superclass (class)
  (gethash class (jdee-xref-get-superclass-hash (jdee-parse-get-package-from-name
						class))))

(defun jdee-xref-is-class-anonymous (class)
  (string-match "\\$[0-9]+$" class))

(defun jdee-xref-is-caller-anonymous-class (caller)
  (jdee-xref-is-class-anonymous (nth 0 caller)))

(defun jdee-xref-is-sig-anonymous-class (sig)
  (jdee-xref-is-class-anonymous (nth 1 sig)))

(defun jdee-xref-get-callers (sig &optional strict)
  (let ((typesig (car sig))
    (classname (cadr sig)))
    (append
     ;; if we're an anonymous class, then we want to see where we are
     ;; created, since it kind of goes along with usage of the function.
     (when (jdee-xref-is-sig-anonymous-class sig)
       (jdee-xref-get-basic-caller (list typesig classname "<init>" nil nil)))

     (jdee-xref-get-basic-caller sig)
     (unless strict
       (apply 'append
	  (mapcar
	   (lambda (classname)
	 (let* ((sig `(,typesig ,classname ,@(cddr sig)))
	    (callers-for-classname (jdee-xref-get-basic-caller sig)))
	   (when callers-for-classname
	     (cons classname callers-for-classname)))) ;; include classname in the usage list
	   (jdee-xref-get-subs classname sig (jdee-xref-get-supers classname nil))))))))


(defun jdee-xref-get-supers (classname collect)
  (mapc (lambda (super)
      (unless (member super collect)
	(setq collect (jdee-xref-get-supers super (cons super collect)))))
    (let* ((package (jdee-parse-get-package-from-name classname))
	   (superclass (jdee-xref-get-superclass classname))
	   (superinterfaces (gethash classname (jdee-xref-get-interface-hash package))))
      (if superclass
	  (cons superclass superinterfaces)
	superinterfaces)))
  collect)


(defun jdee-xref-get-subs (classname sig collect)
  (mapc (lambda (subclass)
	(unless (or (member subclass collect) (member (cddr sig) (jdee-xref-get-members subclass)))
	  (setq collect (jdee-xref-get-subs subclass sig (cons subclass collect)))))
      (gethash classname jdee-xref-subclasses))
  collect)


(defun jdee-xref-notify (widget child &optional event)
  (jdee-xref-goto-caller (widget-get widget :caller)))

(defun jdee-xref-caller-to-sig (caller)
  (list 'function (nth 0 caller) (nth 1 caller) (when (nth 2 caller) (jdee-parse-get-unqualified-name (nth 2 caller)))
	(mapcar 'jdee-parse-get-unqualified-name (nth 3 caller))))

(defun jdee-xref-tree-get-children (sig)
  (when sig
    (mapcar
     (lambda (caller)
       (if (listp caller)
       (let  ((caller-sig (jdee-xref-caller-to-sig caller)))
	 (list
	  'tree-widget
	  :node `(push-button
	      :tag ,(jdee-xref-signature-to-string caller-sig)
	      :format "%[%t%]\n"
	      :sig ,caller-sig
	      :caller ,caller
	      :notify jdee-xref-notify)
	  :dynargs 'jdee-xref-tree-get-children-from-tree
	  :expander 'jdee-xref-tree-get-children-from-tree
	  :sig caller-sig
	  :has-children t))
     (list 'tree-widget :tag caller))) ;; class for next set of usages
       (jdee-xref-get-callers sig))))

(defun jdee-xref-tree-get-children-from-tree (tree)
  (jdee-xref-tree-get-children (widget-get tree :sig)))

;;;###autoload
(defun jdee-xref-display-call-tree (strict)
  "Display an interactive call tree of which function call the current
  function, which can be expanded outward.  STRICT should be true if
  the callers of interfaces to a function, or calls to a superclass
  which may result in a virtual function call to the subclass should
  not be considered.  In other words, if STRICT is true, then only
  calls that are definitely to the requested function are considered. "
  (interactive "P")
  (jdee-xref-load-subclasses-table-if-necessary)
  (let* ((sig (jdee-xref-get-current-signature))
	 (buf (get-buffer-create (concat "JDE call graph for "
					 (jdee-xref-signature-to-string
					  sig)))))
    (switch-to-buffer buf)
    (erase-buffer)
    (widget-create 'tree-widget
		   :tag (jdee-xref-signature-to-string sig)
		   :dynargs 'jdee-xref-tree-get-children-from-tree
		   :expander 'jdee-xref-tree-get-children-from-tree
		   :has-children t
		   :sig sig)
    (use-local-map widget-keymap)
    (widget-setup)))

(defun jdee-xref-get-class-variables (class-token)
  (cl-mapcan (lambda (token)
	       (when (eq (semantic-tag-class token) 'variable)
		 (list token)))
	     (semantic-tag-children-compatibility class-token)))

;;;###autoload
(defun jdee-xref-list-uncalled-functions (strict)
  "Displays a simple list of function that are never called, at least
not directly.  Do not assume that this means this code can never be
reached, since reflection could always call any method.  Use this list
and your best judgement to figure out what are good candidates for
code cleanup.  STRICT should be true if the callers of interfaces to a
function, or calls to a superclass which may result in a virtual
function call to the subclass should not be considered.  In other
words, if STRICT is true, then only calls that are definitely to the
requested function are considered.  This function could take a
while. If it does, you might want to consider increasing
`jdee-xref-cache-size'."
  (interactive "P")
  (jdee-xref-load-subclasses-table-if-necessary)
  (save-excursion
    (cl-flet ((get-unused-string
	       (token)
	       (goto-char (semantic-tag-start token))
	       (unless (jdee-xref-get-callers
			(jdee-xref-class-and-token-to-signature
			 (jdee-xref-get-current-class) token) strict)
		 (list (jdee-xref-signature-to-string
			(jdee-xref-class-and-token-to-signature
			 (jdee-xref-get-current-class) token))))))
      (let ((uncalled-methods
             (cl-mapcan #'get-unused-string
                        (semantic-brute-find-tag-by-class 'function
                                                          (current-buffer)
                                                          t)))
            (unreferenced-variables
             (cl-mapcan #'get-unused-string
                        (cl-mapcan 'jdee-xref-get-class-variables
                                   (semantic-brute-find-tag-by-type "class"
                                                                    (current-buffer)
                                                                    t))))
            (outbuf (get-buffer-create "Unreferenced Methods and Members")))
        (switch-to-buffer outbuf)
        (erase-buffer)
        (insert "The following is a list of methods and members that are\n")
        (insert "uncalled directly by any Java classes that are in the\n")
        (insert "following locations: \n")
        (insert (mapconcat (lambda (x) x) jdee-built-class-path ", "))
        (newline)
        (newline)
        (if uncalled-methods
            (progn
              (insert "Unreferenced methods:\n")
              (insert (mapconcat (lambda (x) x) uncalled-methods "\n")))
          (insert "There are no uncalled methods\n\n"))
        (if unreferenced-variables
            (progn
              (insert "\n\nUnreferenced class variables:\n")
              (insert (mapconcat (lambda (x) x) unreferenced-variables "\n")))
          (insert "\n\nThere are no unreferenced variables\n\n"))
        (read-only-mode 1)
        (not-modified)))))

(defun jdee-xref-remove-classes-from-subclasses-table (classes)
  (maphash (lambda (key value)
	     (puthash key
		      (cl-remove-if (lambda (item)
				   (member item classes)) value)
		      jdee-xref-subclasses))
	   jdee-xref-subclasses))

;;;###autoload
(defun jdee-xref-update (&rest ignored)
  "Update the caller table after a recompile.  This can be called by
the user when they recompile outside of emacs.  It will update the
call list of all files modified in emacs"
  (interactive)
  (message "Updating xref tables")
  (when jdee-xref-modified-classes
    (jdee-xref-remove-classes-from-subclasses-table
     jdee-xref-modified-classes)
    (jdee-xref-update-xref-db jdee-xref-modified-classes)
    (message "Finished updateing xref database"))
  (setq jdee-xref-modified-classes nil))

(defun jdee-xref-file-saved ()
  (when (eq major-mode 'jdee-mode)
    (setq jdee-xref-modified-classes
	  (append jdee-xref-modified-classes
		  (mapcar (lambda (class-token)
			    (concat (jdee-parse-get-package-name)
				    (when (jdee-parse-get-package-name) ".")
				    (semantic-tag-name class-token)))
			  (semantic-brute-find-tag-by-type
			   "class" (current-buffer) t))))))

;;;###autoload
(defun jdee-xref-customize ()
  "Display the customization buffer for the xref package."
  (interactive)
  (customize-group "jdee-xref"))

(global-set-key (kbd "C-c C-v a") 'jdee-xref-first-caller)
(global-set-key (kbd "C-c C-v n") 'jdee-xref-next-caller)


;; Register and initialize the customization variables defined
;; by this package.
(jdee-update-autoloaded-symbols)

(provide 'jdee-xref)

;;; jdee-xref.el ends here
