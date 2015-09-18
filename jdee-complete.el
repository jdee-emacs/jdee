;; jdee-complete.el -- Smart completion for the JDE

;; Author: Rodrigo Reyes <reyes@chez.com>
;; Maintainers: Rodrigo Reyes
;;              Paul Landes
;;              David Ponce
;;              Howard Spector
;;              Stephane Nicolas <s.nicolas@videotron.ca>
;;              Javier Lopez <jlopez@forumsys.com>

;; Keywords: java, intellisense, completion

;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Rodrigo Reyes, Paul Kinnucan,
;;                          Stephane Nicolas, David Ponce, Javier Lopez
;; Copyright (C) 2009 by Paul Landes

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.

;; Java is a registered trademark of Sun Microsystem, Inc.

;;; Commentary:

;; This is one of a set of packages that make up the
;; Java Development Environment for Emacs (JDEE). See the
;; JDEE User's Guide for more information.


;; This package adds smart completion to the JDE. How it works is
;; simple : put the cursor at the end of a statement "under
;; construction", eg. "myVariable.rem<CURSOR HERE> and call the
;; jdee-complete-at-point emacs-lisp function (this is by default
;; C-.). A completion is then inserted. If multiple completions are
;; possible, calling the completion function again will cycle through
;; all the possibilities (as dabbrev-mode does).

;; To retrieve all the possible completions, it uses the java code in
;; jde.util.Completion.getClassInfo(), called by beanshell. That
;; need the class to be compiled (but that's not worst than an etag
;; call).

;; Known bugs/problems :

;; TODO
;; Move all the strings manipulation to the java side so that
;; class info can be loaded in the background.

;; The latest version of the JDEE is available at
;; <URL:http://jdee.sourceforge.net>.

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@attbi.com

;;jdee-eldoc for completion signatures

(require 'eldoc)
(require 'jdee-parse)
(require 'semantic/idle)

;; FIXME: refactor
(declare-function jdee-jeval-r "jdee-bsh" (java-statement))

(defgroup jdee-complete nil
  "JDE Completion"
  :group 'jdee
  :prefix "jdee-complete-")

(defconst jdee-complete-public 0 "Represent a public java modifiers")

(defconst jdee-complete-protected 1 "Represent a protected java modifiers")

(defconst jdee-complete-package 2 "Represent a package java modifiers")

(defconst jdee-complete-private 3 "Represent a private java modifiers")

(defconst jdee-complete-fields 0 "Represents the positions
where the fields are placed.")

(defconst jdee-complete-constructors 1 "Represents the positions
where the constructors are placed.")

(defconst jdee-complete-methods 2 "Represents the positions
where the methods are placed.")

(defconst jdee-complete-classes 3 "Represents the positions
where the classes are placed.")

(defvar jdee-complete-temp-process "*Temp*"
  "Used as the name of a temporary process")

(defvar jdee-complete-last-compiled-class nil
  "Contains the name of the class that was compiled last")

(defvar jdee-complete-current-signature nil
  "Contains the signature of the last method inserted by
either `jdee-complete-in-line' or `jdee-complete-menu'")

(defconst jdee-complete-signature-buffer "*Signature*"
  "Buffer to display signatures")

(defvar jdee-complete-display-signature nil
  "If non nil it displays the `jdee-complete-current-signaure' in
the minibuffer")

(defcustom jdee-complete-signature-display (list "Eldoc")
  "Display modes used to show the method signature after a completion.
The display modes choices are using eldoc-mode(the signature shows in the
minibuffer), a buffer(a one line buffer shows the signature and then
dissapears), or none."
  :group 'jdee-complete
  :type '(list
	  (radio-button-choice
	   (const "Eldoc")
	   (const "Buffer")
	   (const "None"))))

(defcustom jdee-complete-signature-display-time 5
  "Amount of time in seconds to display the method signature
in the minibuffer after a completion."
  :group 'jdee-complete
  :type 'integer)

(defcustom jdee-complete-add-space-after-method nil
  "*If non nil it will add a space between the method
name and the first parenthesis. i.e foo () instead of foo() when using
the completion methods `jdee-complete-in-line' and `jdee-complete-menu'
After customizing this variable, be sure to use
`jdee-complete-flush-classinfo-cache', any class information that was
previously cache is not going to be affected by this setting."
  :group 'jdee-complete
  :type 'boolean)

(defcustom jdee-complete-unique-method-names nil
  "If non nil it will display methods with the same name
 but with different signature only once, the signature for ths methods
 will be the signature of the first method in the list of completions
The list of completion is sorted in alphabetical order.
This variable modifies the completion for `jdee-complete-menu'
and `jdee-complete-in-line'. After customizing this variable,
be sure to use `jdee-complete-flush-classinfo-cache',
any class information that was previously cache is not going to be affected by
this setting"
  :group 'jdee-complete
  :type 'boolean)

(defcustom jdee-complete-insert-method-signature t
  "If non nil it will insert the method signature when using
`jdee-complete-in-line' and `jdee-complete-menu'
i.e. v.get(int, java.lang.String). If `jdee-complete-unique-method-names'
is non nil methods with the same name will get the signature of the first one
in the completion list. After customizing this variable, be sure to use
`jdee-complete-flush-classinfo-cache', any class information that was
previously cache is not going to be affected by this setting"
  :group 'jdee-complete
  :type 'boolean)

(defcustom jdee-complete-display-qualified-types t
  "If non nil use fully qualified types when displaying methods for selection,
i.e. v.get(int, java.lang.String). If nil, use unqualified types, i.e.
v.get(int, String). After customizing this variable, be sure to use
`jdee-complete-flush-classinfo-cache', any class information that was
previously cache is not going to be affected by this setting."
  :group 'jdee-complete
  :type 'boolean)

(defcustom jdee-complete-display-result-type t
  "If non nil include result type when displaying methods for selection.
After customizing this variable, be sure to use
`jdee-complete-flush-classinfo-cache', any class information that was
previously cache is not going to be affected by this setting."
  :group 'jdee-complete
  :type 'boolean)

(defcustom jdee-complete-display-throws t
  "If non nil include thrown exceptions when displaying methods for selection.
After customizing this variable, be sure to use
`jdee-complete-flush-classinfo-cache', any class information that was
previously cache is not going to be affected by this setting."
  :group 'jdee-complete
  :type 'boolean)

;; (makunbound 'jdee-complete-function)
(defcustom jdee-complete-function 'jdee-complete-menu
  "*Function that will be invoked by the `jdee-complete' command.
The `jdee-complete-menu' function displays completions for
the symbol at point in a popup menu. The `jdee-complete-minibuf' function
displays completions in the minibuffer. You may also
specify a custom function to use. The custom function must
be an interactive function that can be called by
`call-interactively'."
  :group 'jdee-project
  :type '(choice
	   (function-item jdee-complete-menu)
	   (function-item jdee-complete-minibuf)
	   (function-item jdee-complete-in-line)
	   (function :format "%t %v" :tag "Custom:")))

(defvar jdee-complete-current-list nil
  "The list of all the completion. Each element of the list is a list
which car is the possible completion, and the cdr is an additional
information about this completion.")

(defvar jdee-complete-current-list-index nil
  "An index to an element in jdee-complete-current-list. This is used to
cycle the list.")

(defun jdee-complete-valid-java-declaration-at (point varname)
  "Verify that a POINT starts a valid java declaration
for the VARNAME variable."
  (save-excursion
    (goto-char point)
    (if (looking-at
	 (concat "\\([A-Za-z0-9_.\177-\377]+\\)[ \t\n\r]+"
		 (jdee-complete-double-backquotes varname)
		 "[ \t\n\r]*[;=]"))
	(match-string 1)
      nil)))

(defun jdee-complete-double-backquotes (varname)
  "Build a new string identical to VARNAME, except that every backquote
`\' is doubled, so that it can be used in a regex expression"
  (let (result (idx 0) (len (length varname)) curcar)
    (while (< idx len)
      (setq curcar (elt varname idx))
      (setq result (concat result (if (eq curcar ?\\)
				      "\\\\"
				    (make-string 1 curcar))))
      (setq idx (1+ idx)))
    result))

(defun jdee-complete-declared-type-of (name)
  "Find in the current buffer the java type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
  (save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (jdee-complete-valid-java-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if resname
	    (progn (setq res resname)
		   (setq found t))))
      res)))

(defun jdee-complete-filter-fqn (importlist)
  "Filter all the fully-qualified classnames in the import list. It uses
the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if importlist
      (if (string= "*" (car (cdr (car importlist))))
	  importlist
	(jdee-complete-filter-fqn (cdr importlist)))))

(defun jdee-complete-guess-type-of (name)
  "Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or a list of possible
packages otherwise."
  (let ((importlist (jdee-parse-import-list)) shortname fullname tmp result)
    (while (and importlist (null result))
      (setq tmp (car importlist))
      (setq shortname (car (cdr tmp)))
      (setq fullname (concat (car tmp) name))
      (cond
       ((string= "*" shortname)
	(setq result importlist))
       ((string= name shortname)
	(setq result fullname))
       (t
	(setq importlist (cdr importlist)))))
    result))


(defvar jdee-complete-classinfo-cache nil)

(defcustom jdee-complete-classinfo-cache-size 50
  "The max size of completion's cache.")

(defun jdee-complete-flush-classinfo-cache ()
  "Flushes all entries in the completion cache"
  (interactive)
  (setq jdee-complete-classinfo-cache nil))

(defun jdee-complete-flush-classes-in-cache (class-list)
  "Flushes all the classes in CLASS-LIST as entries of cache."
  (let ((temp (nth 0 jdee-complete-classinfo-cache))
	(index -1)
	(found nil)
	(class (car class-list)))
    (while class
      (while (and temp (not found))
	(setq index (1+ index))
	(setq temp (nth index jdee-complete-classinfo-cache))
	(if (string= (car temp) class)
	    (setq found t)))
      (if found
	  (setq jdee-complete-classinfo-cache
		(nthcdr (1+ index) jdee-complete-classinfo-cache)))
      (setq class-list (cdr class-list))
      (setq class (car class-list))
      (setq found nil))))

(defun jdee-complete-add-to-classinfo-cache (name classinfo)
  (let (new-entry new-list)
    (if (nth jdee-complete-classinfo-cache-size jdee-complete-classinfo-cache)
	(progn
	  (setq new-entry (list name classinfo))
	  (setq new-list (list new-entry nil))
	  (setcdr new-list (cdr jdee-complete-classinfo-cache))
	  (setq jdee-complete-classinfo-cache new-list)
	  (message "cache is full"))
      ;;else
      (setq jdee-complete-classinfo-cache
	    (append
	     jdee-complete-classinfo-cache
	     (list (list name classinfo)))))))

(defun jdee-complete-get-from-cache (name)
  (let ((temp (nth 0 jdee-complete-classinfo-cache)) (index -1) (found nil))
    (while (and temp (not found))
      (setq index (1+ index))
      (setq temp (nth index jdee-complete-classinfo-cache))
      (if (string= (car temp) name)
	  (setq found t)))
    (if found
	(nth 1 temp)
      nil)))

(defun jdee-complete-get-classinfo (name &optional access-level)
  "Return the class info list for the class NAME and the ACCESS-LEVEL.
Allowed values for access level are 0 for protected 1 for private. This
function first checks to see if the class info is cached. If so, it returns the
cached class info. Otherwise, it creates the class info list. Each
element of the list returned by this function is itself a list whose
car is a possible completion and whose cdr gives additional
informations on the completion."
  ;;replacing double quotes by empty strings double quotes seems to break the
  ;;beanshell
  (while (string-match "\"" name)
    (setq name (replace-match "" nil nil name)))

  ;;replacing back slaches by empty strings backlashes causes beanshell problem
  (while (string-match "\\\\" name)
    (setq name (replace-match "" nil nil name)))

  ;;checking that access-level is not nil
  (if (not access-level)
      (setq access-level jdee-complete-public))

  (let ((class-info (jdee-complete-get-from-cache name))
	public-methods protected-methods private-methods
	package-methods)
    (when (not class-info)
      ;;Getting public class info
      (setq public-methods
	    (jdee-complete-invoke-get-class-info
	     name jdee-complete-public))

      ;;Getting protected class info
      (setq protected-methods
	    (jdee-complete-invoke-get-class-info
	     name jdee-complete-protected))

      ;;Getting package class info
      (setq package-methods
	    (jdee-complete-invoke-get-class-info
	     name jdee-complete-package))

      ;;Getting private class info
      (setq private-methods
	    (jdee-complete-invoke-get-class-info
	     name jdee-complete-private))
      (setq class-info (append public-methods
			       protected-methods
			       package-methods
			       private-methods))
      (if class-info
	  (jdee-complete-add-to-classinfo-cache name class-info)))

    ;;Getting the class info depending on the access level
    (setq class-info
	  (jdee-complete-get-accessible-info class-info access-level name))
    (setq class-info (jdee-complete-build-completion-list class-info))

    ;;Removing duplicates
    (setq class-info (jdee-complete-remove-duplicates class-info))

    ;;Sorting the list
    (setq class-info (sort class-info 'jdee-complete-sort-comparison))
    class-info))

(defun jdee-complete-remove-duplicates (class-list)
  "Removes duplicates from class-list"
  (let (answer temp)
    (while class-list
      (setq temp (car class-list))
       (if (not (jdee-complete-memberp (car temp) answer))
	   (setq answer (append answer (list temp))))
      (setq class-list (cdr class-list)))
    answer))

(defun jdee-complete-memberp (elt lst)
  "Returns t if elt is a memver of lst"
  (let (answer tmp)
    (while lst
      (setq tmp (caar lst))
      (if (string= tmp elt)
	  (progn
	   (setq answer t)
	   (setq lst nil))
	(setq lst (cdr lst))))
    answer))

(defun jdee-complete-get-accessible-info (class-info access name)
  "Takes a list of class info in this format \(list \(list public
info\) \(list protected info\) \(list package info\) \(list private
info\)\).  Each info list is in the format \(list \(list fields\)
\(list constructors\) \(list methods\) \(list inner classes\)\).  This
method will return a list concatenating the fields, methods, and inner
classes for the access level."
  (let* ((public (nth jdee-complete-public class-info))
	(protected (nth jdee-complete-protected class-info))
	(package (nth jdee-complete-package class-info))
	(private (nth jdee-complete-private class-info))
	(package-name (jdee-parse-get-package-name))
	(this (concat (if package-name
			  (concat package-name "." ))
		      (jdee-parse-get-class-at-point)))
	answer fields constructors methods classes packagep)
    (if (null package-name)
	(setq package-name ""))
    (if package-name
	(setq packagep (string-match package-name name)))
    (setq fields (append (nth jdee-complete-fields public)
			 (if (>= access jdee-complete-protected)
			     (nth jdee-complete-fields protected))
			 (if packagep
			     (nth jdee-complete-fields package))
			 (if (or (>= access jdee-complete-private)
				 (string= name this))
			     (nth jdee-complete-fields private))))
    (setq constructors (append (nth jdee-complete-constructors public)
			       (if (>= access jdee-complete-protected)
				   (nth jdee-complete-constructors protected))
			       (if packagep
				   (nth jdee-complete-constructors package))
			       (if (or (>= access jdee-complete-private)
				       (string= name this))
				   (nth jdee-complete-constructors private))))
    (setq methods (append (nth jdee-complete-methods public)
			  (if (>= access jdee-complete-protected)
			      (nth jdee-complete-methods protected))
			  (if packagep
			      (nth jdee-complete-methods package))
			  (if (or (>= access jdee-complete-private)
				  (string= name this))
			      (nth jdee-complete-methods private))))
    (setq classes (append (nth jdee-complete-classes public)
			  (if (>= access jdee-complete-protected)
			     (nth jdee-complete-classes protected))
			  (if packagep
			      (nth jdee-complete-classes package))
			  (if (or (>= access jdee-complete-private)
				  (string= name this))
			      (nth jdee-complete-classes private))))
    (setq answer (list fields constructors methods classes))
    answer))

(defun jdee-complete-invoke-get-class-info (name access)
  "Invoke the method jde.util.Completion.getClassInfo(String, int)"
  (jdee-jeval-r
   (format "jde.util.Completion.getClassInfo(\"%s\",%d);" name access)))


(defun jdee-complete-get-classinfo-javacode (name import access-level)
  "Return the java code that calls the
jde.util.Completion.getClassInfo function with the short java class
name NAME and the package list IMPORT where to look at."
  (save-excursion
    (concat
     "{ "
     "String[] lst = new String[" (number-to-string (length import)) "];\n"
     (let ((count -1))
       (mapconcat
	(function
	 (lambda (x)
	   (setq count (+ 1 count))
	   (concat "lst[" (int-to-string count) "]=\""
		   (car (nth count import)) "\";\n")))
	import
	" "))
     "jde.util.Completion.getClassInfo(\"" name "\",lst,"
     (number-to-string access-level) ");\n"
     "}")))


(defun jdee-complete-sort-comparison (first second)
  (string< (car first) (car second)))

(defun jdee-complete-get-variables (variables)
  "Transform a list of the type (\"var\" \"java.lang.String\")
into (\"var\" \"java.lang.String\ var\")"
  (let (result current prev)
    (if (null jdee-complete-unique-method-names)
	(while variables
	  (setq current (car (car variables)))
	  (setq result
		(append
		 (list (cons (concat current
				     (if jdee-complete-display-result-type
					 (concat
					  " : "
					  (jdee-complete-maybe-unqualify
					   (nth 1 (car variables))))))
			     current))
		 result))
	  (setq variables (cdr variables)))
      (while variables
	(if (not (string= prev current))
	    (progn
	      (setq prev current)
	      (setq result
		    (append
		     (list (cons (concat current
					 (if jdee-complete-display-result-type
					     (concat " : "
						     (jdee-complete-maybe-unqualify
						      (nth 1 (car variables))))))
				 current))
		     result))))
	(setq variables (cdr variables))))
    result))

(defun jdee-complete-build-completion-list (classinfo)
  "Build a completion list from the CLASSINFO list, as returned by the
jde.util.Completion.getClassInfo function."
  (let (result tmp)
    ;; get the variable fields
    (setq tmp (nth jdee-complete-fields classinfo))
    (setq result (jdee-complete-get-variables tmp))

    ;;get the constructors
    (setq tmp (jdee-complete-get-methods
	       (nth jdee-complete-constructors classinfo) t))
    (if tmp (setq result (append tmp result)))

    ;; get the methods
    (setq tmp (jdee-complete-get-methods (nth jdee-complete-methods classinfo)))
    (if tmp (setq result (append tmp result)))

    ;; get inner classes
    (setq tmp (jdee-complete-get-inner-classes
	       (nth jdee-complete-classes classinfo)))
    (if tmp (setq result (append tmp result)))

    result))

(defun jdee-complete-get-methods (classinfo &optional constructor)
  (let ((end-paren (if (null jdee-complete-add-space-after-method) "(" " ("))
	(end-parens (if (null jdee-complete-insert-method-signature)
			(if (null jdee-complete-add-space-after-method)
			    "()"
			  " ()") ""))
	prev tmp current)
    (while classinfo
      (let* ((type (car (cdr (car classinfo))));;method type i.e. boolean
	     (exceptions (jdee-get-exceptions (car (last (car classinfo)))))
	     (method (jdee-complete-build-information-for-completion
		      (car classinfo) end-paren))
	     (display (jdee-complete-build-display-for-completion
		      (car classinfo) end-paren constructor)))
	(setq current (jdee-parse-get-unqualified-name (car (car classinfo))))
	(if (not (and jdee-complete-unique-method-names
		      (string= prev current)))
	    (progn
	      (setq prev current)
	      (setq tmp (append
			 (list
			  (cons
			   display
			   (concat
			    (if (null jdee-complete-insert-method-signature)
				current
			      method)
			    end-parens)))
			 tmp))))
	(setq classinfo (cdr classinfo))))
    tmp))

(defun jdee-complete-get-inner-classes(class-info)
  "Takes as an argument a list of inner classes an return a string of
them or nil"
  (let (tmp fullname pos name)
    (if class-info
	(while class-info
	  (let* ((fullname (caar class-info))
		 (pos (string-match "\\$" fullname))
		 (name (substring fullname (+ 1 pos))))
	    (setq tmp
		  (append
		   (list (cons (concat name " : " fullname)
			       name))
		   tmp)))
	  (setq class-info (cdr class-info))))
    tmp))

(defun jdee-get-exceptions (exceptions)
  "Takes as an argument a list of EXCEPTIONS and return a string of them
or nil"
  (if (and (listp exceptions) (car exceptions))
      (let ((exs ""))
	(while exceptions
	  (setq exs (concat exs (car exceptions)))
	  (setq exceptions (cdr exceptions))
	  (if exceptions (setq exs (concat exs ", "))))
	exs)
    nil))

(defun jdee-complete-maybe-unqualify (type)
  (if jdee-complete-display-qualified-types
      type
    (jdee-parse-get-unqualified-name type)))

(defun jdee-complete-build-display-for-completion (lst
						  end-parens
						  &optional constructor)
  "Builds the string that describes a method in a menu for selecting a completion."
  (let ((result (concat
		 (jdee-parse-get-unqualified-name (car lst))
		 end-parens))
	(rettype (car (cdr lst)))
	(exceptions (if (and (listp (last lst)) (car (last lst)))
			(car (last lst)))))
    (if constructor
	(setq lst (cdr lst))
      (setq lst (cdr (cdr lst))))
    (while (and lst
		(not (listp (car lst))))
      (setq result (concat result (jdee-complete-maybe-unqualify (car lst))))
      (setq lst (cdr lst))
      (if (and lst
	       (not (listp (car lst))))
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    (concat result
	    (if (or (and (not constructor) rettype jdee-complete-display-result-type)
		    (and exceptions jdee-complete-display-throws)) " : ")
	    (if (and (not constructor) rettype jdee-complete-display-result-type)
		(jdee-complete-maybe-unqualify rettype))
	    (if (and exceptions jdee-complete-display-throws)
		(concat " throws "
			(jdee-get-exceptions
			 (mapcar 'jdee-complete-maybe-unqualify exceptions)))))))

(defun jdee-complete-build-information-for-completion (lst
						      end-parens
						      &optional constructor)
  "Builds the text that is inserted in the code for a particular completion."
  (let ((result (concat
		 (jdee-parse-get-unqualified-name (car lst))
		 end-parens)))
    (if constructor
	(setq lst (cdr lst))
      (setq lst (cdr (cdr lst))))
    (while (and lst
		(not (listp (car lst))))
      (setq result (concat result (car lst)))
      (setq lst (cdr lst))
      (if (and lst
	       (not (listp (car lst))))
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    result))

(defun jdee-complete-complete-cycle ()
  "Replace the previous completion by the next one in the list."
 (let (elem tmp)
    (setq jdee-complete-current-list-index (1+ jdee-complete-current-list-index))
    (if (>= jdee-complete-current-list-index (length jdee-complete-current-list))
	(setq jdee-complete-current-list-index 0))
    (setq elem (nth jdee-complete-current-list-index jdee-complete-current-list))
    (setq tmp (cdr elem))
    (if tmp
	(progn
	  (delete-region jdee-parse-current-beginning
			 jdee-parse-current-end)
	  (insert tmp)
	  (setq jdee-complete-current-signature (car elem))
	  (jdee-complete-place-cursor)
	  (set-marker jdee-parse-current-end
		      (+ (marker-position jdee-parse-current-beginning)
			 (length tmp)))
	  (jdee-complete-display-current-signature));;displaying the signature
      (message (format "No completion at this point!(cycle)")))
    ;;  (goto-char (marker-position jdee-complete-current-end))
    ))

(defun jdee-complete-insert-completion (item)
  (if item
      (progn
	(delete-region jdee-parse-current-beginning
		       jdee-parse-current-end)
	(insert item)
	(jdee-complete-place-cursor)
	(jdee-complete-display-current-signature)
	(set-marker jdee-parse-current-end
		    (+ (marker-position jdee-parse-current-beginning)
		       (length item))))))

(defun jdee-complete-find-all-completions (pair lst &optional exact-match)
  (let* (tmp
	 chop-pos
	 (args (nth 2 pair))
	 (pat (nth 1 pair))
	 (result nil)
	 (first-char (substring pat 0 1)))

    (if (null args)
	(setq exact-match nil)
      (setq pat (concat pat args)))

    (if (string= pat "$")
	(setq pat "\\$"))

    (while lst
      (setq tmp (car (car lst)))
      (setq chop-pos (string-match " : " tmp))
      (setq tmp (substring tmp 0 chop-pos))
      (if (if exact-match
	      (string= pat tmp)
	    (equal 0 (string-match pat tmp)))
	  (setq result (append result (list (car lst)))))
      (setq lst (cdr lst)))
    result))


(defun jdee-complete-find-completion-for-pair (pair &optional exact-completion access-level)
  "PAIR is (PREFIX PARTIAL). EXACT-COMPLETION is nil or non-nil.
ACCESS-LEVEL is one of: `jdee-complete-private'
`jdee-complete-protected' nil. Return a list of possible
completions from beanshell."
  (let ((type (jdee-parse-eval-type-of (car pair))))
    (if type
	(cond
	 ((member type jdee-parse-primitive-types)
	  (error "Cannot complete primitive type: %s" type))

	 ((string= type "void")
	  (error "Cannot complete return type of %s is void." (car pair)))

	 (access-level
	  (let ((classinfo (jdee-complete-get-classinfo type access-level)))
	    ;; FIXME: when is classinfo nil?
	    (when classinfo
	      (if (and (string= (nth 1 pair) "")
		       (not exact-completion))
		  (setq jdee-complete-current-list classinfo)
		(setq jdee-complete-current-list
		      (jdee-complete-find-all-completions
		       pair classinfo exact-completion))))))

	 (t
	  (let ((classinfo (jdee-complete-get-classinfo type)))
	    ;; FIXME: when is classinfo nil?
	    (when classinfo
	      (if (and (string= (nth 1 pair) "")
		       (not exact-completion))
		  (setq jdee-complete-current-list classinfo)
		(setq jdee-complete-current-list
		      (jdee-complete-find-all-completions
		       pair classinfo exact-completion)))))))

      ;; type is nil
      nil)))

(defun jdee-complete-in-line ()
  "Completes the method or field name at point.  Repeating the command
cycles through all potential completions for the name.  This function
displays the signature of a method completion as specified by
`jdee-complete-display-current-signature' This command uses the
Beanshell to run Java code that in turn uses Java reflection to
determine the methods and fields defined by the class of the object at
point. This command starts the Beanshell if necessary. Hence, you may
experience a slight delay when using this command for the first time
in a session or when completing a field or method of an object that
has many methods and fields. See `jdee-complete-menu' for a version of
this command that lets you select the desired completion from a popup
menu."
  (interactive)
  (if (and
       jdee-complete-current-list
       (markerp jdee-parse-current-beginning)
       (markerp jdee-parse-current-end)
       (marker-position jdee-parse-current-beginning)
       (marker-position jdee-parse-current-end)
       (>= (point) (marker-position jdee-parse-current-beginning))
       (<= (point) (marker-position jdee-parse-current-end))
       (eq last-command this-command))
      ;; have current completion list
      (jdee-complete-complete-cycle)

    ;; else start over
    (jdee-complete-generic "in-line")))

(defun jdee-complete-choose-completion (&optional title initial-input use-menu)
  "Display completions for the object at point in a menu if USE-MENU
is nonil, otherwise in the minibuffer. The display comprises all of
the possible completions for the object it was invoked on.  To
automatically split large menus this function use `imenu--mouse-menu'
to handle the popup menu. initial-input, whatever the user typed
before invoking the completion"
  (let (index-alist pair name)
    (setq index-alist jdee-complete-current-list)
    (setq pair
	  (if (= (length index-alist) 1)
	      ;; if only one item match, return it
	      (car index-alist)
	    (if use-menu
		(progn
		  ;; delegates menu handling to imenu :-)
		  (require 'imenu)
		  (imenu--mouse-menu
		   index-alist
		   ;; Popup window at text cursor
		   (jdee-cursor-posn-as-event)
		   (or title "Completion")))
	      ;; not menu
	      (assoc (completing-read (or title "Completion: ")
				      index-alist
				      nil ;;predicate
				      nil ;;required-match
				      initial-input) ;;initial-input
				      index-alist))))
    (setq name (cdr pair))
    (setq jdee-complete-current-signature (car pair))
    (jdee-complete-insert-completion name)))

(defun jdee-cursor-posn-as-event()
  "Returns the text cursor position as an EVENT on Emacs and the mouse
cursor position on XEmacs."
  (let ((x (* (frame-char-width)
              (if (and
                   (boundp 'hscroll-mode)
                   (fboundp 'hscroll-window-column))
                  (hscroll-window-column)
                (mod (current-column) (window-width)))))
        (y  (* (frame-char-height)
               (- (count-lines (point-min) (point))
                  (count-lines (point-min) (window-start)))))
        (window (get-buffer-window (current-buffer))))
    (list (list x y) window)))

(defun jdee-complete-menu ()
  "Completes the method or field name at point.  This command displays
a popup menu listing the potential completions for the name at
point. Selecting a completion causes the command to use the completion
to complete the name at point. See `jdee-complete-in-line' for a
version of this command that lets you cycle throught the potential
completions at point."
  (interactive)
  (jdee-complete-generic t))

(defun jdee-complete-minibuf ()
  "Completes the method or field name at point.  This command displays
a popup menu listing the potential completions for the name at
point. Selecting a completion causes the command to use the completion
to complete the name at point. See `jdee-complete-in-line' for a
version of this command that lets you cycle throught the potential
completions at point."
  (interactive)
  (jdee-complete-generic nil))

(defun jdee-complete-generic (completion-type)
  "Generic implementation for jdee-complete methods.
COMPLETION-TYPE is one of:

nil - show completion list in the minibuffer

t   - show completion list in a menu

string -  show completions in-line, cycling thru them."
  (let* ((pair (jdee-parse-java-variable-at-point))
	 jdee-parse-attempted-to-import)

    ;; pair is (prefix  partial-identifier)
    (setq jdee-complete-current-list nil)
    (if pair
	(condition-case err
	    (jdee-complete-pair (jdee-complete-get-pair pair nil) completion-type)
	  (error
	   (condition-case err
	       (jdee-complete-pair (jdee-complete-get-pair pair t) completion-type))
	   (error (message "%s" (error-message-string err)))))

      (message "No completion at this point"))))

(defun jdee-complete-pair (pair completion-type)
  "PAIR is (PREFIX PARTIAL). COMPLETION-TYPE is as for `jdee-complete-generic'."
  (let ((completion-list
	 (jdee-complete-find-completion-for-pair pair nil (jdee-complete-get-access pair))))

    (if (null completion-list)
	;; Check if PREFIX is in the current class
	(setq completion-list
	      (jdee-complete-find-completion-for-pair
	       (list (concat "this." (car pair)) "")
	       nil jdee-complete-private)))
    ;;if completions is still null check if the method is in the
    ;;super class
    (if (null completion-list)
	(setq completion-list (jdee-complete-find-completion-for-pair
			       (list (concat "super." (car pair)) "")
			       nil jdee-complete-protected)))

    (if completion-list
	(let ((title (concat (car pair) "."
			     (car (cdr pair)) "[...]")))
	  (if (null completion-type)
	      (jdee-complete-choose-completion title (car (cdr pair)))
	    (if (string= completion-type "in-line")
		(progn
		  (setq jdee-complete-current-list-index -1)
		  (jdee-complete-complete-cycle))
	      (jdee-complete-choose-completion title (car (cdr pair)) t))))
      (error "No completion at this point"))))

(defun jdee-complete-get-access (pair)
  "PAIR is (PREFIX PARTIAL). If PREFIX is \"this\", return
`jdee-complete-private'. If \"super\", return
`jdee-complete-protected'. Otherwise return nil."
  (let (access)
    (if (string= (car pair) "this")
	(setq access jdee-complete-private)
      (if (string= (car pair) "super")
	  (setq access jdee-complete-protected)))
    access))

(defun jdee-complete-get-pair (pair op)
  "PAIR is (PREFIX  PARTIAL), OP is t or nil.
If PREFIX is not \"\", return PAIR.
Otherwise, if OP is nil, return (\"this\" PARTIAL),
if OP is non-nil, return (PARTIAL PARTIAL)."

  (let ((tmp (list (car pair) (cadr pair))));; copy of PAIR
    (if (and op
	     (string= (car tmp) "" )
	     (not (string= (cadr tmp) "")))
	(setcar tmp (cadr tmp)))

    (if (string= (car tmp) "" )
	;; PREFIX and PARTIAL both nil
	;; FIXME: can we get here?
	(setcar tmp "this"))
    tmp))

(defun jdee-complete ()
  "Displays completions for the Java symbol at point.  This command
delegates the task of displaying completions to the function specified
by `jdee-complete-function'. This allows you to select or specify the
default method for displaying completions."
  (interactive)
  (call-interactively jdee-complete-function))

(define-mode-local-override
   semantic-idle-summary-current-symbol-info jdee-mode ()
   "Collect information on current symbol."
   (or jdee-complete-display-signature
      (semantic-idle-summary-current-symbol-info-default)))

(defun jdee-complete-popup-message (message buffer-or-name)
  "Split up the current window horizontally, the new buffer is exactly
2 lines in height. Message is inserted in the new buffer.  Succesive
calls to this method with the same buffer-or-name will delete the text
inside the buffer and replace it with message. Message should not be
longer than a line."
  (interactive)
  (let* ((popup (get-buffer-window buffer-or-name));;last popup window
	 (new (get-buffer-create buffer-or-name));;new popup window
	 (current (current-buffer));;current buffer
	 (min window-min-height);;current window-min-height
	 (w (selected-window));;selected window
	 (height (window-height w));;window height
	 w2)
    (if popup (delete-window popup));;deleting previous windows
    (set-window-buffer w new);;set buffer of current window to be the new
    (erase-buffer)
    (insert message);;insert message
    (setq window-min-height 2);;set the minimum height
    (setq w2 (split-window w (- height 2)));;split windows
    (set-window-buffer w current);;restore the buffer in the current window
    (if (> (window-height w2) 2);;if the popup window is larger than 2
	(enlarge-window (- (window-height w2) 2)));;resize it to 2
    (setq window-min-height min);; restore window-min-height
    (run-at-time jdee-complete-signature-display-time
		 nil 'delete-window w2)));;set timer to delete popup window

(defun jdee-complete-display-current-signature()
  "Displays the current signature: `jdee-complete-current-signature'. The
display mode will depend on the variable `jdee-complete-signature-display'"
  (interactive)
  (if jdee-complete-current-signature
      (let ((display (car jdee-complete-signature-display)))
	(cond ((string= display "Eldoc") ;;eldoc
	       (setq jdee-complete-display-signature t)
	       (run-at-time jdee-complete-signature-display-time
			    nil `set-variable
			    `jdee-complete-display-signature nil)
	       (eldoc-message jdee-complete-current-signature))
	      ;;use buffer
	      ((string= display "Buffer")
	       (jdee-complete-popup-message jdee-complete-current-signature
					   jdee-complete-signature-buffer))
	      (t));; do nothing
	)))

(defun jdee-complete-place-cursor ()
  "Places the cursor in between the parenthesis after a
completion. This is only done for methods that contain parameters, for
all the other completions the cursor is place at the end."
  (let ((end-paren (string-match ")" jdee-complete-current-signature))
	(start-paren (string-match "(" jdee-complete-current-signature)))
    (if (and end-paren start-paren (not (= start-paren (- end-paren 1))))
	(goto-char (- (point) 1)))))

(provide 'jdee-complete)

;;; jdee-complete.el ends here
