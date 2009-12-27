;; jde-open-source.el -- Open class source files
;; $Id$
;;
;; Author: Klaus Berndl
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, open files

;; Copyright (C) 2002, 2003, 2004 Klaus Berndl
;; Copyright (C) 2009 by Paul Landes

;; This package follows the GNU General Public Licence (GPL), see the
;; COPYING file that comes along with GNU Emacs. This is free software,
;; you can redistribute it and/or modify it under the GNU GPL terms.
;;
;; Java is a registered trademark of Sun Microsystem, Inc.
;;
;;; Commentary:

;; This is one of a set of packages that make up the
;; Java Development Environment (JDE) for Emacs. See the
;; JDE User's Guide for more information.

;; This package allows to open the class at point.

(require 'jde-parse)
(require 'jde-util)
(require 'jde-bsh)
(require 'senator)

(defcustom jde-open-class-at-point-find-file-function 'find-file-other-window
  "Define the function for opening the class at point. See
`jde-open-class-at-point'`. Default is `find-file-other-window'. A function
defined here must have the same signature as `find-file' means the first
argument is the filename and the second optional argument is a
wildcard-pattern."
  :group 'jde-project
  :type '(function :tag "Function to open class at point"))

(defvar jde-open-cap-ff-function-temp-override nil
  "Maybe some tools needs to temporally override the value of
`jde-open-class-at-point-find-file-function'. Cause of the auto. resetting
mechanism of JDE for defcustom-variables this is not possible with the
defcustom version. So, if you need to override the value of
`jde-open-class-at-point-find-file-function' from within your elisp code you
can use the variable `jde-open-cap-ff-function-temp-override'.
`jde-open-class-at-point' checks first if this variable is not nil and uses
then this value. Only if this variable is nil it uses the value of
`jde-open-class-at-point'!
This variable is NOT for user customizing, but only for use within elisp!")

(defmacro jde-with-file-contents (file &rest body)
  "If FILE exists and is readable creates a temporary buffer with the contents
of FILE, points to beginning of buffer, evaluates BODY and return the value of
the last form of BODY. If FILE does not exist or is not readable nil is
returned.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and charcater interpretation is done!"
  (let ((exp-filename (make-symbol "exp-filename")))
    `(let ((,exp-filename (expand-file-name ,file)))
       (if (and (file-exists-p ,exp-filename)
		(file-readable-p ,exp-filename))
	   (with-temp-buffer
	     (insert-file-contents ,exp-filename)
	     (goto-char (point-min))
	     ,@body)
	 nil))))

(defun jde-open-get-class-to-open (pair parsed-symbol)
  "Evaluates PARSE-SYMBOL to check if it is a variable name or a class name.
If this fails point is on a method or an attribute of a class in the current
buffer or in a superclass. In this cases we check first if the parsed-symbol
is a possible member of the current class(\"this\") and if this fails it
checks if it is a member of the base class(\"super\")."
 (if (and (stringp (car pair))
	  (> (length (car pair)) 0))
     ;; if we got a pair all should work fine.
     (jde-parse-eval-type-of (car pair))
   (or (condition-case ()
	   (jde-parse-eval-type-of parsed-symbol)
	 (error nil))
       (if (jde-parse-find-completion-for-pair
	    `("this" ,parsed-symbol) nil jde-complete-private)
	   (jde-parse-eval-type-of "this")
	 nil)
       (if (jde-parse-find-completion-for-pair
	    `("super" ,parsed-symbol) nil jde-complete-private)
	   (jde-parse-eval-type-of "super")
	 nil))))


(defun jde-open-functions-exist ()
  "Checks if the functions `jde-parse-java-variable-at-point',
`jde-parse-eval-type-of', and `jde-parse-find-completion-for-pair' are defined"
  (and (fboundp 'jde-parse-java-variable-at-point)
       (fboundp 'jde-parse-eval-type-of)
       (fboundp 'jde-parse-find-completion-for-pair)))


(defun jde-open-jump-to-class (parsed-symbol class-name) 
  "Place the cursor in the parsed variable"
  (let* (tags super-class (first-time t))
    ;; Searching only for the symbol '{' is not good enough. We can
    ;; have, for example, '{@link }' in the javadoc before the class
    ;; definition.
    (search-forward-regexp "^[^\\*]*?{" nil t)
    (setq tags (jde-get-parents))
    (setq super-class (car tags))
    (message "Superclass of %s is %s" class-name super-class)
    ;; Now let´s jump to the thing-of-interest. If this is a
    ;; variable-name then we will not find this with senator in
    ;; the opened java-file so we search for the definiton of
    ;; the class itself. This feature is only available if we
    ;; have senator!
    (when (and (fboundp 'senator-search-forward) (not (string= parsed-symbol "")))
      (goto-char (point-min))
      (semantic-fetch-tags)
      (senator-parse)
      (setq parsed-symbol (concat "\\b" parsed-symbol "\\b"))
      (while (not (senator-re-search-forward parsed-symbol nil t))
	(message "Could not find %s in %s" parsed-symbol (buffer-name))
        ;; searching for the thing-of-interest has failed 
        ;; let's try in the base class
        (progn
          (if (not super-class)
              (error "Method not found"))
          (let ((jde-open-cap-ff-function-temp-override 'find-file))
            (jde-show-superclass-source-2 tags))
          (goto-char (point-min))
          (senator-parse)
          (search-forward-regexp "^[^\\*]*?{" nil t)
          (setq tags (jde-get-parents))
          ;;if it is the first time try in the class definition
          ;;itself.
          (if first-time
              (progn 
                (setq first-time nil)
                (senator-re-search-forward
                 (progn
                   (string-match ".*\\.\\([^.]+\\)$"
                                 (concat "." class-name))
                   (match-string 1 (concat "." class-name)))
                 nil t)))
          (setq super-class (car tags)))))))

(defun jde-get-parents ()
  "Returns a list with all the parents (super class and interfaces,
if any) of the current class or interface."
  (jde-remove-type 
   (append (semantic-tag-type-superclasses
	    (semantic-current-tag-of-class 'type))
	   (semantic-tag-type-interfaces (semantic-current-tag-of-class 
					  'type)))))

(defun jde-remove-type (list) 
  "Removes generics '<Type>' declaration from every given
class/interface name."
  (mapcar '(lambda(s) (replace-regexp-in-string "<.*>" "" s)) list))

(defun jde-open-class-at-event (event)
  "Like `jde-open-class-at-point', but is mouse-bindable.

Preserves point."
  (interactive "e")
  (jde-open-class-at-point
   (posn-point (event-end event))))

(defun jde-open-class-at-point (&optional position)
  "Opens the source file that defines the class of the symbol at point and
scrolls the source file to the definition of the symbol, which can be the name of
a variable, class, method, or attribute. This function has the
same requirements as the JDEE's field/method completion commands. See, for example,
`jde-complete-menu'. The JDEE searches for the source file first in
`jde-sourcepath', then in `jde-global-classpath', then in
$CLASSPATH, then in the current directory."
  (interactive)
  (if (jde-open-functions-exist)
      (let* ((old-point (if position
			    (prog1
				(point)
			      (goto-char position))))
	     (thing-of-interest (thing-at-point 'symbol))
	     (pair (save-excursion
		     (end-of-thing 'symbol)
		     (jde-parse-java-variable-at-point)))
	     (class-to-open (jde-open-get-class-to-open
			     pair thing-of-interest)))
	(if old-point
	    (goto-char old-point))
	(if (and class-to-open
		 (stringp class-to-open))
	    ;; Handle the case where the definition of the symbol is in the current buffer.
	    (let ((pos
		   (and
		    (string= (car pair) "")
		    (jde-parse-find-declaration-of thing-of-interest))))
	      (ring-insert find-tag-marker-ring (point-marker))
	      (if pos
		  (goto-char pos)
		;; Handle the case where the definition is in another buffer or an
		;; unopened source file.
		(let ((source
		       (jde-find-class-source-file class-to-open)))
		  (if source
		      ;; we have found the source file. So let´s open it and
		      ;; then jump to the thing-of-interest
		      (progn
			(if (typep source 'buffer)
			    (let ((pop-up-frames t))
			      (set-buffer source)
			      (display-buffer source)
			      ;; (jde-mode)
			      ;; (semantic-new-buffer-fcn)
			      ;; (semantic-fetch-tags)
			      )
			  ;; (switch-to-buffer source)
			  ;; (pop-to-buffer source other-window)
			  ;; if the current buffer contains java-file-name do not try to
			  ;; open the file
			  (if (not (string-equal (buffer-file-name) source))
			      (funcall (or jde-open-cap-ff-function-temp-override
					   jde-open-class-at-point-find-file-function)
				       source)))
			(jde-open-jump-to-class thing-of-interest class-to-open))
		    (message "Can not find the source for \"%s\"." class-to-open)))))
	  (message "Cannot determine the class of \"%s\"." thing-of-interest)))
    (message "You need JDEE >= 2.2.6 and Senator to use this command.")))

(defun jde-open-class-source ( &optional unqual-class )
  "Displays source of the class whose name appears at point in the current
Java buffer. This command finds only classes that reside in the source paths
specified by `jde-sourcepath'. You should provide a global setting
for this variable in your .emacs file to accommodate source files that are
not associated with any project."
  (interactive)
  (condition-case err
      (let* ((unqualified-name
	      (or unqual-class
		  (read-from-minibuffer "Class: " (thing-at-point 'symbol))))
	     (class-names
	      ;;expand the names into full names, or a list of names
	      (jde-jeval-r
	       (concat
		"jde.util.JdeUtilities.getQualifiedName(\""
		unqualified-name "\");"))))
	;;Check return value of QualifiedName
	(if (or (eq class-names nil)
		(not (listp class-names)))
	    (error "Cannot find %s" unqualified-name))
	(ring-insert find-tag-marker-ring (point-marker))
	;; Turn off switching project settings to avoid
	;; resetting jde-sourcepath.
	(let ((old-value jde-project-context-switching-enabled-p))
	  (setq jde-project-context-switching-enabled-p nil)
	  ;;If the list is only one long
	  (if (eq 1 (length class-names))
	      ;;then show it
	      (progn(other-window 1)
		    (jde-find-class-source (car class-names)))
		  ;;else let the user choose
	    (let ((class (efc-query-options class-names "Which class?")))
		  (if class
		      (jde-find-class-source class))))
	  (setq jde-project-context-switching-enabled-p old-value)))
    (error
     (message "%s" (error-message-string err)))))

(defalias 'jde-show-class-source 'jde-open-class-source)

;; Thanks to Sandip Chitale <sandip.chitale@blazesoft.com>
(defun jde-show-superclass-source-2 (tags)
  (if tags
      (if (= (length tags) 1)
	  (jde-show-class-source (car tags))
	(let ((parent (efc-query-options tags "Which super class?")))
	  (if parent
	      (jde-show-class-source parent))))
    (error "Superclass not available")))

(defun jde-show-superclass-source ()
  "Show the source for the parent of the class at point."
  (interactive)
  (let ((tags (semantic-tag-type-superclasses
		 (semantic-current-tag-of-class 'type))))
    (jde-show-superclass-source-2 tags)))
;; Thanks to Sandip Chitale <sandip.chitale@blazesoft.com>

(defun jde-show-interface-source ()
  "Show the source for the interface implemented by the class at point.
If the class implements more than one interface, this command prompts
you to select one of the interfaces to show."
  (interactive)
  (let ((tags (semantic-tag-type-interfaces
		 (semantic-current-tag-of-class 'type))))
    (if tags
	(if (= (length tags) 1)
	    (jde-show-class-source (car tags))
	  (let ((interface (efc-query-options tags "Which interface?")))
	    (if interface
		(jde-show-class-source interface)))))))

(defun jde-find-class-source-file (class)
  "Find the source file for a specified class.
CLASS is the fully qualified name of the class. This function searchs
the directories and source file archives (i.e., jar or zip files)
specified by `jde-sourcepath' for the source file corresponding to
CLASS. If it finds the source file in a directory, it returns the
file's path. If it finds the source file in an archive, it returns a
buffer containing the contents of the file. If this function does not
find the source for the class, it returns nil."
  (let ((verified-name (jde-parse-class-exists class))
	outer-class file package)
    (if (null verified-name)
	(error "Class not found: %s" class))
    (setq outer-class (car (split-string verified-name "[$]"))
	  file (concat
		(jde-parse-get-unqualified-name outer-class)
		".java")
	  package (jde-parse-get-package-from-name outer-class))
    (catch 'found
      (loop for path in jde-sourcepath do
	    (progn
	      (setq path (jde-normalize-path path 'jde-sourcepath))
	      (if (and (file-exists-p path)
		       (or (string-match "\.jar$" path)
			   (string-match "\.zip$" path)))
		  (let* ((bufname (concat file " (" (file-name-nondirectory path) ")"))
			 (buffer (get-buffer bufname)))
		    (if buffer
			(throw 'found buffer)
		      (let* ((pkg-path (subst-char-in-string ?. ?/ package))
			     (class-file-name (concat  pkg-path "/" file))
			     success)
			(setq buffer (get-buffer-create bufname))
			(save-excursion
			  (set-buffer buffer)
			  (setq buffer-file-name (expand-file-name (concat path ":" class-file-name)))
			  (setq buffer-file-truename file)
			  (let ((exit-status
				 (archive-extract-by-stdout path class-file-name archive-zip-extract)))
			    (if (and (numberp exit-status) (= exit-status 0))
				(progn
				  (jde-mode)
				  (goto-char (point-min))
				  (setq buffer-undo-list nil)
				  (setq buffer-saved-size (buffer-size))
				  (set-buffer-modified-p nil)
				  (setq buffer-read-only t)
				  (throw 'found buffer))
			      (progn
				(set-buffer-modified-p nil)
				(kill-buffer buffer))))))))
		(if (file-exists-p (expand-file-name file path))
		    (throw 'found (expand-file-name file path))
		  (let* ((pkg-path (subst-char-in-string ?. ?/ package))
			 (pkg-dir (expand-file-name pkg-path path))
			 (file-path (expand-file-name file pkg-dir)))
		    (if (file-exists-p file-path)
			(throw 'found file-path))))))))))

(defcustom jde-preferred-packages
  '("java.util" "java" "javax")
  "Classes from these packages will appear first when reading from user input."
  :group 'jde-project
  :type '(repeat string))

(defun jde-choose-class (classes &optional prompt uq-name confirm-fq-p)
  "Choose a class from user input.

CLASSES are a list of fully qualified classes that are presetned to user as
choices for input.

PROMPT the prompt the user sees.  Don't add the `: ' at the end to this.

UQ-NAME the unqualified name, which is used for the initial input if found as
an import in the buffer.

CONFIRM-FQ-P, if non-nil, confirm the class name even when there
is only one unique fully qualified class found for the simple
class name \(that is the class without the package part in the
name)."
  (flet ((sort-helper
	  (a b)
	  (dolist (pkg jde-preferred-packages)
	    (let ((len (length pkg)))
	      (cond ((eq t (compare-strings pkg 0 len a 0 len))
		     (return t))
		    ((eq t (compare-strings pkg 0 len b 0 len))
		     (return nil))
		    (t (string< a b)))))))
    (setq classes (sort classes 'sort-helper))
    (setq prompt (or prompt "Class"))
    (let ((default (if uq-name
		       (jde-import-get-import uq-name))))
      (setq default (or default (car classes)))
      (if (and (not confirm-fq-p) (= 1 (length classes)))
	  (car classes)
	(efc-query-options classes prompt "Class" jde-read-class-fq-items default)))))

(defvar jde-read-class-items nil
  "*History for `jde-read-class' read items.")

(defvar jde-read-class-fq-items nil
  "*History for `jde-read-class' read items (second part of fully
qualified classes).")

;;;###autoload
(defun jde-read-class (&optional prompt fq-prompt
				 this-class-p confirm-fq-p no-confirm-nfq-p
				 validate-fn)
  "Select a class interactively.  PROMPT is used to prompt the user for the
first class name, FQ-PROMPT is used only if the class name expands into more
than one fully qualified name.

PROMPT text used to prompt the user for the simple class name, or
\"Class\" as the default.  Don't add the colon/space at the end
of this prompt as a default will be added if it exists.

FQ-PROMPT text used to prompt the fully qualified class name, or
\"Select qualified class\" as the default.  Don't add the
colon/space at the end of this prompt as a default will be added
if it exists.

THIS-CLASS-P, if non-nil, use the current class name if no class name at point
and we are in a JDEE buffer.

CONFIRM-FQ-P, if non-nil, confirm the class name even when there
is only one unique fully qualified class found for the simple
class name \(that is the class without the package part in the
name).

NO-CONFIRM-NFQ-P, if non-nil, don't confirm the class to check for fully
qualified classes if it is obtainable from either the point or this class (see
THIS-CLASS-P).  If obtained from the point, then the class name is parsed with
`jde-parse-class-name' for the input.

VALIDATE-FN is a function to validate the class.  This function takes an
argument the output from `jde-parse-class-name' given from the first user input
class name query.  The function should raise an error if there is anything
wrong with the class.  If this is `nil', then no validate is done.

When called interactively, select the class and copy it to the kill ring."
  (interactive (list nil nil t))
  (setq fq-prompt (or fq-prompt "Select qualified class"))
  (let ((ctup (jde-parse-class-name 'point))
	uinput uq-name classes initial-input fqc default)
    (if (and (null ctup)
	     (eq major-mode 'jde-mode)
	     this-class-p)
	(setq ctup
	      (jde-parse-class-name (jde-parse-get-buffer-class))))
    (setq default (cond ((null ctup) nil)
			((first ctup) (first ctup))
			(t (third ctup))))
    (setq prompt (concat (or prompt "Class")
			 (if default
			     (format " (default %s): " default)
			   ": ")))
    (setq uinput
	  (if (and default no-confirm-nfq-p)
	      (prog1
		  default
		(setq fq-prompt (format "%s" fq-prompt)))
	    (read-string prompt nil 'jde-read-class-items default)))
    (setq ctup (jde-parse-class-name uinput))
    (if (null ctup)
	(error "Doesn't appear to be a classname: `%s'" uinput))
    (if validate-fn
	(setq fqc (funcall validate-fn ctup)))
    (when (not (eq 'pass fqc))
      (setq fqc (first ctup)
	    uq-name (third ctup))
      (if fqc
	  (if (not (jde-jeval-r (concat "jde.util.JdeUtilities."
					"classExists(\""
					fqc "\");")))
	      (error "No match for %s" uq-name))
	(setq classes (jde-jeval-r (concat "jde.util.JdeUtilities."
					   "getQualifiedName(\""
					   uq-name "\");")))
	(if (= 0 (length classes))
	    (error "Not match for %s" uq-name))
	(setq fqc (jde-choose-class classes fq-prompt uq-name confirm-fq-p))))
    (when (interactive-p)
      (kill-new fqc)
      (message "Copied `%s'" fqc))
    fqc))

;;;###autoload
(defun jde-find-class-source (class &optional other-window)
  "*Find the source file for a specified fully qualified class.
Calls `jde-find-class-source-file' to do the search.
If it finds the source file, it opens the file in a buffer."
  (interactive (list (jde-read-class "Class") current-prefix-arg))
  (let ((source (jde-find-class-source-file class)))
    (if source
	(progn
	  (if (typep source 'buffer)
	      (switch-to-buffer source)
	      ;; (pop-to-buffer source other-window)
	    (if (not (string-equal (buffer-file-name)  source))
		(if other-window
		    (find-file-other-window source)
		  (find-file source))))
	  (if (fboundp 'senator-re-search-forward)
	      (let ((inner-class-pos (string-match "\\$" class)))
		(if inner-class-pos
		    (let ((inner-class (substring class (+ 1 inner-class-pos))))
		      (when inner-class
			(goto-char (point-min))
			(senator-parse)
			(senator-re-search-forward
			 (concat "\\b" inner-class "\\b") nil t)))))))
      (message "JDE error: Could not find source for \"%s\" in this
project's source path. See `jde-sourcepath' for more information." class))))

(provide 'jde-open-source)

;; End of jde-open-source.el
