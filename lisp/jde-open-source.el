;; jde-open-source.el -- Open class source files
;;
;; $Revision: 1.15 $
;;
;; Author: Klaus Berndl

;; Keywords: java, open files

;; Copyright (C) 2002, 2003, 2004 Klaus Berndl

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

;; Known bugs/problems :
;;
;; TODO
;;
;; The latest version of the JDE is available at
;; <URL:http://jde.sunsite.dk>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at pkinnucan@mediaone.net

(require 'jde-parse)
(require 'jde-util)
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
             (beginning-of-buffer)
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
    (search-forward "{" nil t)
    (setq tags (semantic-tag-type-superclasses
                  (semantic-current-tag-of-class 'type)))
    (setq super-class (car tags))
    (message "Superclass of %s is %s" class-name super-class)
    ;; Now let´s jump to the thing-of-interest. If this is a
    ;; variable-name then we will not find this with senator in
    ;; the opened java-file so we search for the definiton of
    ;; the class itself. This feature is only available if we
    ;; have senator!
    (when (and (fboundp 'senator-search-forward) (not (string= parsed-symbol "")))
      (beginning-of-buffer)
      (setq xtags (semantic-fetch-tags))
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
            (beginning-of-buffer)
            (senator-parse)
            (search-forward "{" nil t)
            (setq tags (semantic-tag-type-superclasses
                          (semantic-current-tag-of-class 'type)))
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

(defun jde-open-class-at-point ()
  "Opens the source file that defines the class of the symbol at point and
scrolls the source file to the definition of the symbol, which can be the name of
a variable, class, method, or attribute. This function has the
same requirements as the JDEE's field/method completion commands. See, for example,
`jde-complete-menu'. The JDEE searches for the source file first in
`jde-sourcepath', then in `jde-global-classpath', then in
$CLASSPATH, then in the current directory."
  (interactive)
  (if (jde-open-functions-exist)
      (let* ((thing-of-interest (thing-at-point 'symbol))
             (pair (save-excursion 
		     (end-of-thing 'symbol)
		     (jde-parse-java-variable-at-point)))
             (class-to-open (jde-open-get-class-to-open
                             pair thing-of-interest)))
        (if (and class-to-open 
		 (stringp class-to-open))
	    ;; Handle the case where the definition of the symbol is in the current buffer.
	    (let ((pos 
		   (and 
		    (string= (car pair) "")
		    (jde-parse-find-declaration-of thing-of-interest))))
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
    (jde-show-class-source "Object")))

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


(provide 'jde-open-source)

;; $Log: jde-open-source.el,v $
;; Revision 1.15  2004/12/17 04:21:26  paulk
;; Create infrastructure for supporting source archives in jde-sourcepath.
;;
;; Revision 1.14  2004/07/09 04:29:45  paulk
;; Update to reflect new nomenclature in semantic 2.0.
;;
;; Revision 1.13  2004/01/13 06:15:25  paulk
;; Some additional tweaks to jde-open-class-at-point.
;;
;; Revision 1.12  2004/01/13 06:07:25  paulk
;; Rewrote jde-open-class-at-point to be more succinct and clear.
;;
;; Revision 1.11  2003/10/06 12:30:37  jslopez
;; Fixes several broken cases with jde-open-class-at-point.
;;
;; Revision 1.10  2003/07/19 05:44:48  paulk
;; Make error message for jde-open-class-at-point more meaningful.
;;
;; Revision 1.9  2003/01/03 16:24:25  jslopez
;; Fixes bug handling finding method definition in a super class.
;; The code was parsing the java file from where it was invoked instead
;; of the class file that was pertinent.
;;
;; Revision 1.8  2002/08/22 04:15:17  jslopez
;; Fixes jde-open-class-at-point to loop through
;; all the super classes looking for the given token.
;;
;; Revision 1.7  2002/07/27 13:21:30  jslopez
;; Fixes regression in jde-open-class-at-point.
;;
;; Revision 1.6  2002/07/27 13:03:19  jslopez
;; Substitute obsolete call to jde-open-base-class-source.
;;
;; Revision 1.5  2002/07/01 04:52:11  paulk
;; - Moved jde-open-class-source, jde-show-superclass-source, jde-show-interface-source from jde-help.el
;;   to jde-open-source.el.
;;
;; - Removed jde-open-source-for-symbol because it has been superceded by jde-open-class-at-point.
;;

;; end jde-open-source.el
