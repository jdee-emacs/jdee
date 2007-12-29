;;; jdecompletion.el -- Smart completion for the JDE

;; Author: Rodrigo Reyes <reyes@chez.com>
;; Maintainer: Rodrigo Reyes
;; Keywords: java, intellisense, completion

;; Copyright (C) 1999 Rodrigo Reyes

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

;;
;; This package adds smart completion to the JDE. How it works is
;; simple : put the cursor at the end of a statement "under
;; construction", eg. "myVariable.rem<CURSOR HERE> and call the
;; prf2-complete-at-point emacs-lisp function (this is by default
;; C-.). A completion is then inserted. If multiple completions are
;; possible, calling the completion function again will cycle through
;; all the possibilities (as dabbrev-mode does).

;; To retrieve all the possible completions, it uses the java code in
;; jde.util.Completion.getClassInfo(), called by beanshell. That
;; need the class to be compiled (but that's not worst than an etag
;; call).

;; Known bugs/problems :

;; - The first call to the bsh function is bugged, and part of the
;; output is trashed. Starting the bsh before completing, or just
;; ignoring the first error and call the completion again, work fine.
;; - Due to the way the JVM works, it is not possible to explicitly
;; unload a class. So, if major changes are done in a class, the
;; beanshell must be restarted in order to reload the class.

;;
;; TODO :
;;
;; - [EASY] Check for the variables,
;; - [EASY] Check for the static classes
;; - [NOT THAT EASY] Keep the completion information in the minibuffer
;; (it is currently erased after the user presses a key).
;; - [AVERAGE] Add a cache for the class informations.

;; The latest version of the JDE is available at
;; <URL:http://sunsite.auc.dk/jde/>.
;; <URL:http://www.geocities.com/SiliconValley/Lakes/1506/>

;; Please send any comments, bugs, or upgrade requests to
;; Paul Kinnucan at paulk@mathworks.com.

(defvar prf2-current-list nil
"The list of all the completion. Each element of the list is a list
which car is the possible completion, and the cdr is an additional
information about this completion.")

(defvar prf2-current-list-index nil
"An index to an element in prf2-current-list. This is used to
cycle the list.")

(defvar prf2-current-beginning (make-marker) 
"The beginning of the region where the last completion was inserted.")

(defvar prf2-current-end (make-marker)
"The end of the region where the last completion was inserted.")

(defun prf2-import-list ()
  "Build the list of java package declared in the current buffer.
It mostly scans the buffer for 'import' statements, and return the
resulting list. It impliciyly adds the java.lang.* package."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let (lst first second)
      (while (not (null 		 
		   (re-search-forward "import[ \t\n\r]+\\(\\([a-zA-Z0-9]+[.]\\)+\\)\\([*]\\|[a-zA-Z0-9]+\\)" nil t) ))
	(setq first (match-string 1))
	(setq second (match-string 3))
	(if (string= "*" second)
	    (setq lst (append lst
			      (list (list first second))))
	  (setq lst (append (list (list first second))
			    lst))))
      (if (not (member "java.lang.*" lst))
	  (setq lst (append lst (list (list "java.lang." "*")))))
      lst)))

(defun prf2-valid-java-declaration-at (point varname)
"Verify that a POINT starts a valid java declaration
for the VARNAME variable."
(save-excursion
  (goto-char point)
  (if (looking-at (concat "\\([A-Za-z_.]+\\)[ \t\n\r]+" varname "[ \t\n\r]*[;=]"))
      (match-string 1)
    nil)))
  
(defun prf2-declared-type-of (name)
"Find in the current buffer the java type of the variable NAME.  The
function returns a string containing the name of the class, or nil
otherwise. This function does not give the fully-qualified java class
name, it just returns the type as it is declared."
(interactive)
(save-excursion
    (let (found res pos orgpt resname)
      (while (and (not found)
		  (search-backward name nil t))
	(setq pos (point))
	(backward-word 1)
	(setq resname (prf2-valid-java-declaration-at (point) name))
	(goto-char pos)
	(forward-char -1)
	(if (not (null resname))
	    (progn (setq res resname)
		   (setq found t))))
    res)))

(defun prf2-filter-fqn (importlist)
"Filter all the fully-qualified classnames in the import list. It uses
the knowledge that those classnames are at the beginning of the list,
so that it can stops at the first package import (with a star `*' at
the end of the declaration)."
  (if (not (null importlist))
      (if (string= "*" (car (cdr (car importlist))))
	  importlist
	(prf2-filter-fqn (cdr importlist)))))



(defun prf2-guess-type-of (name)

"Guess the fully qualified name of the class NAME, using the import
list. It returns a string if the fqn was found, or a list of possible
packages otherwise."

  (let ((importlist (prf2-import-list)) shortname fullname tmp result)
    (while (and (not (null importlist)) (null result))
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

(defun prf2-get-classinfo (name)
"Return the class info list for the class NAME (possibly the short
java name). This list contains lists of elements, which car is a
possible completion, and the cdr gives additional informations on the
car."
  (let ((guessed (prf2-guess-type-of name)) result)
    (if (stringp guessed)
	(setq result (bsh-eval 
                      (oref 'jde-bsh the-bsh)
		      (concat "jde.util.Completion.getClassInfo(\"" guessed "\");")))
      (if (not (null name))
	  (setq result (bsh-eval
                        (oref 'jde-bsh the-bsh)
                        (prf2-get-classinfo-javacode name guessed)))))
    (if (not (null result))
	(eval (read result))
      nil)))


(defun prf2-get-classinfo-javacode (name import)
"Return the java code that calls the
jde.util.Completion.getClassInfo function with the short java class
name NAME and the package list IMPORT where to look at."
  (interactive)
  (save-excursion
    (concat "{ " 
	      "String[] lst = new String[" (length import) "];\n"
	      (let ((count -1))
		(mapconcat (function (lambda (x) 
				       (setq count (+ 1 count))
					   (concat "lst[" count "]=\"" 
						   (car (nth count import)) "\";\n")))
			   import
			   " "))
	      "jde.util.Completion.getClassInfo(\"" name "\",lst);\n"
	      "}")))


(defun prf2-java-variable-at-point ()
  "Return the current word, according to java syntax.
A '.' is  part of a name."
  (interactive)
  (save-excursion
    (let (start varname curcar found 
		(original-point (point)) 
		intermediate-point beg-point)
      (setq curcar (char-before))
      (while (null found)
	(cond 
	 ((or (and (>= curcar ?a) (<= curcar ?z))
		    (and (>= curcar ?A) (<= curcar ?Z))
		    (member curcar '(?_)))
	  (forward-char -1))
	 ((eq ?. curcar)
	  (setq found (point)))
	 (t
	  (setq found t)))
	(setq curcar (char-before)))
      ;;
      (setq intermediate-point (point))
      (if (not (eq t found))
	  (progn 
	    (setq curcar (char-before))
	    (while (or (and (>= curcar ?a) (<= curcar ?z))
		       (and (>= curcar ?A) (<= curcar ?Z))
		       (member curcar '(?. ?_)))
	      (forward-char -1)
	      (setq curcar (char-before)))
	    (setq beg-point (point))
	    (set-marker prf2-current-beginning intermediate-point)
	    (set-marker prf2-current-end original-point)
	    (list (buffer-substring beg-point (- intermediate-point 1))
		  (buffer-substring intermediate-point original-point)))
	nil))))

(defun prf2-build-completion-list (classinfo)
"Build a completion list from the CLASSINFO list, as returned by the
jde.util.Completion.getClassInfo function."
  (let ((result nil) (tmp nil))
    ;; get the variable fields
    (setq tmp (car classinfo))
    (while (not (null tmp))
      (setq result (append (list (list (car tmp))) result))
      (setq tmp (cdr tmp)))
    ;; get the methods 
    (setq tmp (nth 2 classinfo))
    (while (not (null tmp))
      (setq result (append (list (list (concat (car (car tmp))"(")
				       (prf2-build-information-for-completion (car tmp))
				       ;; (car tmp)
				       )) result))
      (setq tmp (cdr tmp)))
    result))

(defun prf2-build-information-for-completion (lst)
  (let ((result (concat (car (cdr lst)) " " (car lst) "(")))
    (setq lst (cdr (cdr lst)))
    (while (not (null lst))
      (setq result (concat result (car lst)))
      (setq lst (cdr lst))
      (if (not (null lst))
	  (setq result (concat result ", "))))
    (setq result (concat result ")"))
    result))

(defun prf2-complete-cycle ()
  "Replace the previous completion by the next one in the list."
  (let (elem)
    (setq prf2-current-list-index (+ 1 prf2-current-list-index))
    (if (>= prf2-current-list-index (length prf2-current-list))
	(setq prf2-current-list-index 0))
    (setq elem (nth prf2-current-list-index prf2-current-list))
    (if (not (null (car elem)))
	(progn
	  (delete-region prf2-current-beginning prf2-current-end)
	  (insert (car elem))))
    (set-marker prf2-current-end (+ (marker-position prf2-current-beginning) (length (car elem))))
    (message (car (cdr elem)))
  ;;  (goto-char (marker-position prf2-current-end))
    ))

(defun prf2-all-completions (pat lst)
  (let ((result nil))
    (while (not (null lst))
      (if (equal 0 (string-match pat (car (car lst))))
	  (setq result (append (list (car lst)) result)))
      (setq lst (cdr lst)))
    result))

(defun prf2-complete-at-point ()
"Smart-complete the method at point."
  (interactive)
    (if (and
	 (not (null prf2-current-list))
	 (markerp prf2-current-beginning)
	 (markerp prf2-current-end)
	 (marker-position prf2-current-beginning)
	 (marker-position prf2-current-end)
	 (>= (point) (marker-position prf2-current-beginning))
	 (<= (point) (marker-position prf2-current-end))
	 (eq last-command this-command))
	(progn  
	  (prf2-complete-cycle))
      (let* ((pair (prf2-java-variable-at-point)) 
	     txt classinfo fulllist
	     )
	(if (not (null pair))
	    (progn 
	      (setq classinfo (prf2-get-classinfo (prf2-declared-type-of (car pair))))
	      (setq fulllist (prf2-build-completion-list classinfo))
	      (setq prf2-current-list (prf2-all-completions (car (cdr pair)) fulllist))
	      (setq prf2-current-list-index -1)
	      (prf2-complete-cycle))
	  (progn
	    (setq prf2-current-list nil)
	    (message "No completion at this point."))))))


;;
;; A default binding, as exemple.
(global-set-key [(control \.)] 'prf2-complete-at-point)

(provide 'jcomplete)

;; end of jdecompletion.el
