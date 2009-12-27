;;; jde-which-method.el --- Print current method in mode line
;; $Id$

;; Copyright (C) 1997-2004 Paul Kinnucan
;; Copyright (C) 2009 by Paul Landes

;; Author: Paul Kinnucan (paulk@mathworks.com)
;; Inspired by Alex Rezinsky's which-func package.
;; Keywords: mode-line, tools

;; This file is not part of GNU Emacs.

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

;; This package displays the name of the method at point
;; in the Emacs mode line.

;;; Code:

(require 'jde-parse)


(defgroup jde-which-method nil
  "Mode to display the current function name in the modeline."
  :group 'jde)

;;;###autoload
(defcustom jde-which-method-mode t
  "Enables the JDE's which method mode.
When which method mode is enabled, the current method name is
displayed in the mode line."
  :group 'jde-which-method
  :type  'boolean)

(defcustom jde-which-method-format '("[" jde-which-method-current "]")
  "Format for displaying the function in the mode line."
  :group 'jde-which-method
  :type 'sexp)

(defcustom jde-mode-line-format
  '("-"
    mode-line-mule-info
    mode-line-modified
    mode-line-frame-identification
    mode-line-buffer-identification
    "   "
    global-mode-string
    "   %[(" mode-name mode-line-process minor-mode-alist "%n" ")%]--"
    (line-number-mode "L%l--")
    (column-number-mode "C%c--")
    (-3 . "%p")
    (jde-which-method-mode
     ("--" jde-which-method-format "--"))
    "-%-")
  "Format for the JDE source buffer mode line."
  :group 'jde
  :type 'sexp)
  
(defcustom jde-which-full-class-name nil
  "Display full inner-class name in JDE's which method mode. 
If nil then display only the last component of class name.
\(see `jde-which-method-max-length', `jde-which-method-class-min-length')
"
  :group 'jde-which-method
  :type  'boolean)

(defcustom jde-which-method-max-length 20
  "Specify the maximum length of the which-method-string \(see
`jde-which-method-format'). If nil, the string is not \
truncated."
  :type '(choice (const :tag "No truncation" :value nil)
		 (integer :tag "Max. length"))
  :group 'jde-which-method)

(defcustom jde-which-method-class-min-length 4
  "Specifies the minimum length of the class part of the full method
name after truncation of the class name, but only if the class
is displayed and if `jde-which-method-max-length'
is not nil. If the full method name is still greater than
`jde-which-method-max-length', the method part of the name is truncated."
  :type '(integer :tag "Min. length")
  :group 'jde-which-method)


(defcustom jde-which-method-abbrev-symbol "~"
"Symbol used to indicate abbreviated part of a method name."
    :group 'jde-which-method
    :type  'string)

;;; Code, nothing to customize below here
;;; -------------------------------------
;;;

(defvar jde-which-method-idle-timer nil
  "Timer that updates the mode line.")

(defvar jde-which-method-unknown "???"
  "String to display in the mode line when the current method is unknown.")

(defvar jde-which-method-current jde-which-method-unknown)
(make-variable-buffer-local 'jde-which-method-current)

(defvar jde-which-method-previous jde-which-method-unknown)
(make-variable-buffer-local 'jde-which-method-previous)

(defvar jde-which-method-current-point-loc -1)
(make-variable-buffer-local 'jde-which-method-current-point-loc)

(defvar jde-which-method-current-method-bounds (cons -1 -1))
(make-variable-buffer-local 'jde-which-method-current-method-bounds)


(defun jde-which-method-truncate-begin (str truncation)
  (if (> truncation (length jde-which-method-abbrev-symbol))
	 (concat jde-which-method-abbrev-symbol (substring str truncation))
       str))

(defun jde-which-method-truncate-end (str truncation)
  (let ((str-length (length str)))
    (if (> truncation (length jde-which-method-abbrev-symbol))
	(concat (substring str 0 (- str-length truncation))
		jde-which-method-abbrev-symbol)
      str)))

(defun jde-which-method-update ()
  (interactive)
  (if (and
       jde-which-method-mode
       (eq major-mode 'jde-mode))
      (condition-case info
	  (let ((p (point)))
	    (unless (or
		     (= jde-which-method-current-point-loc p)
		     (and
		      (>= p (car jde-which-method-current-method-bounds))
		      (<= p (cdr jde-which-method-current-method-bounds))))
	      (let ((name;; (jde-parse-get-method-at-point)
		     (if jde-parse-the-method-map
			 (jde-parse-method-map-get-method-at jde-parse-the-method-map))
		     ))
		(if name
		    (let* ((name-pair (car name))
			   (class (jde-which-method-class-name name))
			   (method (cdr name-pair))
			   (bounds (cdr name))
			   (class-length (length class))
			   (method-length (length method))
			   ;; initialize the truncation with 0!
			   (trunc-class 0)
			   (trunc-method 0)
			   (trunc-complete 0))
		      (when jde-which-method-max-length
			;; compute necessary truncation of method and/or class
			(if jde-parse-buffer-contains-multiple-classes-p
			    (when (> (+ class-length method-length 1)
				     jde-which-method-max-length)
			      (setq trunc-complete (- (+ class-length
							 method-length 1)
						      jde-which-method-max-length))
			      (if (< (- class-length trunc-complete)
				     jde-which-method-class-min-length)
				  (setq trunc-class
					(- class-length
					   jde-which-method-class-min-length)
					trunc-method (- trunc-complete
							trunc-class))
				(setq trunc-method 0
				      trunc-class trunc-complete)))
			  (when (> method-length jde-which-method-max-length)
			    (setq trunc-method (- method-length
						  jde-which-method-max-length)))))
		      ;; truncate method and class with the computed truncation
		      ;; (possible 0, then no truncation is done in fact)
		      (setq class (jde-which-method-truncate-end class trunc-class)
			    method (jde-which-method-truncate-end method trunc-method))
		      ;; set the displayed string from the (possible truncated)
		      ;; class and method parts according to
		      ;; jde-parse-buffer-contains-multiple-classes-p.
		      (setq jde-which-method-current
			    (if jde-parse-buffer-contains-multiple-classes-p
				(format "M:%s.%s" class method)
			      (format "M:%s" method)))
		      (setq jde-which-method-current-point-loc p)
		      (setq jde-which-method-current-method-bounds bounds))
		  (progn
		    (setq name (jde-which-class-name (jde-parse-get-innermost-class-at-point)))
		    (setq jde-which-method-current-point-loc p)
		    (setq jde-which-method-current-method-bounds (cons -1 -1))
		    (if name
			(let* ((class (car name))
			       (class-length (length class)))
			  ;; possible truncate the string to display
			  (when (and jde-which-method-max-length
				     (> class-length jde-which-method-max-length))
			    (setq class (jde-which-method-truncate-begin class
									 (- class-length
									    jde-which-method-max-length))))
			  (setq jde-which-method-current (format "C:%s" class)))
		      (setq jde-which-method-current jde-which-method-unknown))))
		(unless (equal jde-which-method-current jde-which-method-previous)
		  (setq jde-which-method-previous jde-which-method-current)
		  (force-mode-line-update)))))
	(error
	 ;; (debug)
	 (cancel-timer jde-which-method-idle-timer)
	 (setq jde-which-method-idle-timer nil)
	 (message "Error in jde-which-method-update: %s" info)))))

(defun jde-which-full-class-namef (name) 
  ;; name and return value is: (string . point) or nil.
  (save-excursion
    (do ((rv name)) ((not name) rv)
      (goto-char (cdr name))
      (setq name (jde-parse-get-innermost-class-at-point))
      (if name (setf (car rv) (concat (car name) "." (car rv)))))))

(defun jde-which-class-name(name) 
  ;; use given name or gather full-name:
  (if jde-which-full-class-name
      (jde-which-full-class-namef name)
    name
    ))

(defun jde-which-method-class-name(name)
  (if jde-which-full-class-name
      (car (jde-which-full-class-namef (jde-parse-get-innermost-class-at-point)))
    (caar name)
    ))

(defun jde-which-method-update-on-entering-buffer ()
  ;; This is a hook function. Catch all errors to
  ;; avoid canceling other hooks.
  (condition-case err
      (progn
	(setq jde-which-method-current-point-loc 0)
	(setq jde-which-method-current-method-bounds (cons -1 -1))
	(jde-which-method-update))
    (error (message
	    "Which method update error: %s"
	    (error-message-string err)))))

(provide 'jde-which-method)

;; jde-which-method.el ends here
