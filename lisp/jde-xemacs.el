;;; jde-xemacs.el -- xemacs specific code for JDEE.
;; Keywords: java, tools, debugging

;; Copyright (C) 2002, 2003, 2004 Andy Piper <andy@xemacs.org>
;; Copyright (C) 2002 Paul Kinnucan <paulk@mathworks.com>
;; 
;; This file is part of XEmacs.
;; 
;; XEmacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; XEmacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with XEmacs; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Please send bug reports and enhancement suggestions
;; to Andy Piper at <andy@xemacs.org>
;; 
;; If you don't use XEmacs, you should! XEmacs kicks some serious
;; butt!


;; XEmacs doesn't have replace-regexp-in-string so define our own
;; version
(unless (fboundp 'replace-regexp-in-string)
(defun replace-regexp-in-string (regexp rep string &optional
					fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacments it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb) ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))
)

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
    (let ((i (length string))
	  (newstr (if inplace string (copy-sequence string))))
      (while (> i 0)
	(setq i (1- i))
	(if (eq (aref newstr i) fromchar)
	    (aset newstr i tochar)))
      newstr)))

;; For non-MULE versions of xemacs
(unless (fboundp 'coding-system-list)
  (defun coding-system-list (&optional base-only)
    '(raw-text)))

;; XEmacs is missing `match-string-no-properties'.
(unless (fboundp 'match-string-no-properties)
  (defun match-string-no-properties (num &optional string)
     "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
    (let ((match (match-string num string)))
       (and match (set-text-properties 0 (length match) nil match))
      match)))

(unless (fboundp 'line-beginning-position)
  (defun line-beginning-position (&optional n)
    "Return the character position of the first character on the 
current line. With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position."
    (save-excursion (beginning-of-line n) (point))))

(unless (fboundp 'line-end-position)
  (defun line-end-position (&optional n)
    "Return the character position of the last character on the 
current line. With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position."
    (save-excursion (end-of-line n) (point))))

;; XEmacs does not define run-with-timer and run-with-idle-timer

(unless (fboundp 'run-with-timer)
  (defun run-with-timer (secs repeat function &rest args)
    (start-itimer "timer" function secs repeat
                  nil (if args t nil) args))
  (defun run-with-idle-timer (secs repeat function &rest args)
    (start-itimer "idle timer"
                  function secs (if repeat secs nil)
                  t (if args t nil) args)))

(when (featurep 'toolbar)
  (require 'debug-toolbar))

(require 'jde-bug)
(require 'jde-compile)

;; Install gui options on XEmacs versions that can understand them
(when (and (featurep 'widget)
	   (>= emacs-major-version 21)
	   (>= emacs-minor-version 4)
	   (>= emacs-patch-level 10))
  (require 'efc-xemacs))

;; Redefine toolbar-debug and toolbar-compile so that clicking the
;; icons on the toolbar will pop us into jde functions.
(defun toolbar-debug ()
  (interactive)
  (call-interactively 'jde-debug))

(defun toolbar-compile ()
  (interactive)
  (call-interactively 'jde-compile))

(add-hook 'jde-bug-minor-mode-hook 
	  '(lambda (&optional on)
	     (if on
		 (easy-menu-add jde-bug-menu-spec jde-bug-mode-map)
	       (easy-menu-remove jde-bug-menu-spec))))

(defvar jde-xemacs-old-toolbar nil
  "Saved toolbar for buffer.")

(defvar jde-xemacs-old-hooks nil
  "Saved hooks for buffer.")

(defvar jde-xemacs-bug-mode-active nil
  "Indicates whether jde-xemacs-bug-minor-mode is active.")

(defvar jde-xemacs-bug-minor-mode nil
  "Indicates whether buffer is in jde-xemacs-bug-minor-mode or not")
(make-variable-buffer-local 'jde-xemacs-bug-minor-mode)

;; Make sure we only get the toolbar when we start debugging.
(add-hook 'jde-dbs-debugger-hook 'jde-xemacs-bug-minor-mode)

(defvar jde-xemacs-bug-initial-readonly  'undefined
  "read-only status of buffer when not in jde-xemacs-bug-minor-mode")

(defvar jde-xemacs-bug-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (set-keymap-name map 'jde-xemacs-bug-minor-mode-map)
;    (set-keymap-parent map jde-bug-mode-map)
    (define-key map "\C-x\C-q" 'jde-xemacs-bug-minor-mode) ; toggle read-only
    (define-key map "c" 'jde-bug-continue)
    (define-key map "n" 'jde-bug-step-over)
    (define-key map "i" 'jde-bug-step-into)
    (define-key map "b" 'jde-bug-toggle-breakpoint)
    (define-key map "r" 'jde-bug-step-out)
    (define-key map "g" 'jde-debug)
    (define-key map "u" 'jde-bug-up-stack)
    (define-key map "d" 'jde-bug-down-stack)
    (define-key map "p" 'jde-bug-evaluate-expression)
    (define-key map "q" 'jde-bug-exit)
    map)
  "Minor keymap for buffers in jde-xemacs-bug-minor-mode")

;; Create a new minor mode jde-bug-minor-mode is no good because it is
;; unconditionally on.
(semantic-add-minor-mode 'jde-xemacs-bug-minor-mode "[src]" jde-xemacs-bug-minor-mode-map)

(defvar jde-xemacs-toolbar
  '([debug::toolbar-stop-at-icon
     jde-bug-toggle-breakpoint
     t
     "Stop at selected position"]
    [debug::toolbar-stop-in-icon
     jde-bug-set-conditional-breakpoint
     nil
     "Stop in function whose name is selected"]
    [debug::toolbar-clear-at-icon
     jde-bug-toggle-breakpoint
     t
     "Clear at selected position"]
    [debug::toolbar-evaluate-icon
     jde-bug-evaluate-expression 
     (and 
      (jde-dbs-debugger-running-p)
      (jde-dbs-get-target-process))
     "Evaluate selected expression"]
    [debug::toolbar-run-icon
     jde-debug
     t
     "Run current program"]
    [debug::toolbar-cont-icon
     jde-bug-continue
     (jde-dbs-target-process-runnable-p)
     "Continue current program"]
    [debug::toolbar-step-into-icon
     jde-bug-step-into 
     (jde-dbs-target-process-steppable-p)
     "Step into (aka step)"]
    [debug::toolbar-step-over-icon
     jde-bug-step-over 
     (jde-dbs-target-process-steppable-p)
     "Step over (aka next)"]
    [debug::toolbar-up-icon
     jde-xemacs-toolbar-up 
     (or
      (not (jde-dbs-target-process-steppable-p))
      (let* ((process (jde-dbs-get-target-process))
	     (stack-max 
	      (if (slot-boundp process 'stack)
		  (1- (length (oref process stack)))
		0))
	     (stack-ptr (oref process stack-ptr)))
	(< stack-ptr stack-max)))
     "Go Up (towards \"cooler\" - less recently visited - frames, or superclass)"]
    [debug::toolbar-down-icon
     jde-xemacs-toolbar-down
     (or
      (not (jde-dbs-target-process-steppable-p))
      (let* ((process (jde-dbs-get-target-process))
	     (stack-ptr (oref process stack-ptr)))
	(> stack-ptr 0)))
     "Go Down (towards \"warmer\" - more recently visited - frames, or class at point)"]
    [debug::toolbar-fix-icon
     nil
     nil
     "Fix (not available with jde-bug)"]
    [debug::toolbar-build-icon
     jde-compile
     t
     "Compile the current file"]
    ))

(defun jde-xemacs-bug-minor-mode (arg &optional quiet)
  "Minor mode for interacting with JDEbug from a Java source file.
With arg, turn jde-xemacs-bug-minor-mode on iff arg is positive.  In
jde-xemacs-bug-minor-mode, you may send an associated JDEbug buffer commands
from the current buffer containing Java source code."
  (interactive "P")
  (setq jde-xemacs-bug-minor-mode
	(if (null arg)
	    (not jde-xemacs-bug-minor-mode)
	  (> (prefix-numeric-value arg) 0)))

  (cond (jde-xemacs-bug-minor-mode
	 ;; Turn on jde-xemacs-bug-minor-mode
	 (when (not (local-variable-p 'jde-xemacs-bug-initial-readonly (current-buffer)))
	   (set (make-local-variable 'jde-xemacs-bug-initial-readonly)
		buffer-read-only))
	 (jde-xemacs-insert-toolbar nil)
	 (setq buffer-read-only t)
	 ;; Save old hooks and make sure we get turned on for new
	 ;; buffers.
	 (unless jde-xemacs-bug-mode-active
	   (setq jde-xemacs-old-hooks jde-entering-java-buffer-hook)
	   (setq jde-entering-java-buffer-hook '(jde-xemacs-bug-minor-mode-hook))
	   ;; Make sure turning off jde-bug mode turns us off also.
	   (add-hook 'jde-bug-minor-mode-hook 'jde-xemacs-bug-minor-mode)
	   (setq jde-xemacs-bug-mode-active t))
	 ;; Killing the buffer kills the mode
	 (make-local-hook 'kill-buffer-hook)
	 (add-hook 'kill-buffer-hook 'jde-xemacs-bug-minor-mode-reset nil t)
	 (or quiet (message "Entering jde-xemacs-bug-minor-mode...")))
	(t
	 ;; Turn off jde-xemacs-bug-minor-mode
	 (and (local-variable-p 'jde-xemacs-bug-initial-readonly (current-buffer))
	      (progn
		(setq buffer-read-only jde-xemacs-bug-initial-readonly)
		(kill-local-variable 'jde-xemacs-bug-initial-readonly)
		))
	 (jde-xemacs-insert-toolbar t)
	 ;; First time through kill the hooks and reset all other
	 ;; associated buffers.
	 (when jde-xemacs-bug-mode-active
	   (setq jde-entering-java-buffer-hook jde-xemacs-old-hooks)
	   (setq jde-xemacs-old-hooks nil)
	   (setq jde-xemacs-bug-mode-active nil)
	   (remove-hook 'jde-bug-minor-mode-hook 'jde-xemacs-bug-minor-mode)
	   (jde-xemacs-bug-minor-mode-reset))
	 (or quiet (message "Exiting jde-xemacs-bug-minor-mode..."))))
  (redraw-modeline t))

(defun jde-xemacs-bug-minor-mode-hook ()
  "Hook function to run when entering a Java buffer while in bug-minor-mode."
  (jde-xemacs-bug-minor-mode t t))

(defun jde-xemacs-bug-minor-mode-reset ()
  ;; tidy house and turn off jde-xemacs-bug-minor-mode in all buffers
  (mapcar #'(lambda (buffer) 
	      (set-buffer buffer)
	      (cond ((local-variable-p 'jde-xemacs-bug-initial-readonly (current-buffer))
		     (jde-xemacs-bug-minor-mode -1 t))))
	  (buffer-list)))

;;;###autoload
(defun jde-xemacs-insert-toolbar (&optional remove)
  "Insert or remove JDE toolbar in the XEmacs toolbar."
  (interactive "P")
  (when (featurep 'toolbar)
    (if remove
	(progn
	  (if (and jde-xemacs-old-toolbar (not (eq jde-xemacs-old-toolbar 'default)))
	      (set-specifier default-toolbar
			     (cons (current-buffer)
				   jde-xemacs-old-toolbar))
	    (remove-specifier default-toolbar (current-buffer)))
	  (kill-local-variable 'jde-xemacs-old-toolbar))
      (unless jde-xemacs-old-toolbar
	(set (make-local-variable 'jde-xemacs-old-toolbar)
	     (or (specifier-specs default-toolbar (current-buffer)) 'default)))
      (set-specifier default-toolbar (cons (current-buffer)
					   jde-xemacs-toolbar)))))

(defun jde-xemacs-toolbar-up ()
  (interactive)
  (if (jde-dbs-target-process-steppable-p)
      (jde-bug-up-stack)
    (jde-show-superclass-source)))

(defun jde-xemacs-toolbar-down ()
  (interactive)
  (if (jde-dbs-target-process-steppable-p)
      (jde-bug-down-stack)
    (jde-show-class-source)))

(provide 'jde-xemacs)
