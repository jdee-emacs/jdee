;; setnu - line numbering in buffer
;; $Id$

;;; This works better under XEmacs, because it can use the left
;;; margin for the line number glyphs, but it will work under Emacs
;;; also.

;;; vi-style line number mode for Emacs
;;; (requires Emacs 19.29 or later, or XEmacs 19.11 or later)
;;; Copyright (C) 1994, 1995, 1997 Kyle E. Jones
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; A copy of the GNU General Public License can be obtained from this
;;; program's author (send electronic mail to kyle@uunet.uu.net) or from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;;
;;; Send bug reports to kyle@wonderworks.com
;;
;; M-x setnu-mode toggles the line number mode on and off.
;;
;; turn-on-setnu-mode is useful for adding to a major-mode hook
;; variable.
;; Example:
;;     (add-hook 'text-mode-hook 'turn-on-setnu-mode)
;; to automatically turn on line numbering when enterting text-mode."

(provide 'setnu)

(defconst setnu-running-under-xemacs
  (or (string-match "XEmacs" emacs-version)
      (string-match "Lucid" emacs-version)))

(defconst setnu-mode-version "1.05"
  "Version number for this release of setnu-mode.")

(defvar setnu-mode nil
  "Non-nil if setnu-mode is active in the current buffer.")
(make-variable-buffer-local 'setnu-mode)

(defvar setnu-start-extent nil
  "First extent of a chain of extents used by setnu-mode.
Each line has its own extent.  Each line extent has a
`setnu-next-extent' property that points to the next extent in
the chain, which is the extent for the next line in the buffer.
There is also a `setnu-prev-extent' that points at the previous
extent in the chain.  To distinguish them from other extents the
setnu-mode extents all have a non-nil `setnu' property.")
(make-variable-buffer-local 'setnu-start-extent)

(defvar setnu-glyph-obarray (make-vector 401 0)
  "Obarray of symbols whose values are line number glyphs.
Each symbol name is the string represnetation of a number, perhaps
passed with spaces.  The value of the symbol is a glyph that can
be made the begin glyph of an extent to display as a line number.")

(defvar setnu-begin-glyph-property (if (fboundp 'extent-property)
		       'begin-glyph
		     'before-string)
  "Property name to use to set the begin glyph of an extent.")

(defvar setnu-line-number-format (if setnu-running-under-xemacs "%4d" "%4d  ")
  "String suitable for `format' that will generate a line number string.
`format' will be called with this string and one other argument
which will be an integer, the line number.")

(defvar setnu-line-number-face 'bold
  "*Face used to display the line numbers.
Currently this works for XEmacs 19.12 and later versions only.")

(defun setnu-mode (&optional arg)
  "Toggle setnu-mode.
With prefix argument, turn setnu-mode on if argument is positive.
When setnu-mode is enabled, a line number will appear at the left
margin of each line."
  (interactive "P")
  (let ((oldmode (not (not setnu-mode)))
    (inhibit-quit t))
    (setq setnu-mode (or (and arg (> (prefix-numeric-value arg) 0))
	     (and (null arg) (null setnu-mode))))
    (if (not (eq oldmode setnu-mode))
    (if setnu-mode
	(setnu-mode-on)
      (setnu-mode-off)))))

(defun turn-on-setnu-mode ()
  "Turn on setnu-mode.
Useful for adding to a major-mode hook variable.
Example:
    (add-hook 'text-mode-hook 'turn-on-setnu-mode)
to automatically turn on line numbering when enterting text-mode."
  (setnu-mode 1))

;;; Internal functions

;;; The program is written using XEmacs terminology,
;;; e.g. extents, glyphs, etc.  Functions are defined to twist
;;; the FSF Emacs overlay API into the XEmacs model.

(defconst setnu-running-under-xemacs
  (or (string-match "XEmacs" emacs-version)
      (string-match "Lucid" emacs-version)))

(if setnu-running-under-xemacs
    (fset 'setnu-make-extent 'make-extent)
  (fset 'setnu-make-extent 'make-overlay))

(if setnu-running-under-xemacs
    (fset 'setnu-delete-extent 'delete-extent)
  (fset 'setnu-delete-extent 'delete-overlay))

(if setnu-running-under-xemacs
    (fset 'setnu-extent-property 'extent-property)
  (fset 'setnu-extent-property 'overlay-get))

(if setnu-running-under-xemacs
    (fset 'setnu-set-extent-property 'set-extent-property)
  (fset 'setnu-set-extent-property 'overlay-put))

(if setnu-running-under-xemacs
    (fset 'setnu-set-extent-endpoints 'set-extent-endpoints)
  (fset 'setnu-set-extent-endpoints 'move-overlay))

(if setnu-running-under-xemacs
    (fset 'setnu-extent-end-position 'extent-end-position)
  (fset 'setnu-extent-end-position 'overlay-end))

(if setnu-running-under-xemacs
    (fset 'setnu-extent-start-position 'extent-start-position)
  (fset 'setnu-extent-start-position 'overlay-start))

(if setnu-running-under-xemacs
    (defun setnu-set-extent-begin-glyph (e g)
      (set-extent-begin-glyph e g 'outside-margin))
  (defun setnu-set-extent-begin-glyph (e g)
    (overlay-put e setnu-begin-glyph-property g)))

(fset 'setnu-make-glyph (if setnu-running-under-xemacs 'make-glyph 'identity))

(cond ((and setnu-running-under-xemacs (fboundp 'set-glyph-face))
       (fset 'setnu-set-glyph-face 'set-glyph-face))
      (setnu-running-under-xemacs
       (fset 'setnu-set-glyph-face 'ignore))
      (t                ; FSF Emacs
       (defun setnu-set-glyph-face (g face)
     (put-text-property 0 (length g) 'face face g))))

(defun setnu-mode-off ()
  "Internal shutdown of setnu-mode.
Deletes the extents associated with setnu-mode."
  (if (and setnu-running-under-xemacs
       (fboundp 'remove-specifier))
      (remove-specifier left-margin-width (current-buffer)))
  (if setnu-start-extent
      (let (e ee)
    (setq e setnu-start-extent)
    (while e
      (setq ee e)
      (setq e (setnu-extent-property e 'setnu-next-extent))
      (setnu-delete-extent ee))
    (setq setnu-start-extent nil))))

(defun setnu-mode-on ()
  "Internal startup of setnu-mode.
Sets up the extents associated with setnu-mode."
  (if (and setnu-running-under-xemacs
       (fboundp 'set-specifier))
      (set-specifier left-margin-width 6 (current-buffer)))
  (let ((done nil)
    (curr-e nil)
    (n 1)
    (match-data (match-data))
    e start numstr)
    (unwind-protect
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	(setq start (point))
	(while (not done)
	  (setq done (null (search-forward "\n" nil 0)))
	  (setq e (setnu-make-setnu-extent start (point)))
	  (if (null setnu-start-extent)
	  (setq setnu-start-extent e
	    curr-e e)
	(setnu-set-extent-property curr-e 'setnu-next-extent e)
	(setnu-set-extent-property e 'setnu-prev-extent curr-e)
	(setq curr-e e))
	  (setq numstr (format setnu-line-number-format n))
	  (setnu-set-extent-property e 'line-number numstr)
	  (setnu-set-extent-begin-glyph e (setnu-number-glyph numstr))
	  (setq n (1+ n)
	    start (point)))))
      (store-match-data match-data))))

(defun setnu-before-change-function (start end)
  "Before change function for setnu-mode.
Notices when a delete is about to delete some lines and adjusts
the line number extents accordingly."
  (if (or (not setnu-mode) (= start end))
      () ;; not in setnu-mode or this is an insertion
    (let ((inhibit-quit t)
      (start-e nil)
      (match-data (match-data))
      end-e saved-next e ee)
      (unwind-protect
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-char start)
	  (if (search-forward "\n" end t)
	  (progn
	    (setq start-e (setnu-extent-at-create start nil)
	      saved-next (setnu-extent-property
		      start-e
		      'setnu-next-extent))
	    (setq end-e (setnu-extent-at-create end nil))
	    (setnu-set-extent-endpoints
	     start-e
	     (setnu-extent-start-position start-e)
	     (setnu-extent-end-position end-e))
	    (setnu-set-extent-property
	     start-e 'setnu-next-extent
	     (setnu-extent-property end-e 'setnu-next-extent))))
	  (if start-e
	  (progn
	    (setq e (setnu-extent-property start-e 'setnu-next-extent)
	      ee saved-next)
	    (while (and e (setnu-extent-property e 'setnu-next-extent))
	      (setq e (setnu-extent-property e 'setnu-next-extent)
		ee (setnu-extent-property ee 'setnu-next-extent)))
	    (while (and e (not (eq ee start-e)))
	      (setnu-set-extent-begin-glyph
	       e (setnu-extent-property ee setnu-begin-glyph-property))
	      (setnu-set-extent-property
	       e 'line-number (setnu-extent-property ee 'line-number))
	      (setq e (setnu-extent-property e 'setnu-prev-extent)
		ee (setnu-extent-property ee 'setnu-prev-extent)))
	    (setq end-e (setnu-extent-property start-e
			       'setnu-next-extent))
	    (and end-e
	     (setnu-set-extent-property end-e
			    'setnu-prev-extent
			    start-e))
	    (setq e saved-next)
	    (while (not (eq e end-e))
	      (setq ee e
		e (setnu-extent-property e 'setnu-next-extent))
	      (setnu-delete-extent ee))))))
    (store-match-data match-data)))))

(defun setnu-after-change-function (start end length)
  "After change function for setnu-mode.
Notices when an insert has added some lines and adjusts
the line number extents accordingly."
  (if (or (not setnu-mode) (= start end))
      () ; not in setnu-mode or this is a deletion
    (let ((inhibit-quit t)
      (ee nil)
      (match-data (match-data))
      (new-lines 0)
      start-e e saved-end saved-next n numstr)
      (unwind-protect
      (save-excursion
	(save-restriction
	  (widen)
	  (setq start-e (setnu-extent-at-create start nil))
	  (if (< (setnu-extent-end-position start-e) (point))
	  ;; bogus!  insertion didn't put the text into
	  ;; the extent because,
	  ;; a. the extent was zero length or
	  ;; b. this is FSF Emacs which means chars
	  ;;    inserted at the end position of an extent
	  ;;    are not inserted into the extent.
	  (setnu-set-extent-endpoints
	   start-e
	   (setnu-extent-start-position start-e)
	   end))
	  (setq saved-next (setnu-extent-property start-e
			      'setnu-next-extent)
	    saved-end (setnu-extent-end-position start-e)
	    e start-e)
	  (goto-char start)
	  (while (search-forward "\n" end 0)
	(setnu-set-extent-endpoints e
			(setnu-extent-start-position e)
			(point))
	(setq ee (setnu-make-setnu-extent (point) (point)))
	(setnu-set-extent-property e 'setnu-next-extent ee)
	(setnu-set-extent-property ee 'setnu-prev-extent e)
	(setq e ee new-lines (1+ new-lines)))
	  (if ee
	  (progn
	    (setnu-set-extent-endpoints
	     e (setnu-extent-start-position e) saved-end)
	    (setnu-set-extent-property e 'setnu-next-extent saved-next)
	    (and saved-next
	     (setnu-set-extent-property
	      saved-next 'setnu-prev-extent e))
	    (setq e (setnu-extent-property start-e 'setnu-next-extent)
	      ee saved-next)
	    (while ee
	      (setnu-set-extent-begin-glyph
	       e (setnu-extent-property ee setnu-begin-glyph-property))
	      (setnu-set-extent-property
	       e 'line-number (setnu-extent-property ee 'line-number))
	      (setq e (setnu-extent-property e 'setnu-next-extent)
		ee (setnu-extent-property ee 'setnu-next-extent)))
	    (setq n (1+ (string-to-int
		 (setnu-extent-property
		  (setnu-extent-property e 'setnu-prev-extent)
		  'line-number))))
	    (while e
	      (setq numstr (format setnu-line-number-format n))
	      (setnu-set-extent-property e 'line-number numstr)
	      (setnu-set-extent-begin-glyph
	       e (setnu-number-glyph numstr))
	      (setq e (setnu-extent-property e 'setnu-next-extent)
		n (1+ n)))))))
    (store-match-data match-data)))))

(defun setnu-number-glyph (number-string)
  (let ((sym (intern number-string setnu-glyph-obarray)))
    (if (boundp sym)
    (symbol-value sym)
      (let ((g (setnu-make-glyph number-string)))
    (set sym g)
    (setnu-set-glyph-face g setnu-line-number-face)
    g ))))

(defun setnu-make-setnu-extent (beg end)
  "Create an extent and set some properties that all setnu extents have."
  (let ((e (setnu-make-extent beg end)))
    (setnu-set-extent-property e 'setnu t)
;;    (setnu-set-extent-property e 'begin-glyph-layout 'outside-margin)
    (setnu-set-extent-property e 'detachable nil)
    (setnu-set-extent-property e 'evaporate nil)
    e ))

(cond ((fboundp 'overlays-in) ;; expect to see this in 19.30
       (defun setnu-extent-at (pos buf)
     "Finds the setnu extent at the position POS in the buffer BUF."
     (catch 'done
       (save-excursion
	 (and buf (set-buffer buf))
	 (let ((o-list (overlays-in pos (1+ pos))))
	   (while o-list
	 (if (overlay-get (car o-list) 'setnu)
	     (throw 'done (car o-list)))
	 (setq o-list (cdr o-list)))
	   nil )))))
      ((fboundp 'overlays-at)
       (defun setnu-extent-at (pos buf)
     "Finds the setnu extent at the position POS in the buffer BUF."
     (catch 'done
       (save-excursion
	 (and buf (set-buffer buf))
	 (let ((o-list (overlays-at pos)) o-lists)
	   ;; search what overlays-at returns first.  for all
	   ;; but zero length extents this will return the
	   ;; extent we want.
	   (while o-list
	 (if (overlay-get (car o-list) 'setnu)
	     (throw 'done (car o-list)))
	 (setq o-list (cdr o-list)))
	   ;; No luck.  Search the lists returned by
	   ;; overlay-lists.  Use overlays-recenter so we only
	   ;; have to search the `before' lobe of the return
	   ;; value.
	   (overlay-recenter (1- pos))
	   (setq o-lists (overlay-lists))
	   (setq o-list (cdr o-lists))
	   (while o-list
	 (if (and (overlay-get (car o-list) 'setnu)
	      (or (and (= pos (overlay-start (car o-list)))
		   (= pos (overlay-end (car o-list))))
		  (and (>= pos (overlay-start (car o-list)))
		   (< pos (overlay-end (car o-list))))))
	     (throw 'done (car o-list)))
	 (setq o-list (cdr o-list)))
	   nil )))))
       ((fboundp 'map-extents)
    (defun setnu-extent-at (pos buf)
      "Finds the setnu extent at the position POS in the buffer BUF."
      (map-extents (function (lambda (e maparg)
		   (if (setnu-extent-property e 'setnu)
		       e
		     nil)))
	       buf pos pos)))
       (t (error "can't find overlays-in, overlays-at, or map-extents!")))

(defun setnu-extent-at-create (pos buf)
  "Like `setnu-extent-at' except if an extent isn't found, then
it is created based on where the extent failed to be found."
  (let ((e (setnu-extent-at pos buf)) ee beg numstr)
    (if e
    e
      ;; no extent found so one must be created.
      (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (setq e (setnu-extent-at (point) buf))
    (cond (e
	   ;; found one.  extend it to cover this whole line.
	   ;; this takes care of zero length extents that
	   ;; might exist at bob or eob that can't be
	   ;; inserted into.
	   (setq beg (point))
	   (forward-line 1)
	   (setnu-set-extent-endpoints e beg (point))
	   e )
	  ((bobp)
	   ;; we are at bob and there's no extent.
	   ;;
	   ;; this is because the extent that was there got
	   ;; detached because all the text in the buffer was
	   ;; deleted.  so we create a new extent and make it
	   ;; contain the whole buffer, since there can be no
	   ;; other attached extents.
	   (setq e (setnu-make-setnu-extent (point-min) (point-max))
	     numstr (format setnu-line-number-format 1))
	   (setnu-set-extent-property e 'line-number numstr)
	   (setnu-set-extent-begin-glyph e (setnu-number-glyph numstr))
	   (setq setnu-start-extent e)
	   e )
	  (t
	   ;; we must be at eob and there's no extent.
	   ;;
	   ;; this is because the extent that was there
	   ;; shrank to zero length and was detached.  create
	   ;; a new extent that contains all text from point
	   ;; to pos.
	   (setq e (setnu-make-setnu-extent (point) pos))
	   (setq ee (setnu-extent-at (1- (point)) buf))
	   (setnu-set-extent-property e 'setnu-prev-extent ee)
	   (setnu-set-extent-property ee 'setnu-next-extent e)
	   (setq numstr
	     (format setnu-line-number-format
		 (1+ (string-to-int
		  (setnu-extent-property ee 'line-number)))))
	   (setnu-set-extent-property e 'line-number numstr)
	   (setnu-set-extent-begin-glyph e (setnu-number-glyph numstr))
	   e ))))))

(add-hook 'before-change-functions 'setnu-before-change-function)
(add-hook 'after-change-functions 'setnu-after-change-function)