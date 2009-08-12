;; jde-run.el --- runs the Java app in the current buffer.
;; $Id$

;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: tools, processes

;; Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2008 Paul Kinnucan
;; Copyright (C) 2009 by Paul Landes

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

;; Display line-numbers, but has problems with filling (Emacs hangs) and
;; killing lines (confusing the number display).

(require 'setnu)

(defcustom jde-turn-on-setnu-mode-threshold 20000
 "Maximum number of bytes in a file (buffer) that can result in
automatic line numbering.")

(defvar jde-setnu-deletion-check t "deletion check")
(make-variable-buffer-local 'jde-setnu-deletion-check)

(add-hook 
 'after-change-functions 
 ;; when in setnu-mode toggles setnu-mode off and on.
 (lambda (start end length)
   (if setnu-mode
       (if (or
	    (and
	     (> length 0)
	     jde-setnu-deletion-check)
	    (string-match 
		  "[\n\r]" 
		  (buffer-substring-no-properties start end)))
	   (run-with-timer 
	    0.001 nil
	    ;; setnu toggler      
	   (lambda () (setnu-mode) (setnu-mode))))
     (setq jde-setnu-deletion-check nil))))

(add-hook 
 'before-change-functions 
 ;; Determines whether any newlines were deleted
 (lambda (start end) 
   (if setnu-mode
       (if (> end start) 
	   (setq jde-setnu-deletion-check 
		 (string-match "[\n\r]" (buffer-substring-no-properties
					 start end)))))))

(provide 'jde-setnu)
