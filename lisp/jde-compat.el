;;; jde-compat.el -- Integrated Development Environment for Java.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Kinnucan
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004 Paul Kinnucan.

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
;; 
;; This library is intended to eliminate compiler warnings caused
;; by reference to variables that are defined only in Emacs or
;; only in XEmacs or in a particular version of either.
;; This library is required only for compilation.

;;; Code:

;;;###autoload
(defconst jde-xemacsp (string-match "XEmacs" (emacs-version))
  "Non-nil if we are running in the XEmacs environment.")

(defconst jde-xemacs20p (and jde-xemacsp (>= emacs-major-version 20)))

(defconst jde-emacs21p (and (string-match "\\bEmacs\\b" (emacs-version))
			    (>= emacs-major-version 21)))

;; Define XEmacs- and Emacs-only variables 
;; and functions to avoid compiler warnings.
  (if jde-xemacsp
      (progn
	(defvar w32-start-process-show-window nil)
	(defvar w32-quote-process-args nil)
	(defvar last-nonmenu-event nil)
	(defvar tags-table-format-hooks nil)
	(defvar auto-insert nil)
	(defvar message-log-max nil)
	(defun easy-menu-create-menu (&rest args))
	(defun define-key-after (&rest args))
	(defun ange-ftp-ftp-name (&rest args))
	(defun ange-ftp-get-file-entry (&rest args))
	(defun make-overlay (beg end &optional buffer front-advance rear-advance))
	(defun delete-overlay (overlay))
	(defun overlay-start (overlay))
	(defun overlay-end (overlay))
	(defun overlay-lists ())
	(defun event-start (ev))
	(defun posn-point (p))
	(defun posn-window (p))
	(defalias 'frame-char-width 'frame-width)
	(defalias 'frame-char-height 'frame-height))
    (progn
      ;; These are XEmacs-only functions
      (defvar current-menubar nil)
      (defvar tags-table-format-hooks nil)
      (defvar windowed-process-io nil)
      (unless (fboundp 'add-submenu)
	(defun add-submenu (&rest args)))
      (defun frame-property (&rest args))
      (defun frame-highest-window (&rest args))
      (defun mswindows-cygwin-to-win32-path (&rest args))
      (defun ange-ftp-ftp-name (&rest args))
      (defun ange-ftp-get-file-entry (&rest args))
      (defun mswindows-cygwin-to-win32-path (&rest args))
      (defun add-submenu (&rest args))
      (defun add-menu (&rest args))
      (defun easy-menu-create-keymaps (&rest args))
      (defun locate-data-directory (&rest args))
      (defun temp-directory (&rest args))
      (defun extent-at (&rest args))
      (defun make-extent (&rest args))
      (defun set-extent-face (&rest args))
      (defun set-extent-priority (&rest args))
      (defun extent-property (&rest args))
      (defun delete-extent (&rest args))
      (defun map-extents (&rest args))
      (defun extent-start-position (&rest args))))


;; Define XEmacs- and Emacs-only variables 
;; and functions to avoid compiler warnings.
(if (not jde-xemacsp)
    (defun make-event (&rest args)))

(if jde-emacs21p
    (progn
      (defvar browse-url-new-window-p nil)
      (defun hscroll-window-column (&rest args))))

;; Avoid undefined function error when compiling jde-help.
(defun url-file-exists (&args))

;; Required to compile jde-java-font-lock.el in versions
;; of Emacs that lack the cc-fonts package.
(eval-when-compile
  (if (not (fboundp 'c-make-font-lock-search-function))
      (defun c-make-font-lock-search-function (&rest args))))

;; From custom web page for compatibility between versions of custom:
(condition-case ()
    (require 'custom)
  (error nil))
(if (and (featurep 'custom) (fboundp 'custom-declare-variable))
    nil	;; We've got what we needed
  ;; We have the old custom-library, hack around it!
  (defmacro defgroup (&rest args)
    nil)
  (defmacro defface (var values doc &rest args)
    `(progn
       (defvar ,var (quote ,var))
       ;; To make colors for your faces you need to set your .Xdefaults
       ;; or set them up ahead of time in your .emacs file.
       (make-face ,var)
       ))
  (defmacro defcustom (var value doc &rest args)
    `(defvar ,var ,value ,doc)))

(provide 'jde-compat)

;; Change History 

;;
;; $Log: jde-compat.el,v $
;; Revision 1.6  2004/10/11 03:59:22  paulk
;; Various updates.
;;
;; Revision 1.5  2004/07/01 14:04:39  jslopez
;; Compatibility fix for emacs in CVS. Replaces jde-xemacsp check for boundp for
;; the following variables: compilation-nomessage-regexp-alist,
;; compilation-file-regexp-alist, compilation-leave-directory-regexp-alist,
;; compilation-enter-directory-regexp-alist. Uses the compilation-mode without a
;; parameter. The emacs in CVS does not contain the variables, or the parameter
;; for compilation mode.
;;
;; Revision 1.4  2003/10/23 03:57:11  paulk
;; Added compatibility fix for older versions of XEmacs. Thanks to Len Trigg.
;;
;; Revision 1.3  2003/04/01 04:05:15  paulk
;; Add change log to end of file.
;;
;;

;; End of jde-compat.el
