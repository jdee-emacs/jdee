;;; jdee-compat.el -- Integrated Development Environment for Java.

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997, 1998, 2001, 2002, 2003, 2004 Paul Kinnucan.
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
;;
;; This library is intended to eliminate compiler warnings caused
;; by reference to variables that are defined only in Emacs or
;; only in XEmacs or in a particular version of either.
;; This library is required only for compilation.

;;; Code:

;; Define Emacs variables and functions to avoid compiler warnings.
(defvar current-menubar nil)
(defvar tags-table-format-hooks nil)
(defvar windowed-process-io nil)
(unless (fboundp 'add-submenu)
  (defun add-submenu (&rest args)))
(defun frame-property (&rest args))
(defun frame-highest-window (&rest args))
(defun ange-ftp-ftp-name (&rest args))
(defun ange-ftp-get-file-entry (&rest args))
(defun mswindows-cygwin-to-win32-path (&rest args))
(defun add-submenu (&rest args))
(defun add-menu (&rest args))
(defun easy-menu-create-keymaps (&rest args))
(defun locate-data-directory (&rest args))
(defun temp-directory (&rest args) temporary-file-directory)
(defun extent-at (&rest args))
(defun make-extent (&rest args))
(defun set-extent-face (&rest args))
(defun set-extent-priority (&rest args))
(defun extent-property (&rest args))
(defun delete-extent (&rest args))
(defun map-extents (&rest args))
(defun extent-start-position (&rest args))
(defun make-event (&rest args))
(defun hscroll-window-column (&rest args))

;; Avoid undefined function error when compiling jdee-help.
(defun url-file-exists (&args))

;; Required to compile jdee-font-lock.el in versions
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

(provide 'jdee-compat)

;;; jdee-compat.el ends here
