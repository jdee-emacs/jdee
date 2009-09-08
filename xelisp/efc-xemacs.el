;;; efc-xemacs.el -- Emacs Foundation Classes using XEmacs gui features.
;; $Id$

;; Author: Andy Piper <andy@xemacs.org>
;; Maintainer: Andy Piper
;; Keywords: lisp, tools, classes gui

;; Copyright (C) 2002, 2003, 2004 Andy Piper.
;; Copyright (C) 2001, 2002 Paul Kinnucan.
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
;; Boston, MA 02111-1307, US
;;; Commentary:

;; This package contains a set of eieio-based foundation classes
;; for XEmacs.

;;; Code:

(require 'eieio)
(require 'efc)

;; Install ourselves as the default option function,
;; only if this version of XEmacs supports native widgets.
(when (and (fboundp 'make-dialog-box)
	   use-dialog-box)
  (setq efc-query-options-function 'efc-xemacs-query-options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Option Dialog                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-xemacs-option-dialog (efc-dialog)
  ((options        :initarg :options
		   :documentation
		   "Options from from which to choose.")
   (radio-buttons  :initarg :radio-buttons
		   :documentation
		   "Buttons for selecting options.")
   (text           :initarg :text
		   :type string
		   :initform "Select option."
		   :documentation
		   "Text to be inserted at top of dialog.")
   (selection      :initarg :selection
		   :initform nil
		   :documentation
		   "Option chosen by the user."))
   "This dialog allows a user to choose one of a set of OPTIONS by clicking
a radio button next to the option. The dialog sets SELECTION to the option
chosen by the user when the user selects the OK button on the dialog. This
dialog uses recursive edit to emulate a modal dialog.")

(defmethod initialize-instance ((this efc-xemacs-option-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-dialog-show ((this efc-xemacs-option-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (efc-xemacs-option-dialog-show this))

;; This is hack to get round a bug in XEmacs' treatment of :selected
(defvar efc-xemacs-option-dialog-selection nil)

(defun efc-xemacs-option-dialog-show (this)
  (let ((parent (selected-frame)))
    (unless (oref this selection)
      (oset this selection (car (oref this options))))
    (setq efc-xemacs-option-dialog-selection (oref this selection))
    (oset
     this selection
     (make-dialog-box
      'general
      :parent parent
      :title (oref this title)
      :modal t
      :autosize t
      :spec (make-glyph
	     `[layout :orientation vertical
		      :justify center
		      :border [string :data ,(oref this text)]
		      :items ([layout :orientation vertical
				      :horizontally-justify left
				      :vertically-justify center
				      :items
				      ,(mapcar
					(lambda (x)
					  (vector
					   'button :descriptor x
					   :style 'radio
					   :selected
					   (list 'efc-xemacs-option-dialog-action this x)
					   :callback
					   (list 'efc-xemacs-option-dialog-select this x)))
					(oref this options))]
			      [layout :orientation horizontal
				      :justify center
				      :items
				      ([button :descriptor "Ok"
					       :callback-ex
					       (lambda (image-instance event)
						 (efc-xemacs-dialog-ok ,this event))]
				       [button :descriptor "Cancel"
					       :callback-ex 'efc-xemacs-dialog-cancel
					       ])])]))
      )))

(defun efc-xemacs-option-dialog-select (this item)
  (oset this selection item)
  (setq efc-xemacs-option-dialog-selection item))

(defun efc-xemacs-option-dialog-action (this item)
; This doesn't work for some reason I don't understand
;  (equal item (oref this selection))
  (equal efc-xemacs-option-dialog-selection item))

(defun efc-xemacs-dialog-ok (image-instance event)
  "Invoked when the user selects the OK button on the options
dialog. Sets the :selection field of THIS to the option chosen by the
user and kills the dialog window."
  (delete-frame (event-channel event))
  (dialog-box-finish efc-xemacs-option-dialog-selection))

(defun efc-xemacs-dialog-cancel (image-instance event)
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and kills the dialog window."
  (delete-frame (event-channel event))
  (dialog-box-cancel))

(defun efc-xemacs-query-options (options &optional prompt title)
  "Ask user to choose among a set of options."
  (let ((dialog
	 (efc-xemacs-option-dialog
	  (or title "Option Dialog")
	  :title (or title "Option Dialog")
	  :text (if prompt prompt "Select option:")
	  :options options)))
    (efc-dialog-show dialog)
    (oref dialog selection)))

(provide 'efc-xemacs)
