;;; jdee-custom.el -- JDEE customizer

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Keywords: java, tools

;; Copyright (C) 2004 Paul Kinnucan.
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

;;; Code:

(require 'cl-lib)
(require 'cus-edit)

(defun jdee-custom-variable-set (widget)
  "Set the current value for the variable being edited by WIDGET."
  (let ((symbol (widget-value widget)))
    (custom-variable-set widget)
    (jdee-save-project)
    (add-to-list 'jdee-dirty-variables symbol)))

(defun jdee-custom-variable-reset-standard (widget)
  "Restore the standard setting for the variable being edited by WIDGET.
This operation eliminates any saved setting for the variable,
restoring it to the state of a variable that has never been customized."
  (custom-variable-reset-standard widget)
  (jdee-save-project))

(defvar jdee-custom-variable-menu
  '(("Save in JDEE Project File" jdee-custom-variable-set
     (lambda (widget)
       (and
	(eq (widget-get widget :custom-state) 'modified)
	(string-match "^jdee-" (symbol-name (widget-value widget))))))
    ("Set for Current Session" custom-variable-set
     (lambda (widget)
       (eq (widget-get widget :custom-state) 'modified)))
    ("Save for Future Sessions" custom-variable-save
     (lambda (widget)
       (memq (widget-get widget :custom-state) '(modified set changed rogue))))
    ("Reset to Current" custom-redraw
     (lambda (widget)
       (and (default-boundp (widget-value widget))
	    (memq (widget-get widget :custom-state) '(modified changed)))))
    ("Reset to Saved" custom-variable-reset-saved
     (lambda (widget)
       (and (or (get (widget-value widget) 'saved-value)
		(get (widget-value widget) 'saved-variable-comment))
	    (memq (widget-get widget :custom-state)
		  '(modified set changed rogue)))))
    ("Erase Customization" jdee-custom-variable-reset-standard
     (lambda (widget)
       (and (get (widget-value widget) 'standard-value)
	    (memq (widget-get widget :custom-state)
		  '(modified set changed saved rogue)))))
    ("---" ignore ignore)
    ("Add Comment" custom-comment-show custom-comment-invisible-p)
    ("---" ignore ignore)
    ("Don't show as Lisp expression" custom-variable-edit
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'lisp)))
    ("Show initial Lisp expression" custom-variable-edit-lisp
     (lambda (widget)
       (eq (widget-get widget :custom-form) 'edit))))
  "Alist of actions for the `jdee-custom-variable' widget.
Each entry has the form (NAME ACTION FILTER) where NAME is the name of
the menu entry, ACTION is the function to call on the widget when the
menu is selected, and FILTER is a predicate which takes a `jdee-custom-variable'
widget as an argument, and returns non-nil if ACTION is valid on that
widget.  If FILTER is nil, ACTION is always valid.")


(defun jdee-custom-variable-action (widget &optional event)
  "Show the menu for `jdee-custom-variable' WIDGET.
Optional EVENT is the location for the menu."
  (if (eq (widget-get widget :custom-state) 'hidden)
      (custom-toggle-hide widget)
    (unless (eq (widget-get widget :custom-state) 'modified)
      (custom-variable-state-set widget))
    (custom-redraw-magic widget)
    (let* ((completion-ignore-case t)
	   (answer (widget-choose (concat "Operation on "
					  (custom-unlispify-tag-name
					   (widget-get widget :value)))
				  (custom-menu-filter jdee-custom-variable-menu
						      widget)
				  event)))
      (if answer
	  (funcall answer widget)))))


(define-widget 'jdee-custom-variable 'custom
  "Customize a JDEE variable."
  :format "%v"
  :help-echo "Set or reset this variable."
  :documentation-property 'variable-documentation
  :custom-category 'option
  :custom-state nil
  :custom-menu 'custom-variable-menu-create
  :custom-form nil ; defaults to value of `custom-variable-default-form'
  :value-create 'custom-variable-value-create
  :action 'jdee-custom-variable-action
  :custom-set 'custom-variable-set
  :custom-save 'custom-variable-save
  :custom-reset-current 'custom-redraw
  :custom-reset-saved 'custom-variable-reset-saved
  :custom-reset-standard 'custom-variable-reset-standard)

;;;###autoload
(defalias 'jdee-customize-variable 'jdee-customize-option)

;;;###autoload
(defun jdee-customize-option (symbol)
  "Customize SYMBOL, which must be a JDEE option variable."
  (interactive (custom-variable-prompt))
  (unless (get symbol 'custom-type)
    (error "Variable %s cannot be customized" symbol))
  (custom-buffer-create (list (list symbol 'jdee-custom-variable))
			(format "*Customize Option: %s*"
				(custom-unlispify-tag-name symbol))))


(defun jdee-custom-adjust-group (group)
  (let ((symbol-specs (get group 'custom-group)))
    (dolist (spec symbol-specs)
      (let ((symbol (nth 0 spec))
	    (symbol-type (nth 1 spec)))
	(if (eq symbol-type 'custom-group)
	    (jdee-custom-adjust-group symbol)
	  (setcdr spec (list 'jdee-custom-variable)))))))

(defun jdee-custom-adjust-groups ()
  "Change the symbol type in the symbol spec lists for all
JDEE groups from `custom-variable' to `jdee-custom-variable'.
This causes the save-to-project-file menu item to appear
for JDEE variables in group customization buffers."
  (jdee-custom-adjust-group 'jdee))


(provide 'jdee-custom)

;;; jdee-custom.el ends here
