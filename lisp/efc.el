;;; efc.el -- Emacs Foundation Classes
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: lisp, tools, classes

;; Copyright (C) 2001, 2002, 2003, 2004, 2005 Paul Kinnucan.
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
;; for Emacs.

;;; Code:

(require 'eieio)
(require 'wid-edit)

(defcustom efc-query-options-function 'efc-query-options-function-dialog
  "If non-nil the function to use for interactively querying options.
If nil then the default efc custom-based dialogs will be used."
  :type '(choice :tag "Options Query Function"
		 (const :tag "Dialog" efc-query-options-function-dialog)
		 (const :tag "Mini-Buffer" efc-query-options-function-minibuf)
		 (function :tag "Specify Function"
			   efc-query-options-function-dialog))
  :group 'jde)

(defun efc-query-options-function-dialog (options prompt title history)
  (let ((dialog
	 (efc-option-dialog
	  (or title "option dialog")
	  :text (or prompt "Select option:")
	  :options options)))
    (efc-dialog-show dialog)
    (oref dialog selection)))

(defun efc-query-options-function-minibuf (options prompt title history)
  (let ((default (car options))
	sel)
    ;; efc doesn't add the end colon
    (if prompt
	(setq prompt (format "%s: " prompt)))
    (setq sel (completing-read prompt options nil t nil nil history default))
    (if (= (length sel) 0) (error "Input required"))
    sel))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Dialog Class                                                               ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-dialog ()
  ((title     :initarg :title
	      :type string
	      :initform "Dialog"
	      :documentation
	      "Title of dialog")
   (buf       :initarg :buf
	      :type buffer
	      :documentation
	      "Dialog buffer")
   (initbuf   :initarg :initbuf
	      :type buffer
	      :documentation
	      "Buffer from which dialog was called.")
   )
  "Super class of EFC dialogs."
  )

(defmethod initialize-instance ((this efc-dialog) &rest fields)
  "Constructor for dialog."
  ;; Call parent initializer.
  (call-next-method))


(defmethod efc-dialog-create ((this efc-dialog)))

(defmethod efc-dialog-ok ((this efc-dialog))
  "Invoked when the user clicks the dialog's okay button. The
default method kills the dialog buffer."
  (kill-buffer (current-buffer)))

(defmethod efc-dialog-cancel ((this efc-dialog))
  "Invoked when the user clicks the dialog's Cancel button. The
default method kills the dialog buffer."
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf)))

(defmethod efc-dialog-show ((this efc-dialog))
  (oset this initbuf (current-buffer))

  (oset this buf (get-buffer-create (oref this title)))
  (set-buffer (oref this buf))

  (efc-dialog-create this)

  (widget-put
   (widget-create
    'push-button
    :notify
    (lambda (button &rest ignore) (efc-dialog-ok (widget-get button :dialog)))
    "Ok")
   :dialog this)

  (widget-insert "  ")

  (widget-put
   (widget-create
    'push-button
    :notify (lambda (button &rest ignore) (efc-dialog-cancel (widget-get button :dialog)))
    "Cancel")
   :dialog this)

  (use-local-map widget-keymap)
  (widget-setup)

  ;; Position cursor over the first choice.
  (goto-char (point-min))
  (widget-forward 1)

  (pop-to-buffer (oref this buf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Option Dialog                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silence the compiler warnings
(defun efc-option-dialog (&rest a))

(defclass efc-option-dialog (efc-dialog)
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
		   :documentation
		   "Option chosen by the user."))
   "This dialog allows a user to choose one of a set of OPTIONS by clicking
a radio button next to the option. The dialog sets SELECTION to the option
chosen by the user when the user selects the OK button on the dialog. This
dialog uses recursive edit to emulate a modal dialog.")

(defmethod initialize-instance ((this efc-option-dialog) &rest fields)
  "Dialog constructor."
  (call-next-method))

(defmethod efc-dialog-create ((this efc-option-dialog))
  (widget-insert (oref this text))
  (widget-insert "\n\n")
  (oset this radio-buttons
	(widget-create
	 (list
	  'radio-button-choice
	  :value (car (oref this options))
	  :dialog this
	  :notify (lambda (button &rest ignore)
		    (efc-dialog-ok (widget-get button :dialog)))
	  :args (mapcar
		 (lambda (x) (list 'item x))
		 (oref this options)))))
  (widget-insert "\n"))

(defmethod efc-dialog-show ((this efc-option-dialog))
  "Shows the options dialog buffer. After showing the dialog buffer,
this method invokes recursive-edit to emulate the behavior of a modal
dialog. This suspends the current command until the user has selected
an option or canceled the dialog. See `efc-dialog-ok' and
`efc-dialog-cancel' for more information."
  (save-window-excursion
    (call-next-method)
    (recursive-edit)))

(defmethod efc-dialog-ok ((this efc-option-dialog))
  "Invoked when the user selects the OK button on the options
dialog. Sets the :selection field of THIS to the option chosen by the
user, kills the dialog buffer, and exits recursive-edit mode."
  (oset this
	selection
	(widget-value (oref this radio-buttons)))
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf))
  (exit-recursive-edit))

(defmethod efc-dialog-cancel ((this efc-option-dialog))
  "Invoked when the user clicks the dialog's Cancel button.  Invokes
the default cancel method, sets the :selection field of THIS to nil,
and then exits recursive edit mode."
  (call-next-method)
  (oset this selection nil)
  (exit-recursive-edit))

(defun efc-query-options (options &optional prompt title history)
  "Ask user to choose among a set of options."
  (funcall efc-query-options-function options prompt title history))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Multiple Option Dialog                                                     ;;
;;                                                                            ;;
;; Contributed by Philip Lord.                                                ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-multi-option-dialog (efc-option-dialog)
  ((build-message :initarg :text
		  :type string
		  :initform "Building Dialog"
		  :documentation
		  "Warning message while building dialog, as this can be slow"))
  "Provides a dialog with several sets of OPTIONS.
The dialog sets SELECTION to the options selected by the user.")

(defmethod initialize-instance ((this efc-multi-option-dialog) &rest fields)
   "Dialog constructor."
   (call-next-method))

(defmethod efc-dialog-create ((this efc-multi-option-dialog))
  (message "%s..." (oref this build-message))
  (widget-insert (oref this text))
  (widget-insert "\n\n")
  ;; use radio buttons slot as list of radio buttons rather than.
  (oset this radio-buttons
	(mapcar
	 (lambda(list)
	   (prog1
	       (widget-create
		(list
		 'radio-button-choice
		 :value
		 (efc-multi-option-dialog-default this list)
		 :args (mapcar
			(lambda (x)
			  (list 'item x))
			list)))
	     (widget-insert "\n")))
	 (efc-multi-option-dialog-sort this
				       (oref this options))))
  (widget-insert "\n")
  (message "%s...done" (oref this text)))

(defmethod efc-dialog-ok((this efc-multi-option-dialog))
  ;; set the selection up as a list rather a simple result
  (oset this selection
	(mapcar
	 (lambda(widget)
	   (widget-value widget))
	 (oref this radio-buttons)))
  (delete-window)
  (set-buffer (oref this initbuf))
  (pop-to-buffer (oref this initbuf))
  (kill-buffer (oref this buf))
  (exit-recursive-edit))


(defmethod efc-multi-option-dialog-default ((this efc-multi-option-dialog) list)
  "Pick the default from a collection of options."
  (if (= 1 (length list))
      (car list)))

(defmethod efc-multi-option-dialog-sort ((this efc-multi-option-dialog) list)
  "Sort the options."
  ;; sort the ones with the most options first...
  (sort list
	(lambda(a b)
	  (> (length a)
	     (length b)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Compiler Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-compiler ()
  ((name             :initarg :name
		     :type string
		     :documentation "Compiler name.")
   (buffer           :initarg :buffer
		     :type buffer
		     :documentation
		     "Compilation buffer")
   (window           :initarg :window
		     :type window
		     :documentation
		     "Window that displays the compilation buffer.")
   (exec-path        :initarg :exec-path
		     :type string
		     :documentation "Path of compiler executable.")
   (comp-finish-fcn  :initarg :comp-finish-fcn
		     :type function
		     :documentation "\
A list of function to invoke at end of compilation.  Each
function is called with two arguments: the compilation buffer,
and a string describing how the process finished."))
  "Class of compiler-like applications.")

(defmethod create-buffer ((this efc-compiler))
  "Create a buffer to display the output of a compiler process."
  (save-excursion
    (let ((buf (get-buffer-create (format "*%s*" (oref this name))))
	  (error-regexp-alist compilation-error-regexp-alist)
	  (enter-regexp-alist (if (boundp 'compilation-enter-directory-regexp-alist)
				  compilation-enter-directory-regexp-alist))
	  (leave-regexp-alist (if (boundp 'compilation-leave-directory-regexp-alist)
				  compilation-leave-directory-regexp-alist))
	  (file-regexp-alist (if (boundp 'compilation-file-regexp-alist)
				 compilation-file-regexp-alist))
	  (nomessage-regexp-alist
	   ;; silence the compiler warnings
	   (if (boundp 'compilation-nomessage-regexp-alist)
	       (if (not jde-xemacsp) compilation-nomessage-regexp-alist)))
	  (parser compilation-parse-errors-function)
	  (error-message "No further errors")
	  (thisdir default-directory))

      (oset this :buffer buf)

      (set-buffer buf)

      ;; Make sure a compiler process is not
      ;; already running.
      (let ((compiler-proc (get-buffer-process (current-buffer))))
	(if compiler-proc
	    (if (or (not (eq (process-status compiler-proc) 'run))
		    (yes-or-no-p
			 (format "A %s process is running; kill it?" (oref this name))))
		(condition-case ()
		    (progn
		      (interrupt-process compiler-proc)
		      (sit-for 1)
		      (delete-process compiler-proc))
		  (error nil))
	      (error "Cannot have two processes in `%s' at once"
			 (buffer-name)))))

      ;; In case the compiler buffer is current, make sure we get the global
      ;; values of compilation-error-regexp-alist, etc.
      (kill-all-local-variables)

      ;; Clear out the compilation buffer and make it writable.
      (setq buffer-read-only nil)
      (buffer-disable-undo (current-buffer))
      (erase-buffer)
      (buffer-enable-undo (current-buffer))

      (compilation-mode (oref this name))

      (if (boundp 'compilation-parse-errors-function)
	  (set (make-local-variable 'compilation-parse-errors-function) parser))
      (if (boundp 'compilation-error-message)
	  (set (make-local-variable 'compilation-error-message) error-message))
      (set (make-local-variable 'compilation-error-regexp-alist)
	   error-regexp-alist)

      (when (not (featurep 'xemacs))
	(dolist (elt `((compilation-enter-directory-regexp-alist
			,enter-regexp-alist)
		       (compilation-leave-directory-regexp-alist
			,leave-regexp-alist)
		       (compilation-file-regexp-alist
			,file-regexp-alist)
		       (compilation-nomessage-regexp-alist
			,nomessage-regexp-alist)))
	  (if (boundp (car elt))
	      (set (make-local-variable (car elt)) (second elt)))))

      (if (slot-boundp this 'comp-finish-fcn)
	  (set (make-local-variable 'compilation-finish-functions)
	       (oref this comp-finish-fcn)))

      (if (boundp 'compilation-directory-stack)
	  (setq default-directory thisdir
		compilation-directory-stack (list default-directory))))))

(defmethod get-args ((this efc-compiler))
  "Get a list of command-line arguments to pass to the
compiler process.")


(defmethod exec ((this efc-compiler))
  "Start the compiler process."

  (create-buffer this)

  ;; Pop to checker buffer.
  (let ((outwin (display-buffer (oref this :buffer))))
    (compilation-set-window-height outwin)
    (oset this :window outwin))

  (if (not (featurep 'xemacs))
      (if compilation-process-setup-function
	  (funcall compilation-process-setup-function)))

  (let* ((outbuf (oref this :buffer))
	 (executable-path (oref this exec-path))
	 (args (get-args this)))

    (save-excursion
      (set-buffer outbuf)

      (insert (format "cd %s\n" default-directory))

      (insert (concat
	       executable-path
	       " "
	       (mapconcat 'identity args " ")
	       "\n\n"))

      (let* ((process-environment (cons "EMACS=t" process-environment))
	     (w32-quote-process-args ?\")
	     (win32-quote-process-args ?\") ;; XEmacs
	     (proc (apply 'start-process
			  (downcase mode-name)
			  outbuf
			  executable-path
			  args)))
	(set-process-sentinel proc 'compilation-sentinel)
	(set-process-filter proc 'compilation-filter)
	(set-marker (process-mark proc) (point) outbuf)
	(setq compilation-in-progress
	      (cons proc compilation-in-progress)))

      (set-buffer-modified-p nil)
      (setq compilation-last-buffer (oref this :buffer)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Collection Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-collection ()
  ((elem-type :initarg :elem-type
	      :type (or null symbol)
	      :initform nil
	      :documentation "Type of element that this collection contains."))
  "A collection of objects. The collection can be either homogeneous, i.e.,
composed of elements of one type, or heterogeneous. The ELEM-TYPE property of
a heterogeneous collection is nil.")

(defmethod efc-coll-type-compatible-p ((this efc-collection) item)
  "Returns t if ITEM is type-compatible with this collection. An item is
type-compatible with a collection if the collection is heterogeneous or
the item's type is the same as the collection's element type."
  (let ((element-type (oref this elem-type)))
    (or (eq element-type nil)
	(typep item element-type))))

(defmethod efc-coll-iterator ((this efc-collection))
  "Returns an iterator for this collection."
  (error "Abstract method."))

(defmethod efc-coll-visit ((this efc-collection) visitor)
  "Maps VISITOR to each element of the collection. VISITOR
is an object of efc-visitor class."
  (let ((iter (efc-coll-iterator this)))
    (while (efc-iter-has-next iter)
      (efc-visitor-visit visitor (efc-iter-next iter)))))

(defmethod efc-coll-memberp ((this efc-collection) member)
  "Returns nonil if this contains item."
  (error "Abstract method."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Iterator Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-iterator ()
  ()
  "Iterates over a collection.")

(defmethod efc-iter-has-next ((this efc-iterator))
  "Returns nonnil if the iterator has not returned all of the collection's elements."
  (error "Abstract method."))

(defmethod efc-iter-next ((this efc-iterator))
  "Return the next element of the collection."
  (error "Abstract method."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Visitor Class                                                              ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-visitor ()
  ()
  "Visits each member of a collection.")

(defmethod efc-visitor-visit ((this efc-visitor) member)
  "Visits MEMBER, a member of a collection."
  (error "Abstract method."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; List Class                                                                 ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass efc-list (efc-collection)
  ((items  :initarg :items
	   :type list
	   :initform nil
	   :documentation "List of items."))
  "List of items.")

(defmethod initialize-instance ((this efc-list) &rest fields)
  "Iterator constructor."
  (call-next-method))

(defmethod efc-coll-add ((this efc-list) item)
  "Adds an item to the list."
  (if (efc-coll-type-compatible-p this item)
      (oset this items (append (oref this items) (list item)))
    (error "Tried to add an item of type %s to a list of items of type %s"
	   (type-of item) (oref this elem-type))))

(defmethod efc-coll-iterator ((this efc-list))
  "Return an iterator for this list."
  (efc-list-iterator "list iterator" :list-obj this))


(defmethod efc-coll-memberp ((this efc-list) item)
  "Returns nonil if this list contains item."
  (member item (oref this items)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; List Iterator Class                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; silence the compiler warnings
(defun efc-list-iterator (&rest a))

(defclass efc-list-iterator (efc-iterator)
  ((list-obj :initarg :list-obj
	     :type efc-list
	     :documentation "List that this iterator iterates.")
   (list     :type list
	     :documentation "Lisp list."))
  "Iterates over a list.")

(defmethod initialize-instance ((this efc-list-iterator) &rest fields)
  "Iterator constructor."
  (call-next-method)
  (assert (oref this list-obj))
  (assert (typep (oref this list-obj) efc-list))
  (oset this list (oref (oref this list-obj) items)))

(defmethod efc-iter-has-next ((this efc-list-iterator))
  "Returns true if this iterator has another list item to return."
  (oref this list))

(defmethod efc-iter-next ((this efc-list-iterator))
  "Get next item in the list."
  (let* ((list (oref this list))
	 (next (car list)))
    (oset this list (cdr list))
    next))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; List Set Class                                                             ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-list-set (efc-list)
  ()
  "List that contains no duplicates.")


(defmethod efc-coll-add ((this efc-list-set) item)
  "Adds an item to a set only if the set does not
already contain the item."
  (if (efc-coll-memberp this item)
      (error "This set already contains %s" item)
    (call-next-method)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Association Class                                                          ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-assoc (efc-list)
  ()
  "Association")

(defmethod efc-coll-put ((this efc-assoc) key value)
  "Put an item into the association list."
  (oset this items (append (oref this items) (list (cons key value)))))

(defmethod efc-coll-get ((this efc-assoc) key)
  "Get an item from the association list."
  (cdr (assq  key (oref this items))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Association Set Class                                                      ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-assoc-set (efc-assoc)
  ()
  "Association that contains no duplicate keys.")

(defmethod efc-coll-put ((this efc-assoc-set) key value)
  "Adds an item to a set only if the set does not
already contain the item."
  (if (efc-coll-get this key)
      (error "This set already contains %s" key)
    (call-next-method)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Hash Table Class                                                           ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass efc-hash-table (efc-collection)
  ((table :type hash-table
	  :documentation "Lisp table object."))
  "Hash table.")


(defmethod initialize-instance ((this efc-hash-table) &rest fields)
  "Hash table constructor."
  (call-next-method)
  (oset this table (make-hash-table)))

(defmethod efc-coll-put ((this efc-hash-table) key value)
  "Put an item into the table."
  (if (efc-coll-type-compatible-p this value)
      (puthash key value (oref this table))
    (error "Tried to add an item of type %s to a hash table of items of type %s"
	   (type-of value) (oref this elem-type))))

(defmethod efc-coll-get ((this efc-hash-table) key)
  "Get an item from the table."
  (gethash key (oref this table)))

(defmethod efc-coll-visit ((this efc-hash-table) visitor)
  "Visit each item in the hash table. VISITOR is an instance
of efc-visitor class."
  (maphash
   (lambda (key value)
     (efc-visitor-visit visitor value))
   (oref this table)))

(defmethod efc-coll-iterator ((this efc-hash-table))
  "Return an iterator for this hash table."
  (efc-list-iterator
   "hash table iterator"
   :list-obj (let (values)
	       (maphash
		(lambda (key value)
		  (setq values (append values (list value))))
		(oref this table))
	       values)))

(provide 'efc)

;; End of efc.el
