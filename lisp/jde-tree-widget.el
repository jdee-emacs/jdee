;;; jde-tree-widget.el --- Tree widget
;; $Id$

;; Copyright (C) 2001, 2004 by David Ponce
;; Copyright (C) 2009 by Paul Landes

;; Author: David Ponce <david@dponce.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Created: 16 Feb 2001
;; Version: 1.0.5
;; Keywords: extensions

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library provide a `tree-widget' useful to display data
;; structures organized in hierarchical order.
;;
;; The following `tree-widget' extra properties are recognized:
;;
;;   :open
;;      Set to non-nil to unfold the tree.  By default the tree is
;;      folded.
;;
;;   :node
;;      The widget used for the tree node.  By default this is an
;;      `item' widget which displays the tree :tag property value if
;;      defined or a string representation of the tree value using the
;;      function `widget-princ-to-string'.
;;
;;   :keep
;;      Specify a list of extra properties to keep when the tree is
;;      folded so they can be recovered when the tree is unfolded.
;;      This property is also honoured in `tree-widget' children.
;;
;;   :dynargs
;;      Specify a function to be called when the tree is unfolded.
;;      This function will receives the tree widget as its argument
;;      and must return a list of children widget definitions.  Thus
;;      dynamlically providing the tree children in response to an
;;      unfold request.  The list of children definitions is kept in
;;      the tree :args property and the :dynargs function can just
;;      return its value when unfolding the tree again.  To force a
;;      new evaluation of the tree content just set its :args property
;;      to nil and redraw the node.
;;
;;   :has-children
;;      Specify if this tree has children.  This property has meaning
;;      only when used with the above :dynargs one.  It indicates that
;;      children widget exist but will be provided when unfolding the
;;      node.
;;
;;   :no-leaf-handle   (default "*---- ")
;;   :close-handle     (default "-- ")
;;   :no-guide         (default "   ")
;;   :open-handle      (default "-, ")
;;   :guide            (default " | ")
;;   :leaf-handle      (default " |--- ")
;;   :last-leaf-handle (default " `--- ")
;;      These properties define the strings used to draw the tree
;;      like the following:
;;
;;      *---- N0        :no-leaf-handle + node
;;
;;      [-]-, N0        node-handle + :open-handle + node
;;          |--- N1     :no-guide + :leaf-handle + node
;;         [-]-, N2     :no-guide + node-handle + :open-handle + node
;;          |  |--- N21 :no-guide + :guide + :leaf-handle + node
;;          |  `--- N22 :no-guide + :guide + :last-leaf-handle + node
;;         [+]-- N3     :no-guide + node-handle + :close-handle + node
;;
;; About leaf node format
;;   To correctly draw the tree, that is insert the current leaf node
;;   prefix, leaf node widgets should prepend the "%p" escape to the
;;   value of the :format property.  And set the :format-handler
;;   property to `tree-widget-format-handler'.  Something like this:
;;
;;   (define-widget 'leaf-node 'item
;;     :format "%p%t\n"
;;     :format-handler #'tree-widget-format-handler)
;;
;; Basic examples of `tree-widget' usage are provided in this file
;; (see commands `tree-widget-example-1' and `tree-widget-example-2').
;; A more sophisticated example is provided in the dir-tree.el
;; source.
;;
;; Installation

;; Put this file on your Emacs-Lisp load path and add following into
;; your ~/.emacs startup file
;;
;;   (require 'tree-widget)

;; Support
;;
;; This program is available at <http://www.dponce.com/>. Any
;; comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to David Ponce <david@dponce.com>.

;;; Code:

(require 'wid-edit)

;;; Customization.

(defgroup tree-widget nil
  "Customization support for the Tree Widget Library."
  :group 'widgets)

(defcustom tree-widget-node-handle-widget 'tree-widget-node-handle
  "Widget type used for tree node handle."
  :type  'symbol
  :group 'tree-widget)

(defun tree-widget-get-super (widget property)
  "Return WIDGET super class PROPERTY value."
  (widget-get
   (get (widget-type
	 (get (widget-type widget) 'widget-type))
	'widget-type)
   property))

(defun tree-widget-p (widget)
  "Return non-nil if WIDGET inherits from a 'tree-widget' widget."
  (let ((type (widget-type widget)))
    (while (and type (not (eq type 'tree-widget)))
      (setq type (widget-type (get type 'widget-type))))
    (eq type 'tree-widget)))

(defun tree-widget-keep (arg widget)
  "Save in ARG the WIDGET properties specified by :keep."
  (let ((plist (widget-get widget :keep))
	prop)
    (while plist
      (setq prop  (car plist)
	    plist (cdr plist))
      (widget-put arg prop (widget-get widget prop)))))

(defun tree-widget-node (widget)
  "Return the tree WIDGET :node value.
If not found setup a default 'item' widget."
  (or (widget-get widget :node)
      ;; Take care of actually return the :node property value.
      ;; Because FSF Emacs `widget-put' returns the property value and
      ;; XEmacs one returns the widget value!!!  So don't use thing
      ;; like this ;-)
      ;; (or (widget-get widget :node)
      ;;     (widget-put widget :node node))
      (let ((node `(item :tag ,(or (widget-get widget :tag)
				   (widget-princ-to-string
				    (widget-value widget))))))
	(widget-put widget :node node)
	node)))

(defun tree-widget-children-value-save (widget &optional args node)
  "Save WIDGET children values.
Children properties and values are saved in ARGS if non-nil else in
WIDGET :args property value.  Data node properties and value are saved
in NODE if non-nil else in WIDGET :node property value."
  (let ((args       (or args (widget-get widget :args)))
	(node       (or node (tree-widget-node widget)))
	(children   (widget-get widget :children))
	(node-child (widget-get widget :tree-widget-node))
	arg child)
    (while (and args children)
      (setq arg      (car args)
	    args     (cdr args)
	    child    (car children)
	    children (cdr children))
      (cond

       ;; The child is a tree node.
       ((tree-widget-p child)

	  ;; Backtrack :args and :node properties.
	(widget-put arg :args (widget-get child :args))
	(widget-put arg :node (tree-widget-node child))

	;; Save :open property.
	(widget-put arg :open (widget-get child :open))

	;; The node is open.
	(if (widget-get child :open)
	    (progn
	      ;; Save the widget value.
	      (widget-put arg :value (widget-value child))
	      ;; Save properties specified in :keep.
	      (tree-widget-keep arg child)
	      ;; Save children.
	      (tree-widget-children-value-save
	       child
	       (widget-get arg :args)
	       (widget-get arg :node)))))

	;; Another non tree node.
	(t
	 ;; Save the widget value
	 (widget-put arg :value (widget-value child))
	 ;; Save properties specified in :keep.
	 (tree-widget-keep arg child))))

    (cond ((and node node-child)
	   ;; Assume that the node child widget is not a tree!
	   ;; Save the node child widget value.
	   (widget-put node :value (widget-value node-child))
	   ;; Save the node child properties specified in :keep.
	   (tree-widget-keep node node-child)))))

(defvar tree-widget-after-toggle-functions nil
  "Hooks run after toggling a `tree-widget' folding.
Each function will receive the `tree-widget' as its unique argument.
This variable should be local to each buffer used to display
widgets.")

(defun tree-widget-toggle-folding (widget &rest ignore)
  "Toggle a `tree-widget' folding.
WIDGET is a `tree-widget-node-handle-widget' and its parent the
`tree-widget' itself.  IGNORE other arguments."
  (let ((parent (widget-get widget :parent))
	(open   (widget-value widget)))
     (if open
	 ;; Before folding the node up, save children values so next
	 ;; open can recover them.
	 (tree-widget-children-value-save parent))
    (widget-put parent :open (not open))
    (widget-value-set parent (not open))
    (run-hook-with-args 'tree-widget-after-toggle-functions parent)))

(defvar tree-widget-button-keymap
  (let (parent-keymap mouse-button1 keymap)
    (if (featurep 'xemacs)
	(setq parent-keymap  widget-button-keymap
	      mouse-button1 [button1])
      (setq parent-keymap  widget-keymap
	    mouse-button1 [down-mouse-1]))
    (setq keymap (copy-keymap parent-keymap))
    (define-key keymap mouse-button1 #'widget-button-click)
    keymap)
  "Keymap used inside node handle buttons.")

(define-widget 'tree-widget-node-handle 'toggle
  "Tree node handle widget."
  :button-keymap  tree-widget-button-keymap ; XEmacs
  :keymap         tree-widget-button-keymap ; Emacs
  :format         "%[%v%]"
  :on             "[+]"
  :off            "[-]"
  :notify         #'tree-widget-toggle-folding)

(define-widget 'tree-widget 'default
  "Tree node widget."
  :format         "%v"
  :convert-widget #'widget-types-convert-widget
  :value-get      #'widget-value-value-get
  :value-create   #'tree-widget-value-create
  :value-delete   #'tree-widget-value-delete

  ;; *---- N          :no-leaf-handle + node

  ;; [-]-, N          node-handle + :open-handle + node
  ;;     |--- N1      :no-guide + :leaf-handle + node
  ;;    [-]-, N2      :no-guide + node-handle + :open-handle + node
  ;;     |  |--- N21  :no-guide + :guide + :leaf-handle + node
  ;;     |  `--- N22  :no-guide + :guide + :last-leaf-handle + node
  ;;    [+]-- N3      :no-guide + node-handle + :close-handle + node

  :no-leaf-handle   "*---- "
  :close-handle     "-- "
  :no-guide         "   "
  :open-handle      "-, "
  :guide            " | "
  :leaf-handle      " |--- "
  :last-leaf-handle " `--- ")

(defun tree-widget-format-handler (widget escape)
  "Convenient %p format handler to insert a leaf node prefix.
WIDGET is a tree leaf node and ESCAPE a format character."
  (cond

   ;; If %p format insert the leaf node prefix.
   ((eq escape ?p)
    (if (widget-get widget :indent)
	(insert-char ?  (widget-get widget :indent)))
    (insert
     (or (widget-get widget :tree-widget-leaf-handle)
	 "")))

   ;; For other ESCAPE values call the WIDGET super class format
   ;; handler.
   (t
    (let ((handler (tree-widget-get-super widget :format-handler)))
      (if handler
	  (funcall handler widget escape))))))

(defun tree-widget-value-delete (widget)
  "Delete tree WIDGET children."
  ;; Delete children
  (widget-children-value-delete widget)
  ;; Delete node child
  (widget-delete (widget-get widget :tree-widget-node))
  (widget-put widget :tree-widget-node nil))

(defun tree-widget-value-create (widget)
  "Create the tree WIDGET children."
  (let ((args (widget-get widget :args))
	(open (widget-get widget :open))
	(node (tree-widget-node widget))
	children buttons prefix)

    (cond

     ;; Leaf node.
     ((not (or args
	       ;; Take care of dynamic tree.  If :has-children is
	       ;; non-nil let a chance to open the node later.  So
	       ;; don't consider it as a leaf node even if it has not
	       ;; (yet) any children.
	       (and (widget-get widget :dynargs)
		    (widget-get widget :has-children))))

      (insert (or (widget-get widget :tree-widget-leaf-handle)
		  (widget-get widget :no-leaf-handle)))
      (widget-put widget :tree-widget-node
		  (widget-create-child-and-convert widget node)))

     ;; Unfolded node.
     (open

      ;; Maybe the tree is dynamic.
      (if (widget-get widget :dynargs)
	  (let ((newargs
		 ;; Request the definition of children.
		 (funcall (widget-get widget :dynargs) widget)))
	    ;; Maybe reuse definition from the :args cache.
	    (or (eq args newargs)
		;; Otherwise setup a new :args cache.
		(widget-put
		 widget :args
		 (setq args (mapcar #'widget-convert newargs))))))

      (setq buttons
	    (cons (widget-create-child-and-convert
		   widget tree-widget-node-handle-widget
		   :value nil :help-echo "Hide node")
		  buttons))
      (insert (widget-get widget (if args
				     :open-handle
				   :close-handle)))
      (widget-put widget :tree-widget-node
		  (widget-create-child-and-convert widget node))
      (setq prefix
	    (concat (or (widget-get widget :tree-widget-prefix) "")
		    (or (widget-get widget :tree-widget-guide)
			(widget-get widget :no-guide))))
      (if (null args)
	  nil
	(while (cdr args)
	  (insert prefix)
	  (setq children
		(cons (widget-create-child-and-convert
		       widget (car args)
		       :tree-widget-prefix prefix
		       :tree-widget-guide (widget-get widget :guide)
		       :tree-widget-leaf-handle
		       (widget-get widget :leaf-handle))
		      children)
		args (cdr args)))
	;; The last non tree child uses the :last-leaf-handle.
	(insert prefix)
	(setq children
	      (cons (widget-create-child-and-convert
		     widget (car args)
		     :tree-widget-prefix prefix
		     :tree-widget-leaf-handle
		     (widget-get widget :last-leaf-handle))
		    children))))

     ;; Folded node.
     (t

      (setq buttons
	    (cons
	     (widget-create-child-and-convert
	      widget tree-widget-node-handle-widget
	      :value t :help-echo "Show node")
	    buttons))
      (insert (widget-get widget :close-handle))
      (widget-put widget :tree-widget-node
		  (widget-create-child-and-convert widget node))))

    (widget-put widget :children (nreverse children))
    (widget-put widget :buttons  buttons)))

;;;;
;;;; Utilities
;;;;

(defun tree-widget-map (widget fun)
  "For each WIDGET displayed child call function FUN.
FUN is called with three arguments like this:

 (FUN CHILD IS-NODE WIDGET)

where:
- - CHILD is the child widget.
- - IS-NODE is non-nil if CHILD is WIDGET node widget."
  (if (widget-get widget :tree-widget-node)
      (let ((children (widget-get widget :children))
	    child)
	(funcall fun (widget-get widget :tree-widget-node)
		 t widget)
	(while children
	  (setq child    (car children)
		children (cdr children))
	  (if (tree-widget-p child)
	      ;; The child is a tree node.
	      (tree-widget-map child fun)
	    ;; Another non tree node.
	    (funcall fun child nil widget))))))

;;;;
;;;; Samples
;;;;

;;; Compatibility

(cond ((featurep 'xemacs)

       (defalias 'tree-widget-sample-overlay-lists
	 (lambda () (list (extent-list))))
       (defalias 'tree-widget-sample-delete-overlay 'delete-extent))

      (t

       (defalias 'tree-widget-sample-overlay-lists 'overlay-lists)
       (defalias 'tree-widget-sample-delete-overlay 'delete-overlay)))

(defun tree-widget-example-1 ()
  "A simple usage of the `tree-widget'."
  (interactive)
  (switch-to-buffer "*`tree-widget' example 1*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (tree-widget-sample-overlay-lists)))
    (mapc #'tree-widget-sample-delete-overlay (car all))
    (mapc #'tree-widget-sample-delete-overlay (cdr all)))

  (widget-insert (format "%s. \n\n" (buffer-name)))

  (widget-create
   ;; Open this level.
   'tree-widget :open t
   ;; Use a push button for this node.
   :node '(push-button
	   :tag "Root"
	   :format "%[%t%]\n"
	   :notify
	   (lambda (&rest ignore)
	     (message "This is the Root node")))
   ;; Add subtrees (their nodes defaut to items).
   '(tree-widget :tag "Child-1")
   '(tree-widget :tag "Child-2"
		 (tree-widget :tag "Child-2.1")
		 (tree-widget :tag "Child-2.2"
			      (tree-widget :tag "Child-2.2.1")
			      (tree-widget :tag "Child-2.2.2")))
   '(tree-widget :tag "Child-3"
		 (tree-widget :tag "Child-3.1")
		 (tree-widget :tag "Child-3.2")))

  (use-local-map widget-keymap)
  (widget-setup))

(defun tree-widget-example-2-dynargs (widget)
  "Return the children definitions of WIDGET.
Reuse the cached :args property value if exists."
  (or (widget-get widget :args)
      '((tree-widget :tag "Child-2.1")
	(tree-widget :tag "Child-2.2"
		     (tree-widget :tag "Child-2.2.1")
		     (tree-widget :tag "Child-2.2.2")))))

(defun tree-widget-example-2 ()
  "A simple usage of the `tree-widget' with dynamic expansion."
  (interactive)
  (switch-to-buffer "*`tree-widget' example 2*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (tree-widget-sample-overlay-lists)))
    (mapc #'tree-widget-sample-delete-overlay (car all))
    (mapc #'tree-widget-sample-delete-overlay (cdr all)))

  (widget-insert (format "%s. \n\n" (buffer-name)))

  (widget-create
   ;; Open this level.
   'tree-widget :open t
   ;; Use a push button for this node.
   :node '(push-button
	   :tag "Root"
	   :format "%[%t%]\n"
	   :notify
	   (lambda (&rest ignore)
	     (message "This is the Root node")))
   ;; Add subtrees (their nodes defaut to items).
   '(tree-widget :tag "Child-1")
   ;; Dynamically retrieve children of this node.
   '(tree-widget :tag "Child-2"
		 :dynargs tree-widget-example-2-dynargs
		 :has-children t)
   '(tree-widget :tag "Child-3"
		 (tree-widget :tag "Child-3.1")
		 (tree-widget :tag "Child-3.2")))

  (use-local-map widget-keymap)
  (widget-setup))

(provide 'tree-widget)

;;; jde-tree-widget.el ends here
