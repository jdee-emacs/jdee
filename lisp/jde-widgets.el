;;; jde-widgets.el -- Custom-style widgets used by the JDE
;; $Id$

;; Author: Paul Kinnucan <paulk@mathworks.com>
;; Maintainer: Paul Landes <landes <at> mailc dt net>
;; Keywords: java, tools

;; Copyright (C) 1997-2004 Paul Kinnucan.
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

(require 'wid-edit)
(require 'eieio)
(require 'efc)
(jde-require 'tree-widget)


;; ----------------------------------------------------------------------
;; The Tree Widget Code:


;;; The `tree' Widget.

(define-widget 'jde-widget-tree-open-button 'item
  "Open node in `jde-tree' widget."
  :button-prefix "["
  :button-suffix "]"
  :tag "+"
  :action 'jde-widget-tree-open-button-callback
  :help-echo "Show subtree."
  :format "%[%t%]")

(defun jde-widget-tree-open-button-callback (widget &optional event)
  ;; Set parent state to open.
  (widget-value-set (widget-get widget :parent) t))

(define-widget 'jde-widget-tree-close-button 'item
  "Close node in `tree' widget."
  :button-prefix "["
  :button-suffix "]"
  :tag "-"
  :action 'jde-widget-tree-close-button-callback
  :help-echo "Hide subtree."
  :format "%[%t%]")

(defun jde-widget-tree-close-button-callback (widget &optional event)
  ;; Set parent state to closed.
  (let* ((parent (widget-get widget :parent))
	(entries (widget-get parent :args))
	(children (widget-get parent :children)))
    (while (and entries children)
      (widget-put (car entries) :value (widget-value (car children)))
      (setq entries (cdr entries)
	    children (cdr children)))
    (widget-value-set parent nil)))

(define-widget 'jde-widget-tree 'default
  "A tree structure widget."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :prefix ""
  :prefix-extra "   "
  :prefix-empty " |--- "
  :value-get 'widget-value-value-get
  :value-create 'jde-widget-tree-value-create-callback
  :value-delete 'widget-children-value-delete)

(defun jde-widget-tree-value-create-callback (widget)
  ;; Insert all values
  (let ((open (widget-value widget))
	(tag (widget-get widget :tag))
	(entries (widget-get widget :args))
	children buttons)
    (cond ((null entries)
	   ;; Empty node.
	   (insert (widget-get widget :prefix-empty) tag "\n"))
	  (open
	   ;; Open node.
	   (push
	    (widget-create-child-and-convert widget 'jde-widget-tree-close-button)
		buttons)
	   (insert "-\\ " tag "\n")
	   (let ((prefix (concat (widget-get widget :prefix)
				(widget-get widget :prefix-extra)))
		entry)
	     (while entries
	       (setq entry (car entries)
		     entries (cdr entries))
	       (insert prefix)
	       (push (if entries
			(widget-create-child-and-convert widget entry
							  :prefix prefix
							  :prefix-extra " | ")
		       ;; Last entry uses a different prefix.
		       (widget-create-child-and-convert
			widget entry
			:prefix prefix
			:prefix-empty " `--- "))
		     children))))
	  (t
	   ;; Closed node.
	   (push (widget-create-child-and-convert widget 'jde-widget-tree-open-button)
		buttons)
	   (insert "-- " tag "\n")))
    (widget-put widget :children children)
    (widget-put widget :buttons buttons)))

;;----------------------------------------------------------------------
;; Eval this to create a small tree.

(defun test-tree ()
  (interactive)
  (switch-to-buffer "*Tree Example*")
  (kill-all-local-variables)
  ;; (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))

  (widget-insert "Test tree widget. \n\n")

;   (setq tree (widget-create 'tree
;			:tag "Foo"
;			'(tree :tag "First")
;			'(tree :tag "Second"
;			       :value nil
;			       (tree :tag "Nested"))
;			'(tree :tag "Third")))

  (setq tree (widget-create 'jde-widget-tree
		  :tag "<test.Foo:139>"
		  '(jde-widget-tree :tag "n  int  0")
		  ;; '(jde-widget-tree :tag '(jde-widget-tree :tag "n  int  0"))
		  '(jde-widget-tree :tag "a  double 5.5")
		  '(jde-widget-tree :tag "s  S      <test.S:145>"
			 (jde-widget-tree :tag "b   boolean  true"))))

;   (let*  ((threads
;	  (list
;	   (list "ThreadGroup" 189 "system"
;		 (list
;		  (list "Thread" 190 "Signal dispatcher" "runnable" "suspended by debugger")
;		  (list "Thread" 191 "Reference Handler" "waiting" "suspended by debugger")
;		  (list "Thread" 192 "Finalizer" "waiting" "suspended by debugger")))
;	   (list "ThreadGroup" 193 "main"
;		 (list
;		  (list "Thread" 1 "main" "runnable" "suspended at breakpoint"))
;		 nil)))
;	 (tree (jde-dbs-map-threads-to-tree threads)))


;  (apply 'widget-create tree))

  (use-local-map widget-keymap)
  (widget-setup))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Dynamic tree widget                                                        ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-widget 'jde-widget-dtree 'default
  "A widget whose nodes are generated on demand.
The first time the user expands the tree, the tree invokes a function that
generates the nodes. The tree then caches the nodes.
Thereafter, the node uses the cached nodes when the
user closes and then reopens the tree. Use the syntax
(widget-create 'jde-widget-dtree :tag NAME :node-fcn NODE-FUNCTION)
to create the widget where NAME is the tree name and NODE-FUNCTION
is a function that takes one argument, the tree itself, and
returns a list of widgets that are the nodes of the expanded
tree."
  :format "%v"
  :prefix-extra "    "
  :value-get 'widget-value-value-get
  :value-create 'jde-widget-dtree-create-callback
  :value-delete 'widget-children-value-delete
  :has-nodes t)

(defun jde-widget-dtree-create-callback (widget)
  (let ((open (widget-value widget))
	(tag (widget-get widget :tag))
	children buttons)
    (cond
     (open
      (push (widget-create-child-and-convert widget 'jde-widget-tree-close-button)
		buttons)
      (insert "-\\ " tag "\n")
      (let ((prefix (concat (widget-get widget :prefix)
				(widget-get widget :prefix-extra)))
	    (nodes (widget-get widget :nodes))
	    node)

	(when (and (widget-get widget :has-nodes)
		   (not nodes))
	  (setq nodes
		(funcall (widget-get widget :node-fcn) widget))
	  (if nodes
	      (widget-put widget :nodes nodes)
	    (widget-put widget :has-nodes nil)))

	(while nodes
	  (setq node (car nodes)
		nodes (cdr nodes))
	  (insert prefix)
	  (push
	   (if nodes
	       (widget-create-child-and-convert widget node
						:prefix prefix
						:prefix-extra " | ")
	     (widget-create-child-and-convert
	      widget node
	      :prefix prefix
	      :prefix-empty " `--- "))
	   children))))
     (t
      ;; Closed node.
      (push (widget-create-child-and-convert widget 'jde-widget-tree-open-button)
	    buttons)
      (insert "-- " tag "\n")))
    (widget-put widget :children children)
    (widget-put widget :buttons buttons)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                            ;;
;; Java object widget                                                         ;;
;;                                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jde-test-get-fields (process object-id)
  (list
   (cons (list "sum" "double") (list "double" 0.0))
   (cons (list "r" "int") (list "int" 1))
   (cons (list "z" "java.lang.String") (list "java.lang.String" 229 nil))
   (cons (list "B" "double[]") (list "double[]" 228 nil))
   (cons (list "A" "double[][]") (list "double[][]" 227 nil))
   (cons (list "args" "java.lang.String[]") (list "java.lang.String[]" 226 nil))))

(defun jde-widget-java-var-to-tree (process var)
  (let* ((var-name (oref var name))
	 (var-type (oref var jtype))
	 (var-value (oref var value))
	 (var-tag (format "%s %s" var-type var-name)))
    (cond
     ((typep var-value 'jde-dbs-java-udci)
      (setq var-tag (format "%s [id: %d]" var-tag (oref var-value :id)))
      (if (string= (oref var-value :jtype) "java.lang.String")
	  (let* ((cmd (jde-dbs-get-string
		       "get string"
		       :process process
		       :object-id (oref var-value id)))
		 (str-val (jde-dbs-cmd-exec cmd)))
	    (list 'tree-widget
		  :tag var-tag
		  :node-name var-tag
		  :open (jde-dbo-locals-open-p var-tag)
		  :value t
		  (list 'tree-widget :tag str-val)))
	(list 'jde-widget-java-obj
	      :tag var-tag
	      :node-name var-tag
	      :open (jde-dbo-locals-open-p var-tag)
	      :process process
	      :object-id (oref var-value :id))))
     ((typep var-value 'jde-dbs-java-array)
      (setq var-tag (format "%s [id: %d]" var-tag (oref var-value :id)))
      (list 'jde-widget-java-array
	    :tag var-tag
	    :node-name var-tag
	    :open (jde-dbo-locals-open-p var-tag)
	    :process process :object var-value))
     ((typep var-value 'jde-dbs-java-primitive)
      (list 'tree-widget
	    :tag var-tag
	    :node-name var-tag
	    :open (jde-dbo-locals-open-p var-tag)
	    :value t
	    (list 'tree-widget
		  :tag (format "%s" (oref var-value value)))))
     ((typep var-value 'jde-dbs-java-null)
      (list 'tree-widget
	    :tag var-tag
	    :node-name var-tag
	    :open (jde-dbo-locals-open-p var-tag)
	    :value t
	    (list 'tree-widget :tag "null")))
     (t
      (error "Unidentified type of local variable: %s" var-tag)))))

(defun jde-widget-java-obj-get-fields (obj-widget)
  (if (widget-get obj-widget :args)
      (widget-get obj-widget :args)
    (let* ((process (widget-get obj-widget :process))
	   (object-id (widget-get obj-widget :object-id))
	   (cmd
	    (jde-dbs-get-object
	     (format "get_object %d" object-id)
	     :process process
	     :object-id object-id))
	   (object
	    (jde-dbs-cmd-exec cmd))
	   (fields (oref object fields))
	   field
	   nodes)
      (while fields
	(setq field (car fields) fields (cdr fields))
	(setq field (cdr field))
	(push
	 (jde-widget-java-var-to-tree process field)
	 nodes))
      nodes)))

(define-widget 'jde-widget-java-obj 'tree-widget
  "A widget that represents a Java object.
This widget is essentially a tree node whose entries are the fields
of the corresponding object. The first time the user expands the node,
the node retrieves the fields of the object from the debugger and
caches them. Thereafter, the node uses the cached values when the
user closes and then reopens the node. Use the syntax
(widget-create 'jde-widget-java-obj
:tag NAME :process PROCESS :object-id OBJ-ID) to create the widget where
NAME is the object's name, PROCESS is the process in which
the object exists, and  ID is the debugger id for the object."
  :dynargs 'jde-widget-java-obj-get-fields
  :has-children t)


(defun jde-widget-java-array-element-to-tree (process element index)
  (cond
     ((typep element 'jde-dbs-java-udci)
      (if (string= (oref element :jtype) "java.lang.String")
	  (let* ((cmd (jde-dbs-get-string
		       "get string"
		       :process process
		       :object-id (oref element id)))
		 (str-val (jde-dbs-cmd-exec cmd)))
	    (list 'tree-widget
		  :tag (format "[%d] %s" index str-val)
		  :node-name (format "[%d] %s" index str-val)
		  :open (jde-dbo-locals-open-p (format "[%d] %s" index str-val))
		  ))
	(list 'jde-widget-java-obj
	      :tag (format "[%d] %s" index (oref element jtype))
	      :node-name (format "[%d] %s" index (oref element jtype))
	      :open (jde-dbo-locals-open-p (format "[%d] %s" index (oref element jtype)))
	      :process process
	      :object-id (oref element id))))
     ((typep element 'jde-dbs-java-array)
      (list 'jde-widget-java-array
	    :tag (format "[%d] %s" index (oref element jtype))
	    :node-name (format "[%d] %s" index (oref element jtype))
	    :open (jde-dbo-locals-open-p (format "[%d] %s" index (oref element jtype)))
	    :process process
	    :object element))
     ((typep element 'jde-dbs-java-primitive)
      (list 'tree-widget :tag (format "[%d] %s"  index (oref element value))))
     ((typep element 'jde-dbs-java-null)
      (list 'tree-widget :tag (format "[%d] null" index)))
     (t
      (error "Unidentified type of object: <%s|%s>" (oref element jtype)
	     (oref element id)))))

(defun jde-widget-java-array-get-elements (array-widget)
  (if (widget-get array-widget :args)
      (widget-get array-widget :args)
    (let* ((process (widget-get array-widget :process))
	   (array (widget-get array-widget :object))
	   cmd array-length)

      (setq cmd
	    (jde-dbs-get-array
	     (format "get_array_length %d" (oref array id))
	     :process process
	     :array array))
      (jde-dbs-cmd-exec cmd)

      (setq array-length
	    (if (slot-boundp array 'length)
		(oref array length)
	      0))

      (when (> array-length 0)
	(setq cmd
	      (jde-dbs-get-array
	       (format "get_array_elements %d" (oref array id))
	       :process process
	       :array array
	       :index 0
	       :length array-length))
	(jde-dbs-cmd-exec cmd)
	(let ((elements (oref array elements))
	      element
	      nodes
	      (index 0))
	  (while elements
	    (setq element (car elements) elements (cdr elements))
	    (setq nodes
		  (append nodes
			  (list (jde-widget-java-array-element-to-tree process element index))))
	    (setq index (1+ index)))
	  nodes)))))

(define-widget 'jde-widget-java-array 'tree-widget
  "A widget that represents a Java array. Clicking on the widget's
expand button causes the widget to display the values of the array."
  :dynargs 'jde-widget-java-array-get-elements
  :has-children t)

(defun test-obj ()
  (interactive)
  (switch-to-buffer "*Java Object Example*")
  (kill-all-local-variables)
  ;; (make-local-variable 'widget-example-repeat)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))

  (widget-insert "Test object tree. \n\n")

  (widget-create 'jde-widget-java-obj :tag "jmath.System s"   :process "process" :object-id 1)
  (widget-create 'jde-widget-java-obj :tag "java.awt.Frame frame1" :process "process" :object-id 1)

  (use-local-map widget-keymap)
  (widget-setup))



;; ----------------------------------------------------------------------
;; Option Tree Widget

(defun jde-widget-option-tree-open-button-callback (widget &optional event)
  ;; Set parent state to open.
  (widget-value-set (widget-get widget :parent) t)
  (widget-setup))

(define-widget 'jde-widget-option-tree-open-button 'item
  "Button to open an option tree."
  :button-prefix "["
  :button-suffix "]"
  :tag "+"
  :action 'jde-widget-option-tree-open-button-callback
  :help-echo "Show option tree."
  :format "%[%t%]")

(define-widget 'jde-widget-option-tree-close-button 'item
  "Close node in `jde-widget-option-tree' widget."
  :button-prefix "["
  :button-suffix "]"
  :tag "-"
  :action 'jde-widget-option-tree-close-button-callback
  :help-echo "Hide panel."
  :format "%[%t%]")

(defun jde-widget-option-tree-close-button-callback (widget &optional event)
  ;; Set parent state to closed.
  (let* ((parent (widget-get widget :parent))
	(entries (widget-get parent :args))
	(group (car (widget-get parent :children)))
	(children (widget-get group :children)))
    ;; Get values entered by user from children and
    ;; insert them in the corresponding widget definitions
    ;; so that they appear the next time the user expands
    ;; the tree.
    (while (and entries children)
      (widget-put (car entries) :value (widget-value (car children)))
      (setq entries (cdr entries)
	    children (cdr children)))
    (widget-value-set parent nil)))

(defun jde-widget-option-tree-value-create-callback (widget)
  (let ((open-widget-p (widget-value widget))
	(tag (widget-get widget :tag))
	(entries (widget-get widget :args))
	entry children buttons)
    (cond (open-widget-p
	   ;; Wrap widgets in this tree in a group widget
	   ;; to ensure proper formatting.
	   (let ((group-type
		  (list 'group :args entries))
		 group-widget)
	   (push
	    (widget-create-child-and-convert
	     widget
	     'jde-widget-option-tree-close-button)
		buttons)
	   (insert "-\\ " tag "\n")
	   (push (widget-create-child-and-convert widget group-type) children)))
	  (t
	   (push (widget-create 'jde-widget-option-tree-open-button
			  :parent widget)
		 buttons)
	   (insert "-- " tag "\n")))
    (widget-put widget :children children)
    (widget-put widget :buttons buttons)))


(define-widget 'jde-widget-option-tree 'default
  "A panel containing widgets."
  :convert-widget 'widget-types-convert-widget
  :format "%v"
  :value-get 'widget-value-value-get
  :value-create 'jde-widget-option-tree-value-create-callback
  :value-delete 'widget-children-value-delete)


(defun test-option-tree ()
  (interactive)
  (switch-to-buffer "*Panel Example*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (overlay-lists)))
    (mapc 'delete-overlay (car all))
    (mapc 'delete-overlay (cdr all)))

  (widget-insert "Test panel widget. \n\n")

  (let ((panel (widget-create
		'jde-widget-option-tree
		:tag "Compile Options"
		'(cons :tag "Debugger Options"
		   (radio-button-choice :format "%t \n%v"
					:tag "Debugger "
					(const "JDEbug")
					(const "jdb")
					(const "oldjdb")
					(const "Other"))
		   (cons :tag "Other Debugger Info"
			 (string :tag "Path")
			 (radio-button-choice :format "%t \n%v"
					      :tag "Type "
					      (const "Executable")
					      (const "Class"))))
		  '(repeat (string :tag "Path"))
		  '(editable-field :tag "classpath"
				  :format "  %t:  %v\n  %h \n\n"
				  :size 40
				  :doc "Name of project.")
		  '(editable-field :tag "compiler"
				  :format "  %t:  %v\n  %h \n\n"
				  :size 40
				  :doc "Name of project.")
		  '(jde-widget-option-tree :tag "Debugger Options"
					   (repeat (string :tag "Path"))))))
    (use-local-map widget-keymap)
    (widget-setup)))

(provide 'jde-widgets)

;; End of jde-widgets.el
