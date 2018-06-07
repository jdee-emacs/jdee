;;; jdee-avl-tree.el --- JDEE wrapper around AVL Tree

;; Maintainer: jdee-devel

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

;;; Code:

(require 'avl-tree)
(require 'cl-macs)
(require 'eieio)

(defclass jdee-avl-tree ()
  ((tree        :initarg tree
                :type avl-tree-
                :documentation
                "The tree")
   (compare-fcn :initarg compare-fcn
                :type function
                ;; :initform <
                :documentation
                "Compare function."))
  "Balanced binary tree.")

(defmethod initialize-instance ((this jdee-avl-tree) &rest fields)
  "Constructor for binary balanced tree."

  ;; Call parent initializer
  (call-next-method)

  (assert (cl-typep (oref this compare-fcn) 'function))

  (oset this tree (avl-tree-create (oref this compare-fcn))))

(defmethod jdee-avl-tree-add ((this jdee-avl-tree) item)
  "Inserts ITEM in this tree."
  (avl-tree-enter (oref this tree) item))

(defmethod jdee-avl-tree-delete ((this jdee-avl-tree) item)
  "Deletes ITEM from THIS tree."
  (avl-tree-delete (oref this tree) item))

(defmethod jdee-avl-tree-is-empty ((this jdee-avl-tree))
  "Return t if THIS tree is empty, otherwise return nil."
  (avl-tree-empty (oref this tree)))

(defmethod jdee-avl-tree-find ((this jdee-avl-tree) item)
  "Return the element in THIS tree that matches item."
  (avl-tree-member (oref this tree) item))

(defmethod jdee-avl-tree-map ((this jdee-avl-tree) map-function)
  "Applies MAP-FUNCTION to all elements of THIS tree."
  (avl-tree-map map-function (oref this tree)))

(defmethod jdee-avl-tree-first ((this jdee-avl-tree))
  "Return the first item in THIS tree."
  (avl-tree-first (oref this tree)))

(defmethod jdee-avl-tree-last ((this jdee-avl-tree))
  "Return the last item in THIS tree."
  (avl-tree-last (oref this tree)))

(defmethod jdee-avl-tree-copy ((this jdee-avl-tree))
  "Return a copy of THIS tree."
  (avl-tree-copy (oref this tree)))

(defmethod jdee-avl-tree-flatten ((this jdee-avl-tree))
  "Return a sorted list containing all elements of THIS tree."
  (avl-tree-flatten (oref this tree)))

(defmethod jdee-avl-tree-size ((this jdee-avl-tree))
  "Return the number of elements in THIS tree."
  (avl-tree-size (oref this tree)))

(defmethod jdee-avl-tree-clear ((this jdee-avl-tree))
  "Delete all elements of THIS tree."
  (avl-tree-clear (oref this tree)))

(provide 'jdee-avl-tree)

;;; jdee-avl-tree.el ends here
