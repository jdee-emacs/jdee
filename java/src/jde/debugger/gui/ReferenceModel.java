package jde.debugger.gui;

import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import jde.debugger.JDEException;



/** Base class for displaying different types of references */
abstract class ReferenceModel
{

  /**  Default ReferenceModel constructor */
  ReferenceModel( ) {

  } // ReferenceModel constructor

  /** Get the number of children.
   * @return The number of fields that we should be displaying.
   */
  abstract int getChildCount();

  /** Returns if the node allows children */
  abstract boolean getAllowsChildren();

//    * @throws ClassNotLoadedException If the class hasn't been loaded.
//    * This shouldn't be possible, but the compiler doesn't know that.
//    */
  /** Create a child at the given index.
   * @param childIndex index of child to create
   * @return A tree node for the child at the given index
   * @throws JDEException If an error occurs.
   */
  abstract MutableTreeNode createChildAt(int index) throws JDEException;


  /** Get a string to represent the value of the variable */
  abstract String getValue();


  /** Update the values in the children
   * @param children The array of old child values
   */
  abstract void updateChildren(TreeNode[] children);

  /** Convert the TreeNode to an LVTreeNode.
   * @param node The TreeNode to convert
   * @return node cast to an LVTreeNode, or null if node is not an
   * LVTreeNode
   */
  protected static LVTreeNode asLVTreeNode(TreeNode node) {
    if (node instanceof LVTreeNode)
      return (LVTreeNode) node;
    else
      return null;
  }
} // End of class ReferenceModel
