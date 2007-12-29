package jde.debugger.gui;

import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import jde.debugger.JDEException;



/** A TreeNode for null object references.
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.1 $
 */
class NullModel extends ReferenceModel {
  /**  Constructor
   */
  NullModel() {
    super();
  }


  /** Get the number of children.
   * @return The number of fields that we should be displaying.
   */
  public int getChildCount() { return 0; }

  /** Returns if the node allows children */
  public boolean getAllowsChildren() { return false; }

  /** Create a child at the given index.
   * @return A tree node for the child at the given index */
  MutableTreeNode createChildAt(int index) throws JDEException {
    throw new JDEException(getClass().getName() + " does not allow children");
  }

  /** Get a string to represent the value of the variable */
  String getValue() { return "null"; }


  /** Update the values in the children
   * @param children The array of old child values
   */
  void updateChildren(TreeNode[] children) {}
}
