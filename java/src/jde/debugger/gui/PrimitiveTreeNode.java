package jde.debugger.gui;

import java.util.Enumeration;
import java.util.Vector;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

import com.sun.jdi.PrimitiveType;
import com.sun.jdi.PrimitiveValue;
import com.sun.jdi.Value;
import jde.debugger.JDEException;



/** A TreeNode for primitive variables.
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.2 $
 */
class PrimitiveTreeNode extends LVTreeNode {
  /**  Constructor
   * @param name The name of the variable
   * @param type The Type of the variable
   * @param val The value of the variable.
   * @param model The TreeModel.
   */
  protected PrimitiveTreeNode(String name,
			      String typeName,
			      Value val,
			      DefaultTreeModel model) throws JDEException {
    super(name, typeName, val, model);
    if (!(m_type instanceof PrimitiveType))
      throw new JDEException("PrimitiveTreeNode received non-primitive Type " + m_type.name());
    if (!(val instanceof PrimitiveValue))
      throw new JDEException("PrimitiveTreeNode received non-primitive value " + val.type().name());
    m_value = val;
  }


  /** Get the number of children.
   * @return Zero, since primitives do not have children.
   */
  public int getChildCount() {
    return 0;
  }

  /** Returns if the node allows children
   * @return false, since primitives do not have children. */
  public boolean getAllowsChildren() {
    return false;
  }

  /** Get the child at the given index.
   * @throws IllegalArgumentException since primitives do not have children. */
  public TreeNode getChildAt(int index) {
    throw new IllegalArgumentException("Attempt to create a child of a Primitive");
  }

  /** Get the index of the tree node
   * @return -1 since primitives do not have children */
  public int getIndex(TreeNode node) { return -1; }

  /** Get a string to represent the value of the variable */
  String getValue() {
    return m_value.toString();
  }

  /** Asks if the node is a leaf node */
  public boolean isLeaf() { return true; }

  /** Returns an enumeration of the children */
  public Enumeration children() {
    return new Vector().elements();
  }

  /** Called when the value changes
   * @param val The new value
   */
  void valueChanged(Value val) {
    m_value = val;
  }

  private Value m_value;
}
