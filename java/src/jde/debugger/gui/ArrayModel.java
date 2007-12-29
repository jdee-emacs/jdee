package jde.debugger.gui;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;

import com.sun.jdi.ArrayReference;
import com.sun.jdi.ArrayType;
import com.sun.jdi.Field;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StringReference;
import com.sun.jdi.Value;
import jde.debugger.JDEException;



/** A TreeNode for array references.
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.1 $
 */
class ArrayModel extends ReferenceModel {
  /**  Constructor
   */
  ArrayModel(ArrayReference value, DefaultTreeModel treeModel)  {
    super();
    if (null == value)
      throw new IllegalArgumentException("Null value in constructor.");
    if (null == treeModel)
      throw new IllegalArgumentException("Null tree model in constructor.");
    m_value = value;
    m_treeModel = treeModel;
  }


  /** Get the number of children.
   * @return The number of fields that we should be displaying.
   */
  int getChildCount() {
    return m_value.length();
  }
  /** Returns if the node allows children */
  boolean getAllowsChildren() { return true; }

  /** Create a child at the given index.
   * @return A tree node for the child at the given index */
  MutableTreeNode createChildAt(int index) throws JDEException {
    String name = getIndexName(index);
    ArrayType arrayType = (ArrayType) m_value.type();
    MutableTreeNode node =
	LVTreeNode.makeTreeNode(name,
				arrayType.componentTypeName(),
				m_value.getValue(index),
				m_treeModel);
    return node;
  }

  /** Get a string to represent the value of the variable */
  String getValue() {
      return m_value.type().name() + "[" + m_value.length() + "]";
  }

  private static String getIndexName(int index) {
    return "[" + index + "]";
  }

  /** Update the values in the children
   * @param children The array of old child values
   */
  void updateChildren(TreeNode[] children) {
    // For the children that already exist, set the new value.
    TreeNode reloadParent = null;
    for (int index = 0;
	 (index < children.length) &&
	   (index < m_value.length());
	 ++index) {
      LVTreeNode child = asLVTreeNode(children[index]);
      if ((null != child) &&
	  (child.getName().equals(getIndexName(index))))
	child.setValue(m_value.getValue(index));
      else {
		// Different node.  Reset it
	if ((null != children[index]) &&
	    (null != children[index].getParent()))
	  reloadParent = children[index].getParent();
	children[index] = null;
      }
    } // for each child

    // Reload the model if we had significant changes
    if (null != reloadParent)
      m_treeModel.reload(reloadParent);
  }

  private final ArrayReference m_value;
  private final DefaultTreeModel m_treeModel;
}
