package jde.debugger.gui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import com.sun.jdi.ArrayReference;
import com.sun.jdi.ClassNotLoadedException;
import com.sun.jdi.Field;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StringReference;
import com.sun.jdi.Value;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.Protocol;







/** A TreeNode for object references.
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.2 $
 */
class ReferenceTreeNode extends LVTreeNode implements Protocol {
  /**  Constructor
   * @param name The name of the variable
   * @param type The Type of the variable
   * @param val The value of the variable.
   * @param model The tree model
   */
  protected ReferenceTreeNode(String name,
			      String typeName,
			      Value val,
			      DefaultTreeModel model) throws JDEException {
    super(name, typeName, val, model);
    if (!((m_type instanceof ReferenceType) ||
	  (val == null)))
      throw new JDEException("ReferenceTreeNode received non-object reference Type " +
			     m_type.name());
    if ((null != val) &&
	!(val instanceof ObjectReference)) {
      JDE.debug(GUI, "Incorrect Value type in ReferenceTreeNode constructor");
      JDE.debug(GUI, "val=" + val);
      JDE.debug(GUI, "val.type()=" + val.type());
      throw new JDEException("ReferenceTreeNode received non-object reference value " +
			     val.type().name());
    }
  }

  /** Called when the value has changed.  Updates the object which
   * actually implements the public methods.
   * @param oldValue The old value
   * @param newValue THe new value
   */
  protected void valueChanged(Value newValue) {
    // Create the new model for displaying the value
    if (null == newValue)
      m_model = new NullModel();
    else if (newValue instanceof ArrayReference)
      m_model = new ArrayModel((ArrayReference) newValue, getModel());
    else
      m_model = new ObjectModel((ObjectReference) newValue, getModel());

    // If we have children, update the values in them and resize the
    // array
    if (getAllowsChildren()) {
      if (null == m_children)
	m_children = new MutableTreeNode[m_model.getChildCount()];
      else {
	// Ensure that the child array is the correct size
	if (m_children.length != m_model.getChildCount()) {
	  MutableTreeNode[] newChildren = new MutableTreeNode[m_model.getChildCount()];
	  System.arraycopy(m_children, 0,
			   newChildren, 0,
			   Math.min(m_children.length,
				    newChildren.length));
	  m_children = newChildren;
	}
      }
    } else
      m_children = null;

    m_model.updateChildren(m_children);
  }


  /** Get the number of children.
   * @return The number of fields that we should be displaying.
   */
  public int getChildCount() {
    if (null == m_children)
      return 0;
    else
      return m_children.length;
  }

  /** Returns if the node allows children
   * @return true if the value is not null. */
  public boolean getAllowsChildren() {
    return m_model.getAllowsChildren();
  }


  /** Get a string to represent the value of the variable */
  String getValue() {
    return m_model.getValue();
  }




  /**
   * Returns the index of node in the receivers children.
   *
   * @param treeNode a <code>TreeNode</code> value
   * @return the index of node in the receivers children.  Returns -1
   * if <code>treeNode</code> is not a child of this node.
   */
  public int getIndex(TreeNode treeNode) {
    for (int index = 0; index < m_children.length; ++index)
      if (m_children[index] == treeNode)
	return index;
    return -1;
  }

  /**
   * Returns the children of the reciever as an Enumeration.
   *
   * @return the children of the reciever as an Enumeration.
   */
  public Enumeration children() {
    return Collections.enumeration(Arrays.asList(m_children));
  }

  /**
   * Returns the child TreeNode at index childIndex.  The child is
   * created if it doesn't exist.
   *
   * @param childIndex the index of the child
   * @return the child TreeNode at index childIndex.
   */
  public TreeNode getChildAt(int childIndex) {
    try {
      if (null == m_children[childIndex]) {
	m_children[childIndex] = m_model.createChildAt(childIndex);
	m_children[childIndex].setParent(this);
      }
    } catch (JDEException exc) {
      m_children[childIndex] =
	new DefaultMutableTreeNode("Error displaying data: " + exc.getMessage());
    }
    return m_children[childIndex];
  }

  /**
   * Returns true if the receiver is a leaf.
   *
   * @return true if the receiver is a leaf.
   */
  public boolean isLeaf() {
    return (null == m_children) ||
      (m_children.length == 0);
  }



  /**
   * Returns the child TreeNode at index childIndex.  The child is
   * <b>not</b> created if it doesn't exist.
   *
   * @param childIndex the index of the child
   * @return the child TreeNode at index childIndex or null if there
   * is no child at that index yet, or the child is not an LVTreeNode.
   */
  protected LVTreeNode getExistingChildAt(int childIndex) {
    if ((null == m_children[childIndex]) ||
	(!(m_children[childIndex] instanceof LVTreeNode)))
      return null;
    return (LVTreeNode) m_children[childIndex];
  }

  /** Get the current length of the children array. For use when updating values. */
  protected int getExistingChildLength() {
    return m_children.length;
  }

  protected String paramString() {
    return super.paramString() +
      ",m_model=" + m_model +
      ",m_children=" + m_children +
      ((null == m_children)? "": ("[" + m_children.length + "]"));
  }

  private ReferenceModel m_model;
  private MutableTreeNode[] m_children;
}
