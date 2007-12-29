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

import com.sun.jdi.Field;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.ReferenceType;
import com.sun.jdi.StringReference;
import com.sun.jdi.Value;
import jde.debugger.JDEException;



/** A TreeNode for object references that aren't null or arrays.
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.1 $
 */
class ObjectModel extends ReferenceModel {
  /**  Constructor
   */
  ObjectModel(ObjectReference value, DefaultTreeModel treeModel)  {
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
    return getFields().size();
  }
  /** Returns if the node allows children */
  boolean getAllowsChildren() { return true; }
  /** Create a child at the given index.
   * @return A tree node for the child at the given index */
  MutableTreeNode createChildAt(int index) throws JDEException {
    Field field = getFieldAt(index);
    Value value = m_value.getValue(field);
    MutableTreeNode node =
	LVTreeNode.makeTreeNode(field, value, m_treeModel);
    return node;
  }

  /** Get a string to represent the value of the variable */
  String getValue() {
    if (m_value instanceof StringReference) {
      StringReference string = (StringReference) m_value;
      return '"' + string.value() + '"';
    } else {
      return m_value.toString();
    }
  }

  /** Get the fields that we wish to display */
  private List getFields() {
    ReferenceType refType = (ReferenceType) m_value.type();
    Collection fields = new TreeSet(new Comparator() {
	public int compare(Object o1, Object o2) {
	  Field field1 = (Field) o1;
	  Field field2 = (Field) o2;
	  return field1.name().compareTo(field2.name());
	}
      });
    fields.addAll(refType.visibleFields());
    return new ArrayList(fields);
  }
  /** Get the field that we wish to display
   * @param index The index of the field
   */
  private Field getFieldAt(int index) {
    return (Field) getFields().get(index);
  }

  /** Update the values in the children
   * @param children The array of old child values
   */
  void updateChildren(TreeNode[] children) {
    // For the children that already exist, set the new value.
    //
    // If the field name has changed, then this is an entirely
    // different field and we need to create a new node.
    TreeNode reloadParent = null;
    int index = 0;
    List fields = getFields();
    for (Iterator iter = getFields().iterator();
	 iter.hasNext() && (index < children.length);
	 ++index) {
      Field field = (Field) iter.next();
      LVTreeNode node = asLVTreeNode(children[index]);
      if ((null != node) &&
	  (node.getName().equals(field.name()))) {
	// Match found.  Update the child.
	node.setValue(m_value.getValue(field));
      } else {
	// Different node.  Reset it
	if ((null != children[index]) &&
	    (null != children[index].getParent()))
	  reloadParent = children[index].getParent();
	children[index] = null;
      }
    } // for each field

    // Reload the model if we had significant changes
    if (null != reloadParent)
      m_treeModel.reload(reloadParent);
  }


  public String toString() {
    return getClass().getName() + "[" +
      "m_value=" + m_value + "," +
      "m_treeModel=" + m_treeModel + "]";
  }

  private final ObjectReference m_value;
  private final DefaultTreeModel m_treeModel;
}
