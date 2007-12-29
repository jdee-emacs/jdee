package jde.debugger.gui;

import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import com.sun.jdi.ArrayReference;
import com.sun.jdi.ClassNotLoadedException;
import com.sun.jdi.Field;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.PrimitiveValue;
import com.sun.jdi.Type;
import com.sun.jdi.Value;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.Protocol;




/** A TreeNode for displaying local variables.  This class has no
 * public constructors.  Instead, it uses a factory method { @link
 * #makeTreeNode } to create objects.  This will probably return a
 * subclass based on the type of variable we are displaying.<p>
 *
 * This class implements MutableTreeNode mainly because the
 * DefaultTreeModel requires it.  Most of the methods in that
 * interface simply throw exceptions.
 *
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.2 $
 */
abstract class LVTreeNode implements MutableTreeNode, Protocol {

  /** LVTreeNode constructor for subclasses.
   * @param localVar A reference to a local variable.
   * @param val The value of localVar.
   */
  protected LVTreeNode(String name,
		       String typeName,
		       Value val,
		       DefaultTreeModel model) {
    m_name = name;
    m_typeName = typeName;
    m_model = model;
    setValue(val, false);
  }


  /** Create a new LVTreeNode object.  Based on the arguments,
   * Different classes will be returned, customized to actual
   *  values.
   * @param name The name of the variable
   * @param type The Type of the variable
   * @param val The value of the variable.
   * @param model The tree model.
   * @return A TreeNode for use in a JTree.
   */
  public static MutableTreeNode makeTreeNode(String name,
					     String typeName,
					     Value val,
					     DefaultTreeModel model)
    throws JDEException
  {
    if (val instanceof PrimitiveValue)
      return new PrimitiveTreeNode(name, typeName, val, model);
    else if ((val instanceof ObjectReference) ||
	     (val == null))
      return new ReferenceTreeNode(name, typeName, val, model);
    else
      throw new JDEException("Unknown variable type " + val);
  }

  /** Create a new LVTreeNode object.  Based on the arguments,
   * different classes will be returned, customized to actual
   *  values.
   * @param localVar A reference to a local variable.
   * @param val The value of localVar.
   * @param model The tree model.
   * @return A TreeNode for use in a JTree.
   */
  public static MutableTreeNode makeTreeNode(LocalVariable localVar,
					     Value val,
					     DefaultTreeModel model)
    throws JDEException
  {
//     it appears that: localVar.type() returns a ClassType about the
//     class, ObjectReference.referenceType returns the type of the
//     value, which is what we want to use.

    return makeTreeNode(localVar.name(),
			localVar.typeName(),
			val,
			model);
  }

  /** Create a new LVTreeNode object.  Based on the arguments,
   * different classes will be returned, customized to actual
   *  values.
   * @param field A field within an object
   * @param val The value of the field.
   * @return A TreeNode for use in a JTree.
   */
  public static MutableTreeNode makeTreeNode(Field field,
					     Value val,
					     DefaultTreeModel model)
    throws JDEException
  {
    return makeTreeNode(field.name(),
			field.typeName(),
			val,
			model);
  }

  // Implementation of javax.swing.tree.TreeNode

  /** Get the parent of this node */
  public TreeNode getParent() {
    return m_parent;
  }

  /** Set the parent of this node */
  public void setParent(MutableTreeNode parent) {
    m_parent = parent;
  }


  /**
   * Describe <code>remove</code> method here.
   *
   * @param n an <code>int</code> value
   */
  public void remove(int n) {
    throw new IllegalArgumentException("Attempt to remove a node " +
				       "from the local variables tree");
  }

  /**
   * Describe <code>remove</code> method here.
   *
   * @param mutableTreeNode a <code>MutableTreeNode</code> value
   */
  public void remove(MutableTreeNode mutableTreeNode) {
    throw new IllegalArgumentException("Attempt to remove a node " +
				       "from the local variables tree");
  }

  /**
   * Describe <code>insert</code> method here.
   *
   * @param mutableTreeNode a <code>MutableTreeNode</code> value
   * @param n an <code>int</code> value
   */
  public void insert(MutableTreeNode mutableTreeNode, int n) {
    throw new IllegalArgumentException("Attempt to insert a node " +
				       "from the local variables tree");
  }


  /**
   * Describe <code>setUserObject</code> method here.
   *
   * @param object an <code>Object</code> value
   */
  public void setUserObject(Object object) {

  }



  /**
   * Describe <code>removeFromParent</code> method here.
   *
   */
  public void removeFromParent() {
    throw new IllegalArgumentException("Attempt to remove a node from its parent " +
				       "in the local variables tree");
  }

  /** Get the name of the variable */
  String getName() {
    return m_name;
  }

  /** Get the name of the type of the variable */
  String getTypeName() {
    return m_typeName;
  }

  /** Set the value of the object.
   * @param val The new value
   */
  public void setValue(Value val) {
    setValue(val, true);
  }

  /** Set the value of the object.<p>
   *
   * In the constructor, we don't want to notify the model, since that
   * can cause an infinite loop if the object refers to itself.  Also,
   * notification is intended for changes to existing nodes.
   *
   * @param val The new value
   * @param notify Should we notify the tree model
   */
  private void setValue(Value val, boolean notify) {
    int oldNumChildren = getChildCount();

    // Store the value and the type of the value
    if (val != null)
      m_type = val.type();
    else
      m_type = null;

    valueChanged(val);

    // Tell the TreeModel that we changed.
    if (notify)
      handleChildChange(oldNumChildren);
  }

  /** Called when the value has changed.  Default implementation does nothing.
   * @param oldValue The old value
   * @param newValue THe new value
   */
  abstract void valueChanged(Value newValue);

  private void handleChildChange(int oldNumChildren) {
    int numChildren = getChildCount();
    JDE.debug(GUI, "handleChildChange: old=" + oldNumChildren +
	      ",new=" + numChildren + ",name=" + getName());
    if (numChildren == oldNumChildren) {
      m_model.nodeChanged(this);
      JDE.debug(GUI, "nodeChanged(" + getName() + ")");
    } else if (numChildren > oldNumChildren) {
//     } else if (numChildren > oldNumChildren) {
      int added[] = new int[numChildren - oldNumChildren];
      for (int index = 0; index < added.length; ++index)
	added[index] = oldNumChildren + index;
      m_model.nodesWereInserted(this, added);
      JDE.debug(GUI, "nodesWereInserted(" + getName() + ")");
    } else {
      m_model.nodeStructureChanged(this);
//       m_model.nodeChanged(this);
//       int removed[] = new int[oldNumChildren - numChildren];
//       for (int index = 0; index < removed.length; ++index)
// 	removed[index] = numChildren + index;
//       m_model.nodesWereRemoved(this, removed);
      JDE.debug(GUI, "nodeStructureChanged(" + getName() + ")");
    }
  }

  /** Get the string to display the value of the variable */
  abstract String getValue();

  /** Get the TreeModel for this Node */
  protected DefaultTreeModel getModel() {
    return m_model;
  }

  public final String toString() {
      return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "m_name=" + m_name + "," +
      "m_typeName=" + m_typeName + "," +
      "m_type=" + m_type + "," +
      "m_model=" + m_model + "," +
      "m_parent=" + m_parent;
  }

  protected final String m_name;
  protected final String m_typeName;
  protected Type m_type;

  private DefaultTreeModel m_model;
  private TreeNode m_parent;
}
