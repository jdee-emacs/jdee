package jde.debugger.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.InconsistentDebugInfoException;
import com.sun.jdi.InvalidStackFrameException;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.NativeMethodException;
import com.sun.jdi.ObjectCollectedException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadReference;
import jde.debugger.CommandEvent;
import jde.debugger.CommandListener;
import jde.debugger.Debugger;
import jde.debugger.Etc;
import jde.debugger.EventSetEvent;
import jde.debugger.EventSetListener;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.Protocol;
import java.util.Iterator;
import com.sun.jdi.Value;


/** A widget that displays the local variables
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.2
 * @version $Revision: 1.3 $
 */
class LocalVariableDisplay extends JPanel implements Protocol
{

  /**  Default LocalVariableDisplay constructor */
  public LocalVariableDisplay(Debugger debugger ) {
    super(new BorderLayout());

    m_root = new DefaultMutableTreeNode();
    m_tree = createJTree(m_root);
    m_tree.setRootVisible(false);
    m_debugger = debugger;

    // These are the commands that require us to do something
    m_interestingCommands = new HashSet();
    m_interestingCommands.add("stack_frame");

    JScrollPane scroll =
      new JScrollPane(m_tree,
		      ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
		      ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    add(scroll, BorderLayout.CENTER);

    m_message = new JLabel();
    m_defaultLabelColor = m_message.getForeground();
    add(m_message, BorderLayout.SOUTH);


    // Add listeners for events we're interested in
    debugger.addEventSetListener(new EventSetListener() {
	public void eventSetReceived(EventSetEvent evt) {}
	public void debuggerSuspended(EventSetEvent evt) {
	  updateVariables(evt);
	}
	public void debuggerResumed(EventSetEvent evt) {}
      });
    debugger.addCommandListener(new CommandListener() {
	public void commandReceived(CommandEvent evt) {
	  updateVariables(evt);
	}
      });

    JDE.debug(GUI, "LocalVariableDisplay registered EventSetListener.");

  } // LocalVariableDisplay.java constructor

  /** Update the display the local variables in the tree */
  private void updateVariables(EventSetEvent evt) {
    JDE.debug(GUI, "updateVariables()");

    try {
      p_message("");

      // Get the thread.  If there is none, we can't do anything.
      ThreadReference tRef = evt.getThreadReference();
      if (null == tRef) {
	throw new JDEException("No thread with event");
      }

      updateVariables(tRef, 0);

    } catch (JDEException exc) {
      p_error(exc.getMessage());
    }
  }

  /** Update the display the local variables in the tree in response
   * to a set-frame-index command. */
  private void updateVariables(CommandEvent evt) {
    JDE.debug(GUI, "updateVariables(" + evt.getCmdName() + ")");

    try {
      p_message("");

      // Verify that this is a command we have interest
      if (!m_interestingCommands.contains(evt.getCmdName()))
	return;

      // Check argument count
      if (evt.getArgs().size() != 2)
	throw new JDEException("Incorrect number of arguments: " + evt);


      // Get the thread.  If there is none, we can't do anything.
      Long uniqueID   = Etc.safeGetLong(evt.getArgs().get(0), "thread ID");
      int  frameIndex = Etc.safeGetint(evt.getArgs().get(1), "frame index");

      Object oRef = m_debugger.getStore().get(uniqueID);
      if (oRef == null) {
        throw new JDEException("No such thread exists");
      } else if (!(oRef instanceof ThreadReference)) {
        throw new JDEException("Object is not a thread");
      }
      ThreadReference tRef = (ThreadReference)oRef;

      updateVariables(tRef, frameIndex);
    } catch (JDEException exc) {
      p_error(exc.getMessage());
    }
  }

  /** Update the local variable display
   * @param tRef The thread to display
   * @param frameIndex The index in the stack frame for showing the variables
   */
  private void updateVariables(ThreadReference tRef,
			       int frameIndex) throws JDEException {
      // If the the thread isn't suspended, we can't do anything
      if (!tRef.isSuspended()) {
	throw new JDEException("Thread " + tRef + " not suspended");
      }

      StackFrame frame = null;
      try {
        frame = tRef.frame(frameIndex);
      } catch (IncompatibleThreadStateException ex) {
        throw new JDEException("Thread is not suspended");
      } catch (IndexOutOfBoundsException ex) {
        throw new JDEException("Invalid stack frame");
      } catch (ObjectCollectedException ex) {
        throw new JDEException("The frame has already been garbage collected");
      }

      if (frame == null) {
        throw new JDEException("Error ascertaining frame");
      }

      try {


	// Get this and local variables
	final ObjectReference thisRef  = frame.thisObject();
	final List visibleVariables    = frame.visibleVariables();
	final Map  localVariableValues = frame.getValues(visibleVariables);

	// Update the variables.  Do this on the swing thread since it
	// will change the GUI.
	SwingUtilities.invokeLater(new Runnable() {
	    public void run() {
	      // XXX Do we want to do invokeAndWait so that this thread
	      // block?  Might that cause a deadlock? Troy
	      updateVariablesOnSwingThread(thisRef,
					   visibleVariables,
					   localVariableValues);
	    }
	  });

      } catch (AbsentInformationException ex) {
        throw new JDEException("Local variable information not available: compile with -g");
      } catch (NativeMethodException ex) {
        throw new JDEException("Can't access local variables " +
			       "in native methods");
      } catch (InvalidStackFrameException ex) {
	throw new JDEException("Stack frame no longer valid");
      } catch (InconsistentDebugInfoException ex) {
	throw new JDEException("Inconsistent debug information: " +
			       ex.getMessage());
      }
  }

  /** Update the variable values on the swing thread */
  private void updateVariablesOnSwingThread(ObjectReference thisRef,
					    List visibleVariables,
					    Map localVariableValues) {
    try {
      boolean reload = false;
      DefaultTreeModel model = (DefaultTreeModel) m_tree.getModel();
      Collection updated = new HashSet();
      int previousSize = m_root.getChildCount();
      reload = previousSize == 0;

      // Update or set this
      try {
	if (null != thisRef) {
	  LVTreeNode thisNode = getThisNode();
	  String thisType = thisRef.referenceType().name();
	  if ((null == thisNode) ||
	      !(thisNode.getTypeName().equals(thisType))) {
	    MutableTreeNode node =
	      LVTreeNode.makeTreeNode("this",
				      thisType,
				      thisRef,
				      model);
	    JDE.debug(GUI, "made this: isLeaf=" + node.isLeaf() +
		      ", childCount=" + node.getChildCount() +
		      "\n\tnode=" + node);
	    model.insertNodeInto(node, m_root, 0);
	    ++previousSize;
	  } else {
	    thisNode.setValue(thisRef);
	  }
	  updated.add(new Integer(0));
	}

	// Add the local variables
	for (Iterator iter =
	       localVariableValues.entrySet().iterator();
	     iter.hasNext();
	     /* */) {
	  Map.Entry entry = (Map.Entry) iter.next();
	  LocalVariable localVariable = (LocalVariable) entry.getKey();
	  Value value = (Value) entry.getValue();
	  LVTreeNode node = getVariableNode(localVariable);
	  if (null == node) {
	    MutableTreeNode newNode =
	      LVTreeNode.makeTreeNode(localVariable,
				      value,
				      model);
	    model.insertNodeInto(newNode,
				 m_root,
				 m_root.getChildCount());
	  }else {
	    node.setValue(value);
	    updated.add(new Integer(m_root.getIndex(node)));
	  }
	} // for each local variable

	JDE.debug(GUI, "added values to tree, updated=" + updated);

	// Remove the nodes which no longer correspond to a value.  Do
	// this backwards so that the indices we care about don't
	// shift.
	for (int index = previousSize - 1; index >= 0; --index)
	  if (!updated.contains(new Integer(index))) {
	    MutableTreeNode mutNode =
	      (MutableTreeNode) m_root.getChildAt(index);
	    model.removeNodeFromParent(mutNode);
	  }
      } catch (InvalidStackFrameException ex) {
	throw new JDEException("Stack frame no longer valid");
      } catch (InconsistentDebugInfoException ex) {
	throw new JDEException("Inconsistent debug information: " +
			       ex.getMessage());
      } finally {
	// Notify the tree that it needs to reload the model
	if (reload)
	  model.reload();
      }
    } catch (JDEException exc) {
      p_error(exc.getMessage());
    }
  }


  /** Send an error message to the user */
  private void p_error(final String msg) {
    SwingUtilities.invokeLater(new Runnable() {
	public void run() {
	  m_message.setText(msg);
	  m_message.setForeground(Color.red);
	}
      });
  }

  /** Send a message to the user */
  private void p_message(final String msg) {
    SwingUtilities.invokeLater(new Runnable() {
	public void run() {
	  m_message.setText(msg);
	  m_message.setForeground(m_defaultLabelColor);
	}
      });
  }

  /** Get the TreeNode that has the information for "this".
   * @return The LVTreeNode for this, or null if there is no node
   * displaying that data */
  private LVTreeNode getThisNode() {
    // It will be the first node
    if (m_root.getChildCount() == 0)
      return null;

    // It will be an LVTreeNode
    Object node = m_root.getChildAt(0);
    if (!(node instanceof LVTreeNode))
      return null;

    // It will be named "this"
    LVTreeNode thisNode = (LVTreeNode) node;
    if (thisNode.getName() != "this")
      return null;

    // Found it!
    return thisNode;
  }

  /** Get the LVTreeNode corresponding to the local variable
   * @return The LVTreeNode or null if there is no node for this variable.
   */
  private LVTreeNode getVariableNode(LocalVariable local) {
//     JDE.debug(GUI, "getVariableNode(" + local.name() + "): m_root.size=" +
// 	      m_root.getChildCount());
    for (int index = 0; index < m_root.getChildCount(); ++index) {
      // It will be an LVTreeNode
      Object node = m_root.getChildAt(index);
      if (!(node instanceof LVTreeNode))
	continue;

      // It will have the same name.  XXX should we equals() instead?
      // That requires us to store the old LocalVariable, and not all
      // LVTreeNodes will have a LocalVariable.  (Some are this or a
      // field).
      LVTreeNode varNode = (LVTreeNode) node;
      if (!varNode.getName().equals(local.name()))
	continue;

      // Found it!
      return varNode;
    } // for each child
    return null;

  }

  /** Create a JTree that properly renders LVTreeNodes. */
  private JTree createJTree(DefaultMutableTreeNode root) {
    // Override convertValueToText
      JTree tree = new JTree(root) {
	public String convertValueToText(Object value,
					 boolean selected,
					 boolean expanded,
					 boolean leaf,
					 int row,
					 boolean hasFocus) {
	  if (value instanceof LVTreeNode) {
	    LVTreeNode node = (LVTreeNode) value;
	    return node.getName() +
	      " (" + node.getTypeName() + ") " +
	      node.getValue();
	  } else
	    return super.convertValueToText(value,
					    selected,
					    expanded,
					    leaf,
					    row,
					    hasFocus);
	}
    };
    tree.setLargeModel(true);
    return tree;
  }

  private final JTree m_tree;
  private final JLabel m_message;
  private final DefaultMutableTreeNode m_root;
  private final Color m_defaultLabelColor;
  private final Debugger m_debugger;
  private final Collection m_interestingCommands;

  //
  // Debug methods
  //

//   public static void main(String args[]) {
//     JFrame frame = new JFrame("LV test");
//     frame.getContentPane().add(new LocalVariableDisplay());
//     frame.pack();
//     frame.show();
//     frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
//     frame.list();
//   }

  private static void dumpTree(TreeNode node) {
    JDE.debug(GUI, "tree dump");
    dumpTree(node, "");
  }
  private static void dumpTree(TreeNode node, String prefix) {
    if (node instanceof LVTreeNode) {
      LVTreeNode lvt = (LVTreeNode) node;
      JDE.debug(GUI, prefix + lvt.getName());
    } else
      JDE.debug(GUI, prefix + node.toString());
    prefix += ".";
    for (int index = 0;
	 node.getAllowsChildren() &&
	   (index < node.getChildCount());
	 ++index)
      dumpTree(node.getChildAt(index), prefix);
  }


} // End of class LocalVariableDisplay.java
