package jde.debugger.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;

import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.InconsistentDebugInfoException;
import com.sun.jdi.InvalidStackFrameException;
import com.sun.jdi.Location;
import com.sun.jdi.Method;
import com.sun.jdi.NativeMethodException;
import com.sun.jdi.ObjectReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.ThreadGroupReference;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.Value;
import jde.debugger.CommandEvent;
import jde.debugger.Debugger;
import jde.debugger.Etc;
import jde.debugger.EventSetEvent;
import jde.debugger.EventSetListener;
import jde.debugger.JDE;
import jde.debugger.JDEException;
import jde.debugger.Protocol;


/** A widget that displays the threads
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @since 2.3.4
 * @version $Revision: 1.1 $
 */
class ThreadDisplay extends JPanel implements Protocol
{

  /**  Default ThreadDisplay constructor */
  public ThreadDisplay(Debugger debugger ) {
    super(new BorderLayout());

    m_root = new DefaultMutableTreeNode();
    m_tree = createJTree(m_root);
    m_tree.setRootVisible(false);
    m_debugger = debugger;

    // These are the commands that require us to do something - for
    // now, none
    m_interestingCommands = new HashSet();

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
	  updateThreads(evt);
	}
	public void debuggerResumed(EventSetEvent evt) {}
      });

    JDE.debug(GUI, "ThreadDisplay registered EventSetListener.");

  } // ThreadDisplay.java constructor

  /** Update the display the local variables in the tree */
  private void updateThreads(EventSetEvent evt) {
    JDE.debug(GUI, "updateThreads()");

    try {
      p_message("");

      List         l      = m_debugger.getVM().topLevelThreadGroups();
      Iterator     it     = l.iterator();

      JDE.debug(GUI, "top level thread= " + l);
      while (it.hasNext()) {
	// XXX may have to populate the ObjectStore here.

	updateThreadGroup(m_root, (ThreadGroupReference)it.next());
      }

    } catch (JDEException exc) {
      p_error(exc.getMessage());
    }
  }


  /** Update the thread display
   * @param treeNode The tree node to hold the thread
   * @param tRef The thread to display
   */
  private void updateThreadGroup(DefaultMutableTreeNode treeNode,
				 ThreadGroupReference tRef) throws JDEException {
    JDE.debug(GUI, "updateThreadGroup(" + treeNode + ", " + tRef.name() + ")");
    DefaultMutableTreeNode childNode = findTreeNode(treeNode, tRef);

    List     list = tRef.threadGroups();
    Iterator it   = list.iterator();
    while (it.hasNext()) {
      updateThreadGroup(childNode,
			(ThreadGroupReference)it.next());
    }



    list = tRef.threads();
    it   = list.iterator();

    while (it.hasNext()) {
      updateThread(childNode,
		   (ThreadReference)it.next());
    }


//       } catch (AbsentInformationException ex) {
//         throw new JDEException("Local variable information not available: compile with -g");
//       } catch (NativeMethodException ex) {
//         throw new JDEException("Can't access local variables " +
// 			       "in native methods");
//       } catch (InvalidStackFrameException ex) {
// 	throw new JDEException("Stack frame no longer valid");
//       } catch (InconsistentDebugInfoException ex) {
// 	throw new JDEException("Inconsistent debug information: " +
// 			       ex.getMessage());
//       }
  }

  /** Update the thread display
   * @param treeNode The tree node to hold the thread
   * @param tRef The thread to display
   */
  private void updateThread(DefaultMutableTreeNode treeNode,
			    ThreadReference tRef) throws JDEException {
    JDE.debug(GUI, "updateThread(" + treeNode + ", " + tRef.name() + ")");
    boolean reload = treeNode.getChildCount() == 0;

    DefaultMutableTreeNode childNode = findTreeNode(treeNode, tRef);
    DefaultTreeModel model = (DefaultTreeModel) m_tree.getModel();

    // Update the string for each of the child nodes.  Note that if
    // you add or remove nodes here, you must also update
    // NUM_THREAD_CHILDREN.

    // Start at 1, since the id is constant and set when the node is
    // created

    // Update thread status
    {
      String status = getStatusString(tRef);
      MutableTreeNode statusNode = (MutableTreeNode) childNode.getChildAt(1);
      statusNode.setUserObject(status);
      model.nodeChanged(statusNode);
    }

    // Update thread state
    {
      String state = getStateString(tRef);
      MutableTreeNode stateNode = (MutableTreeNode) childNode.getChildAt(2);
      stateNode.setUserObject(state);
      model.nodeChanged(stateNode);
    }

    // Update stack
    {
      DefaultMutableTreeNode stackNode =
	(DefaultMutableTreeNode) childNode.getChildAt(3);
      stackNode.setUserObject("Stack");
      model.nodeChanged(stackNode);
      updateStack(model, stackNode, tRef);
    }

    if (reload)
      model.reload();

//       } catch (AbsentInformationException ex) {
//         throw new JDEException("Local variable information not available: compile with -g");
//       } catch (NativeMethodException ex) {
//         throw new JDEException("Can't access local variables " +
// 			       "in native methods");
//       } catch (InvalidStackFrameException ex) {
// 	throw new JDEException("Stack frame no longer valid");
//       } catch (InconsistentDebugInfoException ex) {
// 	throw new JDEException("Inconsistent debug information: " +
// 			       ex.getMessage());
//       }
  }

  /** Find the child node for the given thread group.  If the child
      doesn't exist, it is created. */
  private DefaultMutableTreeNode findTreeNode(DefaultMutableTreeNode parent,
					      ThreadGroupReference tRef) {

    DefaultMutableTreeNode child = findExistingTreeNode(parent, tRef);;

    // Add a child if none exists
    if (null == child) {
      child = new DefaultMutableTreeNode(new UserData(tRef));
      addChildOnSwingThread(parent, child);
    }
    return child;
  }

  /** Find the child node for the given thread.  If the child doesn't
   *  exist, it is created.<p>
   */
  private DefaultMutableTreeNode findTreeNode(DefaultMutableTreeNode parent,
					      ThreadReference tRef) {

    DefaultMutableTreeNode child = findExistingTreeNode(parent, tRef);;

    // Add a child if none exists.  Also grandchildren to display the
    // thread status
    if (null == child) {
      child = new DefaultMutableTreeNode(new UserData(tRef));
      // Add ID subnode
      child.add(new DefaultMutableTreeNode("id: " + tRef.uniqueID()));
      // Add other nodes that will be updated every time
      for (int index = 1; index < NUM_THREAD_CHILDREN; ++index)
	child.add(new DefaultMutableTreeNode());
      addChildOnSwingThread(parent, child);
    }
    return child;
  }

  /** Find the child node for the given object.
   * @return The child node or null if none exists
   */
  private DefaultMutableTreeNode findExistingTreeNode(DefaultMutableTreeNode parent,
						      ObjectReference objRef) {

    long uniqueID = objRef.uniqueID();
    // Search for a child with the correct id
    if (parent.getChildCount() > 0) {
      for (DefaultMutableTreeNode treeNode =
	     (DefaultMutableTreeNode) parent.getFirstChild();
	   null != treeNode;
	   treeNode = treeNode.getNextSibling()) {
	UserData userData = (UserData) treeNode.getUserObject();
	if (userData.uniqueID == uniqueID) {
	  return treeNode;
	}
      }
    }

    // If we get here, there is no matching child
    return null;
  }
  /** User object to represent a thread group or a thread.  toString
   * value is displayed in the JTree */
  private class UserData {
    public final long uniqueID;
    public String name;
    UserData(ThreadGroupReference tRef) {
      uniqueID = tRef.uniqueID();
      name = tRef.name();
    }
    UserData(ThreadReference tRef) {
      uniqueID = tRef.uniqueID();
      name = tRef.name();
    }
    public String toString() {
      return name;
    }
  }

  private class ThreadGroupData {
    public final long uniqueId;
    public String name;
    ThreadGroupData(ThreadGroupReference tRef) {
      uniqueId = tRef.uniqueID();
      name = tRef.name();
    }
    public String toString() {
      return name;
    }
  }

  /** Update the nodes under the stack to represent the current stack
      */
  private void updateStack(DefaultTreeModel model,
			   DefaultMutableTreeNode stackNode,
			   ThreadReference thread) {
    int index = 0;
    try {
      List stackFrames = thread.frames();
      Iterator it = stackFrames.iterator();
      while (it.hasNext()) {
	StackFrame frame = (StackFrame) it.next();
	String frameString = getStackFrameString(frame);
	if (index < stackNode.getChildCount()) {
	  MutableTreeNode frameNode = (MutableTreeNode) stackNode.getChildAt(index);
	  frameNode.setUserObject(frameString);
	  model.nodeChanged(frameNode);
	} else {
	  addChildOnSwingThread(stackNode,
				new DefaultMutableTreeNode(frameString));
	}
	++index;
      }
    } catch (IncompatibleThreadStateException exc) {
      // The thread is not suspended, so we can't display the stack.
      // Fall through to the code that removes extra frames, which
      // will now remove all the children.
    }

    // Remove any extra frames
    for (/* */; index < stackNode.getChildCount(); ++index) {
      MutableTreeNode child = (MutableTreeNode) stackNode.getChildAt(index);
      model.removeNodeFromParent(child);
    }

  }


  /** Add a child on to the parent.  Do this on the Swing thread to
   * avoid synchronization issues
   */
  private void addChildOnSwingThread(final DefaultMutableTreeNode parent,
				     final DefaultMutableTreeNode child) {
    SwingUtilities.invokeLater(new Runnable() {
	public void run() {
	  DefaultTreeModel model = (DefaultTreeModel) m_tree.getModel();
	  model.insertNodeInto(child, parent, parent.getChildCount());
	}
      });
  }

  /** Get a string indicating the thread status.  This code is largely
   * copied from { @link jde.debugger.Rep#getThreadRep Rep }
   */
  private String getStatusString(ThreadReference thread) {
    int    status       = thread.status();
    String statusString = "unknown";
    switch (status) {
    case ThreadReference.THREAD_STATUS_MONITOR:
      statusString = "waiting on monitor";
      break;
    case ThreadReference.THREAD_STATUS_NOT_STARTED:
      statusString = "not started";
      break;
    case ThreadReference.THREAD_STATUS_RUNNING:
      statusString = "runnable";
      break;
    case ThreadReference.THREAD_STATUS_SLEEPING:
      statusString = "sleeping";
      break;
    case ThreadReference.THREAD_STATUS_WAIT:
      statusString = "waiting";
      break;
    case ThreadReference.THREAD_STATUS_ZOMBIE:
      statusString = "zombie";
      break;
    case ThreadReference.THREAD_STATUS_UNKNOWN:
      statusString = "unknown";
      break;
    default:
      break;
    }
    // note that the above status string refers to the state of the
    // thread *before* a suspension, if there was a suspension.
    return "status: " + statusString;
  }

  /** Get a string indicating the thread state.  This code is largely
   * copied from { @link jde.debugger.Rep#getThreadRep Rep }
   */
  private String getStateString(ThreadReference thread) {
    /* Due to a bug in ThreadReference.isSuspended(), we need to
       use suspendCount().  This comment is copied from Rep.java.  I
       (troy) don't know if it currently applies or even what the bug is. */
    String stateString = "normal";
    if (thread.isAtBreakpoint()) {
      stateString = "suspended at breakpoint";
    } else if (thread.suspendCount() > 0) {
      stateString = "suspended by debugger";
    }

    return "state: " + stateString;
  }

  /** Get a string indicating the stack frame.  This code is largely
   * copied from { @link jde.debugger.Rep#getThreadRep Rep }
   */
  private String getStackFrameString(StackFrame frame) {
    try {
      Location loc = frame.location();
      Method method = loc.method();
      String result = loc.declaringType().name() + "." +
	method.name() +
	"(" + loc.sourceName() + ":" + loc.lineNumber() + ")";
      return result;
    } catch (AbsentInformationException ex) {
      return "Information not available";
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


  /** Create a JTree that properly renders LVTreeNodes. */
  private JTree createJTree(DefaultMutableTreeNode root) {
    JTree tree = new JTree(root);
    return tree;
  }

  private final JTree m_tree;
  private final JLabel m_message;
  private final DefaultMutableTreeNode m_root;
  private final Color m_defaultLabelColor;
  private final Debugger m_debugger;
  private final Collection m_interestingCommands;

  /** Number of children added below a thread node */
  private final static int NUM_THREAD_CHILDREN = 4;
  //
  // Debug methods
  //


  private static void dumpTree(TreeNode node) {
    JDE.debug(GUI, "tree dump");
    dumpTree(node, "");
  }
  private static void dumpTree(TreeNode node, String prefix) {
    JDE.debug(GUI, prefix + "<" + node.toString() + ">");
    prefix += ".";
    for (int index = 0;
	 node.getAllowsChildren() &&
	   (index < node.getChildCount());
	 ++index)
      dumpTree(node.getChildAt(index), prefix);
  }


} // End of class ThreadDisplay.java

/*
 * $Log: ThreadDisplay.java,v $
 * Revision 1.1  2004/12/24 16:05:19  troy
 * Add window to display threads and stacks
 *
 */
