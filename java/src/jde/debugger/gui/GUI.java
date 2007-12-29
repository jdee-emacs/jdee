package jde.debugger.gui;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyVetoException;
import javax.swing.JDesktopPane;
import javax.swing.JFrame;
import javax.swing.JInternalFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPanel;

import jde.debugger.Debugger;
import jde.debugger.JDE;
import jde.debugger.Protocol;


/**
 * This is a first shot at trying to produce a GUI that displays debugging info from
 * JDEbug. I recommend a redesign, either after studying this a bit more, or straight
 * away, depending on the experience of the developer. The main things I am unhappy with
 * are:
 * <ul>
 *
 *   <li>it feels unelegant to have the commands (GetLocals, etc) be
 *   aware of who is the recipient of their results. A better design
 *   would be to let the Debugger decide where to send certain events,
 *   command results, etc.</li>
 *
 *   <li>this class itself is a hack - for instance, there's been very
 *   little thought on member variables are actually necessary, what
 *   JTree-related classes to use, and so on. I've been using it to
 *   learn more about JTree, but wasn't able to finish.</li>
 *
 *   <li>there is (maybe was, now) a bug in the AWT threads, meaning
 *   that it hasn't been possible to shut them down in any other way
 *   than with a System.exit() command.  The Quit command does that
 *   now, which is bad since it can hide other problems. Also, the
 *   whole shutdown sequence is a little messy, maybe due to an
 *   unclear division of labour between the SessionManager, Debugger
 *   and command classes.</li>
 *
 * </ul>
 *
 * <p>
 * Created: Thu Jan 31 13:13:39 2002
 *
 * @author <a href="mailto:petter.mahlen@chello.se">Petter Måhlén</a>
 * @author <a href="mailto:udalrich@carolingia.org">Troy Daniels</a>
 * @version
 */

public class GUI implements Protocol {
  private final JFrame   m_frame;
  private final JMenuBar m_menuBar;
  private final JMenu m_menuWindow;

  public GUI(final Debugger debugger) {
    m_frame    = new JFrame("JDEbug " + debugger.getProcID());
    m_frame.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
    JDesktopPane desktop = new JDesktopPane();
    m_frame.setContentPane(desktop);

    // Initialize the frame
    m_menuBar = new JMenuBar();
    m_frame.setJMenuBar(m_menuBar);
    m_menuWindow = new JMenu("Window");
    m_menuBar.add(m_menuWindow);

    // Set up the GUI stuff
    addComponent("Local Variables",
		 new LocalVariableDisplay(debugger));
    addComponent("Threads", new ThreadDisplay(debugger));


    // Finally, add the base panel to the content pane of the frame.
    m_frame.setSize(350, 400);
    m_frame.show();

    JDE.debug(FRAMEWORK, "GUI constructor done");
  }

  private void addComponent(String title, Component comp) {
    final JInternalFrame internalFrame =
      new JInternalFrame(title,
			 true, // resizable,
			 false, // closable,
			 true, //  maximizable,
			 true // iconifiable
			 );

    JPanel basePanel = new JPanel();
    basePanel.setLayout(new GridLayout(0, 1));
    basePanel.add(comp);
    internalFrame.getContentPane().add(basePanel);
    internalFrame.pack();
    internalFrame.show();
    m_frame.getContentPane().add(internalFrame);
    int offset = 10 * m_frame.getContentPane().getComponentCount();
    internalFrame.setLocation(offset, offset);
    internalFrame.setSize(200, 300);

    JMenuItem menuItem = new JMenuItem(title);
    m_menuWindow.add(menuItem);
    menuItem.addActionListener(new ActionListener() {
	public void actionPerformed(ActionEvent evt) {
	  try {
	    internalFrame.setIcon(false);
	    internalFrame.toFront();
	  } catch (PropertyVetoException exc) {
	    JDE.signalException(exc);
	  }
	}
      });
  }

  /** Shutdown the GUI */
  public synchronized void shutdown() {
    if (null != m_frame)
      m_frame.dispose();
  }
  protected void finalize() throws Throwable {
    super.finalize();
    shutdown();
  }


}// GUI
