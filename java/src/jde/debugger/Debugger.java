package jde.debugger;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sun.jdi.ThreadReference;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.AttachingConnector;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.connect.IllegalConnectorArgumentsException;
import com.sun.jdi.connect.LaunchingConnector;
import com.sun.jdi.connect.ListeningConnector;
import com.sun.jdi.connect.VMStartException;
import com.sun.jdi.request.ClassPrepareRequest;
import com.sun.jdi.request.EventRequest;
import com.sun.jdi.request.EventRequestManager;
import jde.debugger.command.CommandHandler;
import jde.debugger.command.ProcessCommandHandler;
import jde.debugger.gui.GUI;
import jde.debugger.spec.EventRequestSpecList;


/**
 * The main class for debugging a specific process. A Debugger instance
 * handles the following tasks:
 * <ul>
 *  <li>Executing commands sent from Emacs (through the
 * ProcessCommandHandler in {@link #m_handler m_handler})</li>
 *  <li>Handling events from the VM (through the EventHandler in
 * {@link #m_eventHandler m_eventHandler})</li>
 *  <li>Keeping track of requested event specifications that haven't
 * yet been resolved ({@link #m_eventRequestSpecList m_eventRequestSpecList})</li>
 *  <li>Keeping track of objects and the ID that is used as a
 * reference in the Emacs/Java communication. ({@link #m_objectStore m_objectStore})</li>
 *  <li>Connecting the standard input/output/error streams of an
 * application that was launched through the debugger to Emacs. This
 * is done through {@link #m_sio m_sio}</li>
 * </ul>
 *
 * Created: Tue Jan 08 12:24:36 2002
 *
 * @author Petter Måhlén
 * @version $Revision: 1.3 $
 */

public class Debugger implements Protocol {
  private ProcessCommandHandler m_handler;
  private VirtualMachine        m_vm;
  private boolean               m_vmAlive;
  private EventRequestSpecList  m_eventRequestSpecList;
  private Integer               m_procID;
  private DebuggeeSIO           m_sio;
  private EventHandler          m_eventHandler;
  private ObjectStore           m_objectStore;
  private boolean               m_useGUI;
  private GUI                   m_gui;

  /**
   * This map stores the event requests that are NOT specs. storing
   * it here allows the user to cancel them easily: they just specify the
   * id, that gets reverse-looked up here, uniquely identifying the actual
   * request.
   * <p>
   * Of course, the id is sent back to the user when the actual command is
   * responded to, so that the handle is available to jde in the first
   * place
   */
  protected Map                 m_identifiableEventRequests;

  /**
   * Creates a new <code>Debugger</code> instance. Before the
   * instance can be used, the following things must happen:
   * <ul>
   *  <li>The VM connection must be set up. This is done through
   * either {@link #launchVM launchVM()}, {@link #attachVMShmem
   * attachVMShmem()}, {@link #attachVMSocket attachVMSocket()},
   * {@link #listenShmem listenShmem()}, or {@link
   * #listenSocket listenSocket()}.</li>
   *  <li>The Debugger instance must have been started. This is done
   * through the {@link #start start} method.</li>
   * </ul>
   *
   * @param procID an <code>Integer</code> value identifying this
   * process in the communication with Emacs.
   */
  public Debugger(Integer procID, boolean useGUI) {
    JDE.debug(EVENTS, "creating debugger with id: " + procID);

    m_procID                    = procID;
    m_identifiableEventRequests = new HashMap();
    m_handler                   = new ProcessCommandHandler(this);
    m_eventRequestSpecList      = new EventRequestSpecList(this);
    m_sio                       = new DebuggeeSIO(this);
    m_eventHandler              = new EventHandler(this);
    m_objectStore               = new ObjectStore(this);
    m_vmAlive                   = false;
    m_useGUI                    = useGUI;
    if (m_useGUI)
    m_gui                       = new GUI(this);
    else
      m_gui                     = null;
  }

  /**
   * Starts up the threads that make the debugger go. Also makes sure that
   * a ClassPrepareRequest is sent to the VM, so that it's possible to resolve
   * breakpoints, etc.
   *
   * @exception JDEException if an error occurs
   */
  public void start() throws JDEException {
    JDE.debug(EVENTS, "starting debugger: " + m_procID);

    if (!m_vmAlive) {
      throw new JDEException("INTERNAL ERROR: attempted to start debugger " + m_procID + " without a VM");
    }

    m_handler.start();
    m_eventHandler.start();


    // we need to raise all class prepare events to
    // make sure we resolve the corresponding specs.
    ClassPrepareRequest cprequest =
      m_vm.eventRequestManager().createClassPrepareRequest();

    // this (hack?) is used to identify if the user itself specified
    // a class prepare request, or the event was raised because of
    // this request.
    cprequest.putProperty("default", "default");
    cprequest.setSuspendPolicy(EventRequest.SUSPEND_ALL);

    // XXX we don't want to get ClassPrepareEvents for the standard
    // java classes, since there's no point in debugging those. At least
    // I don't think there is. / Petter
    cprequest.addClassExclusionFilter("java.*");
    cprequest.addClassExclusionFilter("javax.*");
    cprequest.addClassExclusionFilter("sun.*");

    cprequest.enable();
  }

  /**
   * Tells the debugger to stop executing, meaning that the VM is shut down.
   * The actual execution of the threads is not stopped until the shutdown()
   * method is called, which is only done when a VM disconnect event is sent
   * from the VM.
   *
   * @see EventHandler#vmDisconnectEvent
   */
  public void stopExecution() {
    VMUtil.shutdown(m_vm);
    m_vmAlive = false;
  }

  /**
   * Shuts the debugger down and deregisters it from the SessionManager.
   *
   * @exception JDEException if an error occurs
   * @see SessionManager#deregisterDebugger
   */
  public void shutdown() throws JDEException {
    JDE.debug(EVENTS, "debugger " + m_procID + " shutting down");

    if (m_vmAlive) {
      // the VM will not have been properly shut down if the application runs
      // to the end and the VM disconnects because of that. On the other hand,
      // if the Emacs side issues a Finish command, the VM will be shut down,
      // which is the reason for the m_vmAlive variable.
      VMUtil.shutdown(m_vm);
      m_vmAlive = false;
    }

    if (m_handler != null) {
      m_handler.requestStop();
    }

    if (m_eventHandler != null) {
      m_eventHandler.shutdown();
    }

    if (m_sio != null) {
      m_sio.shutdown();
    }

    if (null != m_gui) {
      m_gui.shutdown();
    }
    // XXX - stop the rest of the stuff as well, when that has been added.

    // Invalidate this object
    m_handler      = null;
    m_eventHandler = null;
    m_vm           = null;
    m_sio          = null;
    m_gui	   = null;

    // this debugger should no longer be available to the session manager
    SessionManager.deregisterDebugger(this);
  }



  /**
   * Launches a virtual machine for the process to be debugged, and
   * sets up the standard in/out/err streams for the process.
   *
   * @param cmdID an <code>Integer</code> value used for setting up
   * SIO streams.
   * @param args a <code>List</code> value
   * @exception JDEException if an error occurs
   */
  public void launchVM(Integer cmdID, List args) throws JDEException {

    // this is the connector that launches a debuggee vm
    String connectSpec = "com.sun.jdi.CommandLineLaunch";

    // check if this kind of connector is, indeed,
    // available. if not, throw an exception.
    LaunchingConnector connector = (LaunchingConnector) VMUtil.getConnector(connectSpec);
    if (connector == null)
      throw new JDEException("No such connector is available: "+connectSpec);


    // first set up the argument map. a table that describes the
    // different keys should be in the public jpda documentation.
    Map argumentMap = connector.defaultArguments();

    Connector.Argument mainArg =
      (Connector.Argument)argumentMap.get("main");

    // compose the command line
    String commandLine = "";
    String quote =((Connector.Argument)argumentMap.get("quote")).value();

    // check if there are special launch options we need to process
    if (args.size() == 0)
      throw new JDEException("Insufficient arguments");

    // XXX - not sure if it's a brilliant idea to hard-code the
    // java executable name here, but that's the way it was
    // done. Anyway, if it becomes a problem, it is now flagged. / Petter
    String executable = "java";

    // be careful with the loop here....
    while ((args.size() >0)
           && args.get(0).toString().startsWith("-")) {
      String origArg = args.remove(0).toString();
      String arg = origArg.toLowerCase();
      if (arg.equals("-vmexec")) {
        if (args.size() == 0)
          throw new JDEException("Missing argument to 'use_executable'");
        executable = args.remove(0).toString();
        Connector.Argument vmexecArg =
          (Connector.Argument)argumentMap.get("vmexec");
        vmexecArg.setValue(executable);
      }
      else if (arg.equals("-home")) {
        if (args.size() == 0)
          throw new JDEException("Missing argument to 'home'");
        String home = args.remove(0).toString();
        Connector.Argument homeArg = (Connector.Argument) argumentMap.get("home");
        homeArg.setValue(home);
        continue;
      }
      else {
        args.add(0, origArg);
        break;
      }
    }

    if (args.size() == 0)
      throw new JDEException("Missing arguments: no class specified?");

    // take care of spaces too! so quote everything.
    Iterator iterator = args.iterator();
    while(iterator.hasNext()) {
      // commandLine += quote + iterator.next() + quote + " ";
      String arg = (String)iterator.next();
      if (arg.equalsIgnoreCase("-classic")) {
        Connector.Argument optionsArg =
          (Connector.Argument)argumentMap.get("options");
        String options = optionsArg.value();
        options = "-classic" + " " + options;
        optionsArg.setValue(options);
        JDE.signal(m_procID, MESSAGE, "VM options: '" + options + "'", QUOTE);
      }
      else
        commandLine += quote + arg + quote + " ";
    }
    mainArg.setValue(commandLine);

    m_vm = null;

    try {
      m_vm = connector.launch(argumentMap);
      JDE.signal(m_procID, MESSAGE, "Launched VM " + m_vm.description(), QUOTE);
      m_vmAlive = true;

      // If we're launching the application, the standard in/out/err needs to be connected
      // to Emacs.
      m_sio.initConnect(cmdID);
    } catch (IOException ex) {
      JDE.debug(EXCEPTION, "Exception launching VM: " + ex.toString());
      throw new JDEException("Unable to launch: " + ex.toString().replace('\\','/'));
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Invalid or inconsistent connector arguments for connector '"+connector+"'");
    } catch (VMStartException ex) {
      throw new JDEException(ex.getMessage().toString().replace('\\','/'));
    }
  }


  /**
   * Attaches to a currently running VM through shared memory. The
   * JPDA framework currently only supports that on Windows systems.
   *
   * @param args a <code>List</code> value
   * @exception JDEException if an error occurs
   */
  public void attachVMShmem(List args) throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Missing name");

    // the attaching connector...
    String connectSpec = null;
    connectSpec = "com.sun.jdi.SharedMemoryAttach";

    AttachingConnector connector = (AttachingConnector) VMUtil.getConnector(connectSpec);
    if (connector  == null)
      throw new JDEException("No such connector is available: "+connectSpec);

    try {
      Map argumentMap = connector.defaultArguments();

      Connector.Argument nameArg =
        (Connector.Argument) argumentMap.get("name");
      nameArg.setValue(args.remove(0).toString());

      m_vm      = connector.attach(argumentMap);
      m_vmAlive = true;

      JDE.signal(m_procID, MESSAGE, "Attached VM (shmem) " + m_vm.description(), QUOTE);

    } catch (IOException ex) {
      JDE.debug(EXCEPTION, ex.toString());
      throw new JDEException("Error attempting to attach to process via shared memory.");
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Illegal connector arguments for connector '"+connector);
    }
  }

  /**
   * Attaches to a currently running VM through socket
   * communication. Works for all platforms, but is slower than
   * shared memory.
   *
   * @param args a <code>List</code> value
   * @exception JDEException if an error occurs
   */
  public void attachVMSocket(List args) throws JDEException {

    if (args.size() < 1)
      throw new JDEException("Missing arguments: specify at least the port");

    // the attaching connector...
    String connectSpec = null;
    connectSpec = "com.sun.jdi.SocketAttach";

    AttachingConnector connector = (AttachingConnector) VMUtil.getConnector(connectSpec);
    if (connector == null)
      throw new JDEException("No such connector is available: " + connectSpec);

    try {
      Map argumentMap = connector.defaultArguments();

      while ((args.size() > 0) && args.get(0).toString().startsWith("-")) {
        String arg = args.remove(0).toString().toLowerCase();
        if (arg.equals("-host")) {
          if (args.size() == 0)
            throw new JDEException("Missing argument to 'host'");
          String host = args.remove(0).toString();
          Connector.Argument hostArg =
            (Connector.Argument)argumentMap.get("hostname");
          hostArg.setValue(host);
        } else if (arg.equals("-port")) {
          if (args.size() == 0)
            throw new JDEException("Missing argument to 'port'");
          String port = args.remove(0).toString();
          Connector.Argument portArg =
            (Connector.Argument)argumentMap.get("port");
          portArg.setValue(port);
        } else {
          args.add(0, arg);
          break;
        }
      }

      m_vm      = connector.attach(argumentMap);
      m_vmAlive = true;

      JDE.signal(m_procID, MESSAGE, "Attached VM (socket) " + m_vm.description(), QUOTE);

    } catch (IOException ex) {
      JDE.debug(EXCEPTION, ex.toString());
      throw new JDEException("I/O error occurred while attempting to attach process.");
    } catch (IllegalConnectorArgumentsException ex) {
      throw new JDEException("Illegal connector arguments for connector '"+connector);
    }

  }

  /**
   * Starts a thread that waits for a VM to be launched and connect
   * to a given address using shared memory. Executing this in a
   * separate thread means that the command handler can go on
   * waiting for new commands, without freezing up. The new thread
   * dies as soon as the VM connects.
   *
   * @param address a <code>String</code> value
   * @exception JDEException if an error occurs
   */
  public void listenShmem(final String address) throws JDEException {
    String connectSpec = "com.sun.jdi.SharedMemoryListen";

    final ListeningConnector connector   = (ListeningConnector) VMUtil.getConnector(connectSpec);
    final Debugger           thisAsLocal = this;

    if (connector == null)
      throw new JDEException("No such connector is available: "+connectSpec);

    Thread thread = new Thread("Listen on shared memory channel.") {

        public void run()  {

          try {
            Map argumentMap = connector.defaultArguments();

            Connector.Argument nameArg =
              (Connector.Argument)argumentMap.get("name");
            nameArg.setValue(address);

            connector.startListening(argumentMap);
            m_vm = connector.accept(argumentMap);
            connector.stopListening(argumentMap);

            JDE.signal(m_procID, MESSAGE, "Attached VM (shmem) " + m_vm.description(), QUOTE);
            m_vmAlive = true;
            thisAsLocal.start();
          } catch (IOException ex) {
            JDE.debug(EXCEPTION, ex.toString());
            JDE.signal(m_procID, MESSAGE,
                       "I/O error occurred while listening at shared memory address:"
                       + address,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          } catch (IllegalConnectorArgumentsException ex) {
            JDE.debug(EXCEPTION, ex.toString());
            JDE.signal(m_procID, MESSAGE,
                       "Illegal argument error occurred while listening " +
                       "at shared memory address: " + address,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          } catch (JDEException ex) {
            JDE.debug(EXCEPTION, ex.toString());
            JDE.signal(m_procID, MESSAGE,
                       "Error starting up debugger: " + ex,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          }
        }
      };

    JDE.signal(m_procID, MESSAGE,
               "Listening at shared memory address: " + address,
               QUOTE);
    thread.start();

  }

  /**
   * Starts a thread that waits for a VM to be launched and connect
   * to a given address using socket communication. Executing this in a
   * separate thread means that the command handler can go on
   * waiting for new commands, without freezing up. The new thread
   * dies as soon as the VM connects.
   *
   * @param address a <code>String</code> value
   * @exception JDEException if an error occurs
   */
  public void listenSocket(final String address) throws JDEException {
    String connectSpec = "com.sun.jdi.SocketListen";

    final ListeningConnector connector   = (ListeningConnector) VMUtil.getConnector(connectSpec);
    final Debugger           thisAsLocal = this;

    if (connector == null)
      throw new JDEException("No such connector is available: "+connectSpec);

    Thread thread = new Thread("Listen on socket.") {

        public void run()  {
          try {
            Map argumentMap = connector.defaultArguments();

            Connector.Argument portArg =
              (Connector.Argument)argumentMap.get("port");
            portArg.setValue(address);

            connector.startListening(argumentMap);
            m_vm = connector.accept(argumentMap);
            connector.stopListening(argumentMap);

            JDE.signal(m_procID, MESSAGE,
                       "Attached VM (socket) " + m_vm.description(),
                       QUOTE);
            m_vmAlive = true;
            thisAsLocal.start();
          } catch (IOException ex) {
            JDE.debug(EXCEPTION, ex.toString());
            JDE.signal(m_procID, MESSAGE,
                       "Error occurred when listening on socket: " + ex,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          } catch(IllegalConnectorArgumentsException ex) {
            JDE.signal(m_procID, MESSAGE,
                       "Illegal connector arguments for connector '"+connector + " " + ex,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          } catch(JDEException ex) {
            JDE.signal(m_procID, MESSAGE,
                       "Error starting up debugger: " + ex,
                       QUOTE);
            try {
              SessionManager.deregisterDebugger(thisAsLocal);
            }
            catch (JDEException e) { /* FALLTHROUGH */ }
          }
        }
      };

    JDE.signal(m_procID, MESSAGE,
               "Listening at socket address: " + address, QUOTE);
    thread.start();
  }


  public EventRequestSpecList getEventRequestSpecList() {
    return m_eventRequestSpecList;
  }

  public CommandHandler getCommandHandler() {
    return m_handler;
  }

  public Integer getProcID() {
    return m_procID;
  }

  public ObjectStore getStore() {
    return m_objectStore;
  }

  public void signalCommandResult(Integer cmdID, String message, boolean success) {
    JDE.commandResult(cmdID, message, success, NOQUOTE);
  }

  public void signalCommandResult(Integer cmdID, String message, boolean success, boolean quote) {
    JDE.commandResult(cmdID, message, success, quote);
  }

  public VirtualMachine getVM() {
    return m_vm;
  }

  public GUI getGUI() {
    return m_gui;
  }

  /**
   * Returns true if this is a valid debugger. A debugger is valid if
   * the start() method has been called, but not the shutdown()
   * method. XXX - actually not correct at the moment, but it
   * doesn't matter. The method returns true from the moment the
   * Debugger instance has been created until the shutdown() method
   * is called.
   *
   * @return a <code>boolean</code> value
   */
  public boolean isValid() {
    // Am using the command handler to indicate whether this is a live
    // debugger or not.
    return m_handler != null;
  }

  /**
   * Returns the thread corresponding to a given name, or null if
   * there is no such thread.
   *
   * @param name
   */
  public ThreadReference getThreadReference(String name) {

    List     list = m_vm.allThreads();
    Iterator it   = list.iterator();

    ThreadReference thread;
    while (it.hasNext()) {
      thread = (ThreadReference)it.next();
      if (thread.name().equals(name)) return thread;
    }

    return null;
  }


  /**
   * Adds an event request to the identifiable events, for future
   * reference. Also enables the event.
   *
   * @return an identifier for the request
   */
  public Long addIdentifiableRequest(EventRequest e) {
    Long id = SessionManager.generateObjectID();
    synchronized (m_identifiableEventRequests) {
      m_identifiableEventRequests.put(id, e);
    }

    e.enable();

    return id;
  }

  /**
   * Removes an event request. Also disables/deletes from the vm.
   */
  public void deleteIdentifiableRequest(Long id) throws JDEException {

    EventRequestManager erm = getVM().eventRequestManager();

    synchronized (m_identifiableEventRequests) {
      if (!m_identifiableEventRequests.containsKey(id)) {
        throw new JDEException("Invalid request ID");
      } else {
        Object e = m_identifiableEventRequests.remove(id);
        if (e == null) {
          throw new JDEException("No such event request");
        } else if (e instanceof EventRequest) {
          ((EventRequest)e).disable();
          erm.deleteEventRequest((EventRequest)e);
        } else {
          throw new JDEException("INTERNAL ERROR: Not an event request : " + e.toString());
        }
      }
    }
  }

    /** Add an EventSetListener.  If the listener is already in the
     * list, nothing is done.<p>
     *
     * This is handled by the eventHandler, but there is no public
     * access to that
     */
  public void addEventSetListener(EventSetListener listener) {
      m_eventHandler.addEventSetListener(listener);
    }

    /** Remove an EventSetListener.  If the listener is already in the
     * list, nothing is done */
  public void removeEventSetListener(EventSetListener listener) {
      m_eventHandler.removeEventSetListener(listener);
    }

    /** Add an CommandListener.  If the listener is already in the
     * list, nothing is done.<p>
     *
     */
  public void addCommandListener(CommandListener listener) {
      m_handler.addCommandListener(listener);
    }

    /** Remove an CommandListener.  If the listener is already in the
     * list, nothing is done */
  public void removeCommandListener(CommandListener listener) {
      m_handler.removeCommandListener(listener);
    }

}// Debugger


/*
 * $Log: Debugger.java,v $
 * Revision 1.3  2003/04/29 16:51:56  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.2  2003/01/15 05:50:51  paulk
 * Remove CRs.
 *
 * Revision 1.1  2003/01/08 07:16:45  paulk
 * Initial revision.
 *
 */

// End of Debugger.java
