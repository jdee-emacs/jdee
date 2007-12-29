package jde.debugger;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;
import java.util.Iterator;

/**
 * ProcessRegistry.java
 *
 *
 * Registry of processes. The registry maps each process
 * to its ID.
 *
 * Created: Thu Feb 15 13:48:06 2001
 *
 * @author <a href="mailto:pkinnucan@mediaone.net">Paul Kinnucan</a>
 * @version $Revision: 1.1 $
 */
public class ProcessRegistry {

  private ProcessRegistry (){}

  public void addProcess(Integer procID, DebuggeeProcess proc) {
    synchronized (processes) {
      processes.put(procID, proc);
    }
  }

  /**
   * called by {@link DebuggeeProcess#shutdown}
   * to remove it's own entry from
   * the applications collection here
   */
  public void removeProcess(Integer procID) {
    synchronized (processes) {
      processes.remove(procID);
    }
  }

  public boolean processExists(Integer procID) {
    synchronized (processes) {
      return 
	processes.containsKey(procID) && 
	(((DebuggeeProcess)getProcess(procID)) != null);
    }
  }

  public DebuggeeProcess getProcess(Integer procID) {
    synchronized (processes) {
      return (DebuggeeProcess) processes.get(procID);
    }
  }

  public static ProcessRegistry getRegistry() {
    return registry;
  }

  /**
   * Shuts down all the applications prior to exiting
   */
  public void shutdownProcesses()
    throws JDEException {
    // run the "shutdown" function for all the apps currently being
    // debugged.
    synchronized (processes) {
      Iterator iterator = processes.values().iterator();
      while (iterator.hasNext()) {
	((DebuggeeProcess)iterator.next()).shutdown();
	iterator.remove();
      }
    }
  }


  Map processes = Collections.synchronizedMap(new HashMap());

  private static ProcessRegistry registry = new ProcessRegistry();

}// ProcessRegistry
