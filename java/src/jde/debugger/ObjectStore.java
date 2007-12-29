package jde.debugger;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.sun.jdi.*;

/**
 * ObjectStore.java
 * <p>
 *
 * The main function of this class is to keep a store of all the currently
 * referenced objects. Any time jdebug sends an object ID across, it stores
 * the ObjectReference itself in {@link #m_objectMap}, mapped to the id that
 * will identify this object. This id is the ObjectReference.uniqueID().
 * <p>
 * <i>
 * XXX - I am not really sure if the below argumentation is really correct.
 * The store is definitely needed to look up what object corresponds to a 
 * given unique id. And having a reference to the object stored there means 
 * it will not be garbage collected. But why make such a fuss about it? 
 * Something to think about, I could be missing something. / Petter
 * </i>
 * <p>
 * We need to do this because if we don't keep a link to the ObjectReference
 * <i>some</i>where, it might get garbage collected, and the id used to
 * identify it (ie the uniqueID) reused. If the user then requests info
 * about that ID, he'll be sent information about the new object, which is
 * obviously wrong.
 * <p>
 * When jde wants to know more about the object, it sends across the id,
 * which is used to reference the ObjectReference in the Map
 * <p>
 * <i>
 * XXX The below stuff is another aspect of the ObjectStore that
 * appears to be unnecessarily complicated. It is currently not
 * active, since there is no call to the method {@link #trim}
 * </i>
 * <p>
 * Since this is done with each object that's ever reported to jde, the list
 * can get pretty huge, and needs to be refreshed from time to time. For
 * this purpose, we maintain the variable {@link #m_maximumLimit}.
 * <p>
 * Objects keep getting added to the list, until we
 * reach {@link #m_maximumLimit}. At this point, a notification is sent to the
 * jde side requesting for a list of all the object references (ie, the ids)
 * that it is currently interested in. 
 * <p>
 * When this list is obtained, the {@link #m_objectMap} is scanned and entries
 * <i>not</i> in this list removed. {@link #m_maximumLimit} is then set to
 * 2 times the current size of the list, or the old maximumLimit, whichever
 * is larger. This is to ensure we don't keep sending the request over and
 * over again very frequently.
 * <p>
 * Note that we keep adding objects to the Map even after sending the
 * notification to jde: any reducing the size of the list is only done
 * when jde responds.
 * <p>
 * <b>Note:</b> Question: Should we disable garbage collection of objects
 * in the debugee VM once we put their corresponding ObjectReference in the
 * debugger VM in the objectstore? And maybe enable the gc once they're
 * removed from the store? This way we'll never get the ObjectCollected
 * exception, and we can use the object as long as its displayed on the
 * emacs side. The only thing is, we'd need the emacs side to be pretty
 * frequent about the list of things it is displaying so we don't encumber
 * the debuggee VM too much with objects it's unable to collect.
 * <p>
 * Created: Thu Jul 29 10:38:06 1999
 * 
 * @author Amit Kumar
 * @author Paul Kinnucan
 * @author Petter Mahlen
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class ObjectStore implements Protocol {
    
  private Debugger m_debugger;
    
  /**
   * Maps object_id -> ObjectReference. Not synchronized, since it is
   * only accessed from synchronized methods in this class.
   */
  private Map     m_objectMap;

  /** maximum number of objects before we send a notification to jde */
  private long    m_maximumLimit = 100;

  /** keep track of if our request has been met yet */
  private boolean m_requestPending = false;

  /** Create a new object map for a new process. */
  public ObjectStore(Debugger debugger) {
    m_debugger  = debugger;
    m_objectMap = new HashMap();
  }

  /** 
   * Register an object. Needs to be synchronized since both the event 
   * handler and command handler threads will be accessing the object
   * store.
   */
  public synchronized void put(ObjectReference ref) {
	
    //	Debug.printIf(Debug.EVENTS, "storing object with reference number: " + ref.uniqueID());
	
    m_objectMap.put(new Long(ref.uniqueID()), ref);
    long size = m_objectMap.size();
	
    if (size > m_maximumLimit) {
      if (!m_requestPending) {
        JDE.signal(m_debugger.getProcID(), REPORT_IDS_IN_USE, null);
        m_requestPending = true;
      }
    }
  }

  /**
   * jde sent us a list of objects it is currently interested in. Trim
   * objectMap based on this list. 
   * <p>
   * XXX - this method is never called, currently!
   * Also, I would say it's unnecessary, since a debugging session is usually pretty
   * short and involves less than a couple of hundred commands from Emacs. And that
   * in term means that there won't be more than a couple of thousand objects or so
   * in the map, which should be OK. / Petter
   */
  public synchronized void trim(List objectIDs) {
    Map newMap = new HashMap();
    Iterator it = objectIDs.iterator();
    while (it.hasNext()) {
      Long id = (Long)it.next();
	    
      if (m_objectMap.containsKey(id)) {
        newMap.put(id, m_objectMap.get(id));
      }
    }
    m_maximumLimit   = 2 * newMap.size();
    m_objectMap      = newMap;
    m_requestPending = false;
  }

  /** Returns the object corresponding to the id, or null */
  public synchronized ObjectReference get(Object id) {
    return (ObjectReference) m_objectMap.get(id);
  }

} // ObjectStore

/*
 * $Log: ObjectStore.java,v $
 * Revision 1.3  2003/01/08 06:53:37  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of ObjectStore.java
