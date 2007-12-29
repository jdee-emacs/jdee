
package jde.debugger;

import jde.debugger.spec.*;

import com.sun.jdi.request.*;

import java.util.*;

/**
 * ApplicationCommands.java
 * <p>
 * An abstract class that must be implemented by classes that process
 * commands from the jde side.
 * <p>
 * Created: Fri Jul 30 17:13:57 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public abstract class ProcessCommands implements Protocol {

    final DebuggeeProcess proc;
    final ObjectStore store;

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
    protected Map identifiableEventRequests =
	Collections.synchronizedMap(new HashMap());

    /**
     * Adds an event request to the above map. Also enables the request.
     *
     * @return an identifier for the request
     */
    protected Long addIdentifiableRequest(EventRequest e) {
	Long id;
	synchronized (identifiableEventRequests) {
	    id = proc.generateObjectID();
	    identifiableEventRequests.put(id, e);
	}
	e.enable();
	return id;
    }

    /**
     * Removes an event request. Also disables/deletes from the vm.
     */
    protected void deleteIdentifiableRequest(Long id)
	throws JDEException {

	EventRequestManager erm = proc.getVM().eventRequestManager();

	synchronized (identifiableEventRequests) {
	    if (!identifiableEventRequests.containsKey(id)) {
		throw new JDEException("Invalid request ID");
	    } else {
		Object e = identifiableEventRequests.get(id);
		if (e == null) {
		    throw new JDEException("No such event request");
		} else if (e instanceof EventRequest) {
		    ((EventRequest)e).disable();
		    erm.deleteEventRequest((EventRequest)e);
		} else {
		    throw new JDEException("Not an event request");
		}
	    }
	}
    }
	

    public ProcessCommands(DebuggeeProcess p, ObjectStore s) {
	proc = p;
	store = s;
    }
    
} // ProcessCommands
