package jde.debugger.spec;


import com.sun.jdi.*;
import com.sun.jdi.request.*;

/**
 * WatchpointSpec.java
 * <p>
 * 
 * <p>
 * Created: Tue Aug  3 15:25:42 1999
 * 
 * @author Amit Kumar
 * @since 0.1\
 * @version $Revision: 1.3 $
 */

abstract public class WatchpointSpec extends EventRequestSpec {

    /**
     * For certain specs that need it, the object ID (for whatever need)
     * is stored in the objectID
     */
    public static final Object objectIDKey = "objectID";
    Long objectID = null;
    public void setObjectID(Long objectID) {
	this.objectID = objectID;
	if (request != null)
	    request.putProperty(objectIDKey, objectID);
    }
    
    String fieldName;
    
    public WatchpointSpec(ReferenceTypeSpec refSpec,
			  String fieldName) {
	super(refSpec);
	this.fieldName = fieldName;
    }

    void setRequest(EventRequest request) {
	request.putProperty(objectIDKey, objectID);
	super.setRequest(request);
    }

} // WatchpointSpec

/*
 * $Log: WatchpointSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of WatchpointSpec.java
