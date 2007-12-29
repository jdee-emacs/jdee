package jde.debugger;

import java.util.Iterator;
import java.util.List;

import com.sun.jdi.Bootstrap;
import com.sun.jdi.VirtualMachine;
import com.sun.jdi.connect.Connector;
import com.sun.jdi.VMDisconnectedException;


/**
 * Contains a couple of static functions that simplify managing
 * virtual machines. Some kind of rearrangement between this class and
 * the {@link Debugger} class should probably be made - this class
 * doesn't feel so meaningful at the moment, whereas the methods for
 * launching/attaching/listening to VMs in the Debugger class are
 * messy. Something for the future.
 *
 * <p>
 * Created: Tue Jan 08 18:49:28 2002
 *
 * @author Petter Måhlén
 * @version 1.0
 */

public class VMUtil {
    private static List s_connectors = Bootstrap.virtualMachineManager().allConnectors();

    private VMUtil() {
    }
    
    /**
     * Gets a connector.
     *
     * @param type connector class name
     *
     */    
    public static final Connector getConnector(String name) {
        Iterator iter = s_connectors.iterator();
        while (iter.hasNext()) {
            Connector connector = (Connector)iter.next();
            if (connector.name().equals(name)) {
                return connector;
            }
        }
        return null;
    }


    /**
     * Shut down the indicated virtual machine.
     *
     * @param vm a <code>VirtualMachine</code> value
     */
    public static void shutdown(VirtualMachine vm) {
	/*
	 * taken from the original (Amit Kumar) DebuggeeProcess.shutdown()
	 *
	 * XXX
	 * As far as I can understand, vm.dispose() doesn't terminate the process
	 * that is being debugged. It seems to me as if doing so would be necessary
	 * (otherwise the VM process should be orphaned), so this needs to be
	 * analysed. / Petter
	 */
	
	// isolate the process first
	Process process = null;
	if (vm != null) {
	    try {
		process = vm.process();
		vm.dispose();
	    }
	    catch (VMDisconnectedException e) {
		// If it's already disconnected, no problem, so just ignore it.
	    }
	}
	
	if (process != null) {
	    process.destroy();
	    // XXX sun's jdb implementation works a lot to make sure
	    // the stderr and stdout are dumped before killing
	    // things. i'm not sure how important it is, or even how
	    // well it works (given i can't test it)
	    // sooo, if the reader finds bugs with the output handling
	    // on finish, lemme know. (comment by Amit Kumar)
	}
    }

}// VMUtil
