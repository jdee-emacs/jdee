package jde.debugger.spec;


import com.sun.jdi.*;
import java.util.*;
import jde.debugger.JDE;

/**
 * SourceNameReferenceTypeSpec.java
 *
 *
 * Created: Mon Jul 19 13:52:21 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.3 $
 */

public class SourceNameReferenceTypeSpec implements ReferenceTypeSpec {

    final String m_sourceName;
    final int    m_lineNumber;
    
    public SourceNameReferenceTypeSpec(String src, int line) {
	m_sourceName = src;
	m_lineNumber = line;
    }
    
    public String getSourceName() {
	return m_sourceName;
    }
    
    /**
     * Does the specified ReferenceType match this spec.
     */
    public boolean matches(ReferenceType refType) {
	try {
	    // Need to fix up the source name as indicated in the ReferenceType, due to
	    // the following bugs in the implementation of javac:
	    // http://developer.java.sun.com/developer/bugParade/bugs/4241573.html
	    // http://developer.java.sun.com/developer/bugParade/bugs/4404985.html
	    // http://developer.java.sun.com/developer/bugParade/bugs/4334342.html
	    //
	    // what happens is that the compiler sometimes incorrectly includes path
	    // information in the SourceFile attribute of the class file. Also, the
	    // directory separator can be either forward or backward slash, independent
	    // of the system-wide file separator, so we need to check for both.
	    
	    String sourceMaybeWithPath = refType.sourceName();
	    int    directorySepIndex   = sourceMaybeWithPath.lastIndexOf('/');
	    
	    if (directorySepIndex == -1) {
		directorySepIndex = sourceMaybeWithPath.lastIndexOf('\\');
	    }
	    
	    String refSourceName = sourceMaybeWithPath;
	    
	    if (directorySepIndex > -1) {
		refSourceName = sourceMaybeWithPath.substring(directorySepIndex + 1);
	    }
	    
	    /*	    Debug.printIf(Debug.EVENTS, "SourceNameRefTSpec.matches(): m_src <" + m_sourceName + ">, " +
			  "refT.src <" + refType.sourceName() + ">, modified src <" + refSourceName + ">");
	    */
	    
	    if (refSourceName.equals(m_sourceName)) {
		try {
		    List locs = refType.locationsOfLine(m_lineNumber);
		    // if we don't throw an exception then it was found
		    // XXX - here, I would like to identify "erroneous" breakpoints, that is,
		    // breakpoints that don't map to any byte code location. They should be 
		    // flagged in such a way that the UI can remove/highlight them as incorrect.
		    
		    return locs.size() > 0;
		    
		} catch(Exception exc) {
		    /*
		} catch(AbsentInformationException exc) {
		} catch(ObjectCollectedException  exc) {
		} catch(InvalidLineNumberException  exc) {
		} catch(ClassNotPreparedException  exc) {
		    */
		    // -- should not happen, so don't catch this ---
		    JDE.debug(EXCEPTION, exc.toString());
		}
	    }
	} catch(AbsentInformationException exc) {
	    // for sourceName(), fall through, it happens a lot with uninteresting 
	    // classes (standard classes compiled without -g, for instance).
	}
	return false;
    }
    
    public String toString() { 
	return m_sourceName + " " + m_lineNumber;
    }

} // SourceNameReferenceTypeSpec


/*
 * $Log: SourceNameReferenceTypeSpec.java,v $
 * Revision 1.3  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of SourceNameReferenceTypeSpec.java
