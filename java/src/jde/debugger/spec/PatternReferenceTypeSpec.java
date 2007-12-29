package jde.debugger.spec;

import com.sun.jdi.*;

import java.util.*;
import jde.debugger.JDEException;

/**
 * PatternReferenceTypeSpec.java
 *
 *
 * Created: Mon Jul 19 12:47:26 1999
 *
 * @author Amit Kumar
 * @since 0.1
 * @version $Revision: 1.2 $
 */

public class PatternReferenceTypeSpec implements ReferenceTypeSpec {

    final String classPattern;
    final boolean isWild;

    public PatternReferenceTypeSpec(String classPattern) {
	// XXX should we check the validity of the class name as
	// suggested in the checkClassName doc below?
	isWild = classPattern.startsWith("*.");
	if (isWild) {
	    this.classPattern = classPattern.substring(1);
	} else {
	    this.classPattern = classPattern;
	}
    }

    public String getClassPattern() { return classPattern; }

    public boolean matches(ReferenceType refType) {
	if (isWild) {
	    return refType.name().endsWith(classPattern);
	} else {
	    return refType.name().equals(classPattern);
	}
    }

    private void checkClassName(String className) throws JDEException {
        // Do stricter checking of class name validity on deferred
        //  because if the name is invalid, it will
        // never match a future loaded class, and we'll be silent
        // about it.
        StringTokenizer tokenizer = new StringTokenizer(className, ".");
        boolean first = true;
        while (tokenizer.hasMoreTokens()) {
            String token = tokenizer.nextToken();
            // Each dot-separated piece must be a valid identifier
            // and the first token can also be "*". (Note that 
            // numeric class ids are not permitted. They must
            // match a loaded class.)
            if (!isJavaIdentifier(token) && !(first && token.equals("*"))) {
                throw new JDEException("(Class Pattern Resolution Error) Invalid pattern '"+className+"'");
            }
            first = false;
        }
    }

    private boolean isJavaIdentifier(String s) {
        if (s.length() == 0) {                              
            return false;
        }
        if (! Character.isJavaIdentifierStart(s.charAt(0))) {
            return false;
        }
        for (int i = 1; i < s.length(); i++) {
            if (! Character.isJavaIdentifierPart(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public String toString() {
        return isWild? "*." + classPattern : classPattern;
    }
    
} // PatternReferenceTypeSpec

/*
 * $Log: PatternReferenceTypeSpec.java,v $
 * Revision 1.2  2003/01/15 06:06:15  paulk
 * Petter Mahlen's changes.
 *
 */

// End of PatternReferenceTypeSpec.java
