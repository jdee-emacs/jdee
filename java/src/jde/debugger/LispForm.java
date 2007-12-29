
package jde.debugger;

/**
 * LispForm.java
 * <p>
 * A wrapper on a string, used to differentiate a normal string and a string
 * that's actually a lisp form
 * <p>
 * Created: Fri Jul 30 11:00:29 1999
 * 
 * @author Amit Kumar
 * @since 0.1
 */

public class LispForm {

    private String str;
    
    public LispForm() {
	this.str = "";
    }

    public LispForm(String str) {
	this.str = str;
    }

    public void cat(String str) {
	this.str += str;
    }

    public String toString() {
	return str;
    }
    
} // LispForm
