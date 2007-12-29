package jde.debugger;

import com.sun.jdi.Type;
import com.sun.jdi.LocalVariable;
import com.sun.jdi.ClassNotLoadedException;
import com.sun.jdi.Value;


/**
 * DisplayableValue.java
 *
 *
 * Created: Fri Feb 01 12:02:55 2002
 *
 * @author <a href="mailto:petter.mahlen@chello.se">Petter Måhlén</a>
 * @version $Revision: 1.1 $
 */

public class DisplayableValue {
  private Value         m_value;
    
  public DisplayableValue(Value value) {
    m_value     = value;
  }


  public Value getValue() {
    return m_value;
  }

  public void setValue(Value value) {
    m_value = value;
  }
    
  public String toString() {
    if (m_value == null) {
      return "null value";
    }
	
    return m_value.toString();
  }
}// DisplayableValue
