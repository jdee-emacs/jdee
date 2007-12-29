/*
 * Copyright (C) 2002 by Nick Sieger
 *
 * $Revision: 1.1 $
 * $Date: 2003/02/15 20:58:26 $
 *
 * Author: Nick Sieger <nsieger@bitstream.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package jde.juci;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * Class responsible for converting java objects into lisp forms.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class LispWriter {

  /**
   * Java-equivalent of the elisp symbol <code>t</code>.
   */
  public static final Symbol T   = new Symbol("t");

  /**
   * Java-equivalent of the elisp symbol <code>nil</code>.
   */
  public static final Symbol NIL = new Symbol("nil");

  /**
   * Symbol to represent a Java <code>null</code> to Elisp, in the JUCI
   * convention.  Since it is a non-special Elisp symbol, it is quoted here.
   */
  public static final Quoted NULL = new Quoted(new Symbol("null"));

  private PrintWriter output;
  private boolean     autoQuoteLists = true;
  private boolean     inQuote        = false;

  /**
   * Creates a new <code>LispWriter</code> instance.
   */
  public LispWriter(PrintWriter dest) {
    this.output = dest;
  }

  public boolean isAutoQuoteLists()  {
    return this.autoQuoteLists;
  }

  public void setAutoQuoteLists(boolean autoQuoteLists) {
    this.autoQuoteLists = autoQuoteLists;
  }

  public void writeList(List list) {
    if (isAutoQuoteLists() && !inQuote) {
      output.write("'");
      inQuote = true;
      writeForm(list);
      inQuote = false;
    } else {
      writeForm(list);
    }
  }

  public void writeForm(List list) {
    output.print("(");
    for (int i = 0; i < list.size(); i++) {
      if (i > 0) {
        output.print(" ");
      }
      writeUnknown(list.get(i));
    }
    output.print(")");
  }

  public void writeAlist(Map map) {
    List alist = new ArrayList();
    for (Iterator i = map.entrySet().iterator(); i.hasNext(); ) {
      Map.Entry e = (Map.Entry) i.next();
      alist.add(new Cons(e.getKey(), e.getValue()));
    }
    writeList(alist);
  }

  public void writeCons(Cons cons) {
    output.print("(");
    writeUnknown(cons.getCar());
    output.print(" . ");
    writeUnknown(cons.getCdr());
    output.print(")");
  }

  public void writeString(String string) {
    output.print("\"");
    for (int i = 0; i < string.length(); i++) {
      char c = string.charAt(i);
      // TODO: any more special characters that should be quoted here?
      switch (c) {
      case '\"':
      case '\'':
      case '\\':
        output.write("\\");
        output.write(c);
        break;
      case '\n':
        output.write("\\n");
        break;
      case '\b':
        output.write("\\b");
        break;
      case '\r':
        output.write("\\r");
        break;
      case '\t':
        output.write("\\t");
        break;
      case '\f':
        output.write("\\f");
        break;
      default:
        output.print(c);
        break;
      }
    }
    output.print("\"");
  }

  public void writeInt(int n) {
    output.write(Integer.toString(n));
  }

  public void writeInt(Integer n) {
    writeInt(n.intValue());
  }

  public void writeLong(long n) {
    output.write(Long.toString(n));
  }

  public void writeLong(Long n) {
    writeLong(n.longValue());
  }

  public void writeFloat(float f) {
    output.write(Float.toString(f));
  }

  public void writeFloat(Float f) {
    writeFloat(f.floatValue());
  }

  public void writeDouble(double d) {
    output.write(Double.toString(d));
  }

  public void writeDouble(Double d) {
    writeDouble(d.doubleValue());
  }

  public void writeChar(char c) {
    output.write("?");
    // TODO: any more special characters that should be quoted here?
    switch (c) {
    case '\"':
    case '\'':
    case '\\':
    case '?':
    case ')':
    case '(':
    case ']':
    case '[':
      output.write("\\");
      output.write(c);
      break;
    case '\n':
      output.write("\\n");
      break;
    case '\b':
      output.write("\\b");
      break;
    case '\r':
      output.write("\\r");
      break;
    case '\t':
      output.write("\\t");
      break;
    case '\f':
      output.write("\\f");
      break;
    default:
      output.write(c);
      break;
    }
  }

  public void writeChar(Character c) {
    writeChar(c.charValue());
  }

  public void writeBoolean(boolean b) {
    if (b) {
      writeT();
    } else {
      writeNil();
    }
  }

  public void writeBoolean(Boolean b) {
    writeBoolean(b.booleanValue());
  }

  public void writeT() {
    writeSymbol(T);
  }

  public void writeNil() {
    writeSymbol(NIL);
  }

  public void writeSymbol(Symbol name) {
    output.write(name.getName());
  }

  public void writeQuoted(Quoted q) {
    boolean changeQuote = ! inQuote;

    if (changeQuote) {
      inQuote = true;
    }

    output.write("'");
    writeUnknown(q.getQuoted());

    if (changeQuote) {
      inQuote = false;
    }
  }

  private void writeNumber(Number n) {
    if (n instanceof Long) {
      writeLong(n.longValue());
    } else if (n instanceof Double) {
      writeDouble(n.doubleValue());
    } else if (n instanceof Float) {
      writeFloat(n.floatValue());
    } else {
      writeInt(n.intValue());
    }
  }

  public void writeUnknown(Object o) {
    if (o instanceof Number) {
      writeNumber((Number) o);
    } else if (o instanceof Collection) {
      writeList(new ArrayList((Collection) o));
    } else if (o instanceof Map) {
      writeAlist((Map) o);
    } else if (o instanceof Cons) {
      writeCons((Cons) o);
    } else if (o instanceof Quoted) {
      writeQuoted((Quoted) o);
    } else if (o instanceof Symbol) {
      writeSymbol((Symbol) o);
    } else if (o instanceof Character) {
      writeChar((Character) o);
    } else if (o instanceof Boolean) {
      writeBoolean((Boolean) o);
    } else if (o != null) {
      writeString(o.toString());
    } else {
      writeQuoted(NULL);
    }
  }
}

// LispWriter.java ends here
