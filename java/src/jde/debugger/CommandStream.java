package jde.debugger;
import java.io.StreamTokenizer;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StreamTokenizer;
import java.util.ArrayList;
import java.util.List;



/**
 * CommandStream.java
 *
 *
 * Created: Tue Feb 13 15:40:34 2001
 *
 * @author <a href="mailto: "</a>
 * @version 1.0
 */

public class CommandStream extends StreamTokenizer implements Protocol {

  public CommandStream (BufferedReader in){

    super(in);
      m_reader = in;
    setSyntax();
  }

  public List nextCommand() {

    List commandLine = new ArrayList();

    try {

      int token = nextToken();

      while (token != TT_EOL) {

	switch (token) {

	case TT_EOF :
	  throw new IOException("EOF occurred reading command stream.");

	case TT_WORD :
	case '"' :
	case '\'':
	  commandLine.add(sval);
	  break;

	default:
          commandLine.add(String.valueOf((char)token));
	  break;
	} // end of switch ()

	token = nextToken();
      }

      if (commandLine.size() < 3) {
        if (commandLine.size() > 0) {
          JDE.commandResult(new Integer(-1), "Malformed command: size=" + commandLine.size(),
			    CMD_NOK, QUOTE);
	  JDE.debug(COMMANDS, commandLine.toString());
	}
        commandLine = nextCommand();
      }
    }
    catch (IOException ex) {
      commandLine = null;
    } // end of catch

    return commandLine;
  }

  /**
   * Sets the syntax of the command stream. We want the input to be broken
   * into lines, with whitespaces separating tokens
   */
  private void setSyntax() {
    resetSyntax();
    eolIsSignificant(true);
    whitespaceChars('\u0000', '\u0020');
    wordChars('\u0021', '\u00ff');
    quoteChar('"');
  }

  public static void main (String[] args) {

    PrintWriter out = new PrintWriter(System.out);
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    CommandStream commandStream = new CommandStream(in);

    out.print(">  ");
    out.flush();
    List command = commandStream.nextCommand();
    while (command != null && ! ((String) command.get(2)).startsWith("qu")) {

      int n = command.size();
      for (int i = 0; i<n; i++) {
        out.println(command.get(i));
      } // end of for ()

      out.print("> ");
      out.flush();
      command = commandStream.nextCommand();
    } // end of while ()


  } // end of main ()

  private final Reader m_reader;

  // ----------------------------------------
  // Fix for bug 4809647
  // ----------------------------------------
  static {
      try {
	StdIn.fix();
      } catch (IOException exc) {
	throw new ExceptionInInitializerError(exc);
      }
  };

  /**
   * You must call <code>StdIn.fix()</code> once before using
   <code>System.in</code>.
  */
  private static final class StdIn
  {
    private static InputStream       in;
    private static PipedInputStream  pis;
    private static PipedOutputStream pos;

    private static Thread            pump = new Thread("StdIn Pump")
      {
        public void run()
        {

	  byte[] buf = new byte[5120];

	  try {
	    while (true) {
	      int available = in.available();

	      if (available == 0) {
		Thread.sleep(50);
		continue;
	      }

	      int howMany = Math.min(buf.length, available);


	      //This works because we asked how many are there
	      in.read(buf, 0, howMany);
	      pos.write(buf, 0, howMany);
// 	      JDE.debug(JDE_PIPE, "Pumped " + howMany + " bytes to stdin, " +
// 			"starting with '" +
// 			new String(buf, 0, Math.min(20, howMany)) + "...'");

	    }
	  }
	  catch (Exception ex) {
	    ex.printStackTrace();
	  }
        }

      };

    /**
     * Nobody can create an instance of this class
     */
    private StdIn() {}

    /**
     * This method replaces System.in and workarounds bug
     #4809647
    */
    public synchronized static void fix()
      throws IOException
    {
      if (in != null) {
	return;
      }

      in  = System.in;
      pos = new PipedOutputStream();
      pis = new PipedInputStream(pos);
      System.setIn(pis);
      pump.setDaemon(true);
      pump.setPriority(Thread.MIN_PRIORITY);
      pump.start();
    }
  }
  // ----------------------------------------
  // End Fix for bug 4809647
  // ----------------------------------------

}// CommandStream
/*
 * $Log: CommandStream.java,v $
 * Revision 1.5  2004/12/24 16:05:12  troy
 * Add window to display threads and stacks
 *
 * Revision 1.4  2003/04/29 16:51:56  troy
 * Initial version of GUI.  Includes display of local variables.
 *
 * Revision 1.3  2003/01/08 07:03:08  paulk
 * Remove carriage returns.
 *
 * Revision 1.2  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of CommandStream.java


