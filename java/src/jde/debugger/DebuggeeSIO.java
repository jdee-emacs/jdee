package jde.debugger;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import java.io.PrintStream;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Connects standard input/output/error from a debuggee process to
 * Emacs. This is done using four threads: a first thread started by
 * the {@link #initConnect initConnect} method, which waits for Emacs
 * to connect to a specified port. When that connection is
 * established, three threads for stdin, stdout and stderr are created
 * and started. The threads continue until the {@link #shutdown
 * shutdown} method is called.
 *
 * Created: Sun Feb 18 01:24:09 2001
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.7 $
 */

public class DebuggeeSIO implements Protocol {

  Integer procID;

  /** Socket connection to do i/o */
  Socket m_sioSocket = null;


  Thread standardIOConnectThread;
  StandardInputProcessor standardInputProcessor;
  StandardOutputProcessor standardOutputProcessor;
  StandardErrorProcessor standardErrorProcessor;
  StandardOutputWriter standardOutputWriter;


  private Debugger m_debugger;
    
  public DebuggeeSIO(Debugger debugger){
    m_debugger = debugger;
    procID     = debugger.getProcID();
  }

  /**
   * Launches a thread to connect the Emacs standard I/O buffer
   * for the current process to the standard I/O of the process.
   *
   * <p>
   * This method creates a socket for the standard I/O connection.
   * The thread waits for Emacs to connect to the standard I/O socket.
   *
   * @return Address of standard I/O socket.
   * @exception JDEException if an error occurs
   */
  public int initConnect(final Integer cmdId) throws JDEException {
	
    JDE.signal(procID, MESSAGE, "initSIOConnect: starting standard I/O handshake.", QUOTE);
	
    ServerSocket ss = null;
    try {
      ss = new ServerSocket(0);
    } catch (IOException ex) {
      throw new JDEException("Unable to create a server socket");
    }

    final ServerSocket sstmp = ss;
    final int port = ss.getLocalPort();

    standardIOConnectThread = new Thread("Standard I/O Thread for App #"+procID) {
        public void run() {
          try {
            sstmp.setSoTimeout(15000);

            //   Note!!!   Added to solve initial hang up problem
            JDE.commandResult(cmdId, String.valueOf(port), CMD_OK, NOQUOTE);
			
            JDE.signal(procID, MESSAGE, "Debugger waiting for Emacs to connect to app SIO port " +
                       port + ".", QUOTE);
			
            m_sioSocket = sstmp.accept();
            sstmp.close();
            initTransport();
          } catch (IOException ex) {
            JDE.signal(procID, ERROR, "Gave up waiting for Emacs to connect to SIO port: " + port, QUOTE);
          } catch (SecurityException ex1) {
            JDE.signal(procID, ERROR, "Security exception occurred while connecting to app SIO port " +
                       port, QUOTE);
          }
        }
      };

    JDE.signal(procID, MESSAGE, "initSIOConnect: starting SIO connect thread.", QUOTE);
    standardIOConnectThread.start();
    return port;
  }

  /**
   * Describe <code>initTransport</code> method here.
   *
   */
  public void initTransport() {
    JDE.signal(procID, MESSAGE, "Debugger connected to standard I/O socket.", QUOTE);

    final Process process = m_debugger.getVM().process();
    standardInputProcessor = new StandardInputProcessor(process.getOutputStream());
    standardInputProcessor.start();

    standardOutputWriter = new StandardOutputWriter(m_sioSocket);
    standardOutputWriter.println("*** Process Standard I/O ***");

    standardOutputProcessor = new StandardOutputProcessor(process.getInputStream());
    standardOutputProcessor.start();

    standardErrorProcessor = new StandardErrorProcessor(process.getErrorStream());
    standardErrorProcessor.start();

  }

  public void shutdown() {

    try {
      if (m_sioSocket != null) {
        m_sioSocket.close();
      }
    } catch (IOException e) {
      
    } // end of try-catch
    
  }

  /**
   * Reads standard input from Emacs and forwards it to the application.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardInputProcessor extends Thread {

    public StandardInputProcessor(final OutputStream toVmStream) {
      super("Input Processor for App #"+m_debugger.getProcID());
      
      toVM = new PrintStream(toVmStream, true);

      try {
        fromEmacs =
          new BufferedReader(new InputStreamReader(m_sioSocket.getInputStream()));
      }
      catch (IOException ex1) {
        JDE.signal(procID, ERROR, "Could not get standard input stream from Emacs.", QUOTE);
      }
	
      // setPriority(Thread.MAX_PRIORITY-1);

    }

    public void run() {

      if (fromEmacs == null) return;
      
      try {
        String line;
        while ((line = fromEmacs.readLine()) != null) {
          toVM.println(line);
          toVM.flush();
        }

        /* XXX - Petter: handle this later? it seems to already be taken out...
           if (!proc.isShuttingDown()) {
           try {
           // m_sioSocket.close();
           JDE.signal(procID, MESSAGE, "Process closed its standard input.");
           } catch (Exception ex) {
           JDE.signal(procID, MESSAGE, "Couldn't close socket to standard input.");
           }
           }
        */
			
      } catch (IOException ex) {
        /* XXX - Petter: handle this later? it seems to already be taken out...
           if (!proc.isShuttingDown()) {
           try {
           // m_sioSocket.close();
           JDE.signal(procID, ERROR, "Input error; application I/O closed");
           } catch (Exception e) {
           JDE.signal(procID, ERROR, "Input error; couldn't close application I/O");
           }
           }
        */
      }
    }

    PrintStream toVM;
    BufferedReader fromEmacs;

  }


  /**
   * Writes a line to the socket connected to the
   * standard I/O buffer maintained by Emacs for this
   * application.
   *
   * <p>This class is used by the StandardOutputProcessor
   * and StandardErrorProcessor to forward the application's
   * standard ouput and error output to Emacs.
   *
   * @author "" <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   */
  private class StandardOutputWriter {

    public StandardOutputWriter(Socket m_sioSocket) {
      if (m_sioSocket == null) {
        JDE.signal(procID, ERROR, "Could not transport app output. " +
                   "Transport socket does not exist.", QUOTE);
        return;   
      }

      OutputStream toEmacsStream;

      try {
        toEmacsStream = m_sioSocket.getOutputStream();
        if (toEmacsStream == null) {
          JDE.signal(procID, ERROR, "Could not transport app output. Transport socket closed.", QUOTE);
          return;
        }
      }
      catch (IOException ex1) {
        JDE.signal(procID, ERROR, "Could not transport app output. Transport socket closed.", QUOTE);
        return;
      }

      toEmacs = new BufferedWriter(new OutputStreamWriter(toEmacsStream));
    }
    
    public void write(char[] cbuf, int len) {
      if (toEmacs != null) {
        try {
          toEmacs.write(cbuf, 0, len);
          toEmacs.flush();
        }
        catch (IOException ex1) {
          JDE.signal(procID, ERROR, "I/O error: cannot write process output to Emacs.", QUOTE);
        }
      }
    }

    public void println(String line) {
      try {
        toEmacs.write(line);
        toEmacs.newLine();
      } catch (IOException e) {
        JDE.signal(procID, ERROR, "I/O error: cannot write process output to Emacs.", QUOTE);	
      } // end of try-catch
      
    }

    BufferedWriter toEmacs;
  }


  /**
   * Forwards the application's standard output to Emacs.
   *
   * @author Paul Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardOutputProcessor extends Thread {

    public StandardOutputProcessor(InputStream fromVMStream) {
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
        char[] cbuf = new char[256];
        int len;
        while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
          synchronized (standardOutputWriter) {
            if (standardOutputWriter != null) {
              standardOutputWriter.write(cbuf, len);      
            } // end of if ()    
          }
        }
      }
      catch (IOException ex) {
      }

      /* XXX - Petter: handle this later? it seems to already be taken out...
         if (!proc.isShuttingDown()) {
         try {
         // m_sioSocket.close();
         JDE.signal(procID, MESSAGE, "Closed transport for application's standard output.");
         } catch (Exception ex) {
         JDE.signal(procID, ERROR, "Could not close application standard output transport.");
         }
         }
      */
    }

    BufferedReader fromVM;

  }


  /**
   * Forwards the application's error output to Emacs.
   *
   * @author Paulk Kinnucan <paulk@mathworks.com>
   * @version 1.0
   * @since 1.0
   * @see Thread
   */
  private class StandardErrorProcessor extends Thread {

    public StandardErrorProcessor(InputStream fromVMStream) {
      super("Standard Error Processor for App #" + procID);
      fromVM = new BufferedReader(new InputStreamReader(fromVMStream));
      setPriority(Thread.MAX_PRIORITY-1);
    }

    public void run() {

      String line;

      try {
        char[] cbuf = new char[256];
        int len;
        while ((len = fromVM.read(cbuf, 0, 256)) != -1) {
          synchronized (standardOutputWriter) {
            if (standardOutputWriter != null) {
              standardOutputWriter.write(cbuf, len);      
            } // end of if ()      
          }
        }
      }
      catch (IOException ex) {
      }

      /* XXX - Petter: handle this later? it seems to already be taken out...
         if (!proc.isShuttingDown()) {
         try {
         // m_sioSocket.close();
         JDE.signal(procID, MESSAGE, "Closed transport for application's standard error output.");
         } catch (Exception ex) {
         JDE.signal(procID, ERROR, "Could not close application standard error output transport.");
         }
         }
      */
    }

    BufferedReader fromVM;

  }


}// DebuggerSIO

/*
 * $Log: DebuggeeSIO.java,v $
 * Revision 1.7  2003/01/15 05:49:22  paulk
 * Add Petter's changes.
 *
 * Revision 1.6  2003/01/08 07:03:08  paulk
 * Remove carriage returns.
 *
 * Revision 1.5  2003/01/08 06:53:38  paulk
 * Integrate Petter Mahlen's updates.
 *
 */

// End of DebuggerSIO.java
