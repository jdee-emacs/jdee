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

import java.io.*;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.StringTokenizer;


/**
 * Simple logger for debugging JUCI.
 *
 * @author <a href="mailto:nsieger@bitstream.net">Nick Sieger</a>
 * @version 1.0
 */
public class Logger {

  public static class Level {
    private int lvl;
    private Level(int lvl) {
      this.lvl = lvl;
    }
    public String toString() {
      return levels[lvl];
    }
  }

  public static final Level ERROR   = new Level(0);
  public static final Level WARNING = new Level(1);
  public static final Level INFO    = new Level(2);
  public static final Level DEBUG   = new Level(3);

  private static int nextThreadId = 10;
  private static ThreadLocal threadIds = new ThreadLocal();

  private static final String[] levels = {
    "ERROR", "WARNING", "INFO", "DEBUG"
  };

  private static final String LINE_SEP = System.getProperty("line.separator");

  private final Writer output;
  private LispWriter lispWriter;

  private DateFormat timestampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");

  /**
   * Creates a new <code>Logger</code> instance.
   */
  public Logger() {
    this.output = null;
  }

  /**
   * Creates a new <code>Logger</code> instance.
   */
  public Logger(Writer destination) {
    this.output     = destination;
    this.lispWriter = new LispWriter(new PrintWriter(destination));
    try {
      output.write("******************** New Log ********************");
      output.write(LINE_SEP);
      output.flush();
    } catch (IOException io) {}
  }

  public void error(String msg) {
    log(ERROR, msg);
  }

  public void error(Throwable t) {
    log(ERROR, t);
  }

  public void error(String msg, Throwable t) {
    log(ERROR, msg, t);
  }

  public void warning(String msg) {
    log(WARNING, msg);
  }

  public void warning(Throwable t) {
    log(WARNING, t);
  }

  public void warning(String msg, Throwable t) {
    log(WARNING, msg, t);
  }

  public void info(String msg) {
    log(INFO, msg);
  }

  public void debug(String msg) {
    log(DEBUG, msg);
  }

  public void debug(String msg, Object form) {
    log(DEBUG, null, null);
    if (lispWriter != null) {
      try {
        output.write(msg + ": ");
        lispWriter.writeUnknown(form);
        output.write(LINE_SEP);
        output.flush();
      } catch (IOException io) {
      }
    }
  }

  public void log(Level lvl, String msg) {
    log(lvl, msg, null);
  }

  public void log(Level lvl, Throwable t) {
    log(lvl, null, t);
  }

  public void log(Level lvl, String msg, Throwable t) {
    if (output == null) {
      return;                   // logging disabled
    }

    StringBuffer msgbuf = new StringBuffer("T");
    msgbuf.append(getThreadId()).append("|");
    msgbuf.append(timestampFormat.format(new Date())).append("|");
    msgbuf.append(lvl).append("|");
    msgbuf.append(inferCaller());
    if (msg != null) {
      msgbuf.append(LINE_SEP).append(msg);
    }
    msgbuf.append(LINE_SEP);
    if (t != null) {
      StringWriter writer = new StringWriter();
      t.printStackTrace(new PrintWriter(writer));
      msgbuf.append(writer.toString());
    }
    synchronized (this) {
      try {
        output.write(msgbuf.toString());
        output.flush();
      } catch (IOException io) {
        // whoops, unable to write
      }
    }
  }

  private int getThreadId() {
    Integer id;
    synchronized (Logger.class) {
      id = (Integer)threadIds.get();
      if (id == null) {
        id = new Integer(nextThreadId++);
        threadIds.set(id);
      }
    }
    return id.intValue();
  }

  private String inferCaller() {
    // Infer the calling class and method from a stack trace
    StringWriter writer = new StringWriter();
    new Exception().printStackTrace(new PrintWriter(writer));
    StringTokenizer stackTraceParser = new StringTokenizer(writer.toString(), "\r\n");
    String line = null;
    boolean foundLogger = false;
    while (stackTraceParser.hasMoreTokens()) {
      line = stackTraceParser.nextToken();
      if (foundLogger == false) {
        if (line.indexOf("jde.juci.Logger") != -1) {
          foundLogger = true;
        }
      }
      else {
        if (line.indexOf("jde.juci.Logger") == -1) {
          break;
        }
      }
    }
    StringTokenizer classMethodParser = new StringTokenizer(line);
    classMethodParser.nextToken(); // past 'at'
    return classMethodParser.nextToken();
  }

}

// Logger.java ends here
