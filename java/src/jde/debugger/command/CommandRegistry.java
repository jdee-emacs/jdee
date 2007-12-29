package jde.debugger.command;
import java.util.Collection;
import java.util.HashSet;
import java.util.Collections;

/**
 * Each command has a command id associated with it, that is used by
 * jde, to match with the corresponding result/error. jdebug maintains
 * the pending command ids in this collection (as does {@link
 * Application}, see
 * {@link Application#pendingCommands}),
 * and removes
 * them once the command processing is over.<br>
 * Hence, the command id can actually be reused.
 *
 * Created: Sun Feb 18 00:22:14 2001
 *
 * @author <a href="mailto: "</a>
 * @version
 */
public class CommandRegistry {

  private CommandRegistry (){}

  public void addCommand(Integer cmdID) {
    synchronized (pendingCommands) {
      pendingCommands.add(cmdID);
    }
  }

  public void removeCommand(Integer cmdID) {
    synchronized (pendingCommands) {
      pendingCommands.remove(cmdID);
    }
  }

  public boolean commandExists(Integer cmdID) {
    synchronized (pendingCommands) {
      return pendingCommands.contains(cmdID);
    }
  }   

  static public CommandRegistry getTheRegistry() {
    return theRegistry;
  }

  private Collection pendingCommands = Collections.synchronizedSet(new HashSet());

  static CommandRegistry theRegistry = new CommandRegistry();

}// CommandRegistry
