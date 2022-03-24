import scala.scalanative.unsafe._
import scala.scalanative.posix._
import BuiltIn.builtInCommands

object Main {
  def shellExecCommand(command: Command): Boolean = {
    def rec(command: Command): Boolean = {
      command match {
        case SimpleCommand(command, args) => {
          Low.exec(command, args)
          System.err.println("command not found")
          false
        }
        case PipeCommand(head, rest) => {
          val Pipe(reader, writer) = Low.pipe()
          Low.fork() match {
            case Child => {
              Low.restoreTtySignals()
              Low.close(reader)
              Low.dup2(writer, 1)
              Low.close(writer)
              rec(head)
              true
            }
            case Parent(child) => {
              Low.close(writer)
              Low.dup2(reader, 0)
              Low.close(reader)
              Low.waitpid(child)
              rec(rest)
            }
          }
        }
      }
    }

    def normalCommand(): Boolean = {
      Low.fork() match {
        case Child => rec(command)
        case Parent(child) => {
          Low.waitpid(child)
          true
        }
      }
    }

    command match {
      case SimpleCommand(name, args) => {
        BuiltIn.findBuiltIn(name) match {
          case Some(f) => f(args)
          case _       => normalCommand()
        }
      }
      case _ => {
        normalCommand()
      }
    }
  }

  def repl(): Unit = {
    Parser.readUserInput() match {
      case None => ()
      case Some(command) => {
        if (shellExecCommand(command)) repl()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    Low.ignoreTtySignals()
    repl()
  }
}
