import scalanative.unsafe._
import scalanative.posix._
import Low.{stringIsCString}

object BuiltIn {
  def cd(args: Array[String]): Boolean = {
    args match {
      case Array(path) => {
        Zone { implicit z =>
          if (unistd.chdir(args(0)) != 0) {
            println("No such file directory")
          }
        }
      }
      case Array() => {
        println("Please input file path")
      }
      case _ => {
        println("Please input single path")
      }
    }
    true
  }

  def exit(args: Array[String]): Boolean = {
    println("Good bye")
    false
  }

  val builtInCommands: Array[(String, Function1[Array[String], Boolean])] =
    Array(
      ("cd", cd _),
      ("exit", exit _)
    )

  def findBuiltIn(name: String): Option[Function1[Array[String], Boolean]] = {
    builtInCommands.find(_._1 == name).map(_._2)
  }
}
