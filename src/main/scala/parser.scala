import scala.io.StdIn.{readLine}

sealed trait Command
case class SimpleCommand(command: String, args: Array[String]) extends Command
case class PipeCommand(simple: SimpleCommand, rest: Command) extends Command

object Parser {

  def parse(str: String): Option[Command] = {
    def splitAtPipe(tokens: List[String]): List[List[String]] = {
      tokens.indexOf("|") match {
        case -1 => List(tokens)
        case p  => tokens.take(p) +: splitAtPipe(tokens.drop(p + 1))
      }
    }
    val splitted = splitAtPipe(str.split(" ").toList)
    if (splitted.contains(Nil) || splitted.isEmpty) None
    else {
      Some(
        splitted
          .map { case command :: args =>
            SimpleCommand(command, args.toArray)
          }
          .reduceRight[Command](PipeCommand(_, _))
      )
    }
  }

  def readUserInput(): Option[Command] = {
    print("> ")
    parse(readLine())
  }

}
