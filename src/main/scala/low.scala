import scalanative.posix.unistd
import scala.scalanative.unsafe._
import scala.scalanative.posix._

sealed trait Fork
case object Child extends Fork
case class Parent(child: Int) extends Fork

case class Pipe(reader: CInt, writer: CInt)

object Low {

  def fork(): Fork = {
    unistd.fork() match {
      case 0          => Child
      case n if n < 0 => throw new Exception()
      case n          => Parent(n)
    }
  }

  def waitpid(pid: CInt): Unit = {
    Zone { implicit z =>
      val wstatus = alloc[CInt]
      stuff.waitpid(pid, wstatus, 0)
    }
  }

  def pipe(): Pipe = {
    Zone { implicit z =>
      val pipes = alloc[CInt](2)
      unistd.pipe(pipes)
      Pipe(pipes(0), pipes(1))
    }
  }

  def dup2(field: CInt, fieldsnew: CInt): Unit = {
    unistd.dup2(field, fieldsnew)
  }

  def close(field: CInt): Unit = {
    unistd.close(field)
  }

  @extern
  private object stuff {
    def execvp(file: CString, args: Ptr[CString]): CInt = extern
    def waitpid(pid: CInt, wstatus: Ptr[CInt], options: CInt): CInt = extern
    def setpgid(pid: CInt, gpid: CInt): CInt = extern
    def tcsetpgrp(fd: CInt, pgrp: CInt): CInt = extern
  }

  implicit def stringIsCString(str: String)(implicit zone: Zone): CString =
    toCString(str)

  implicit def ArrayStringIsCStringArray(
      itr: Array[String]
  )(implicit zone: Zone): Ptr[CString] = {
    itr.toList
  }

  implicit def StringIterableIsCStringArray[CC[X] <: Iterable[X]](
      itr: CC[String]
  )(implicit zone: Zone): Ptr[CString] = {
    val length = itr.size
    val ptr: Ptr[CString] = alloc[CString](length + 1)
    for ((str, index) <- itr.zipWithIndex) {
      ptr(index) = toCString(str)
    }
    ptr(length) = null
    ptr
  }

  def exec(command: String, args: Array[String]): Boolean = {
    Zone { implicit z =>
      stuff.execvp(command, command +: args)
      true
    }
  }

  def ignoreTtySignals(): Unit = {
    import scala.scalanative.libc.signal._
    signal(SIGINT, SIG_IGN)
  }

  def restoreTtySignals(): Unit = {
    import scala.scalanative.libc.signal._
    signal(SIGINT, SIG_DFL)
  }

}
