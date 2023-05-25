package scala.cli.directivehandler

abstract class DirectiveException(
  message: String,
  val positions: Seq[Position] = Nil,
  cause: Throwable = null
) extends Exception(message, cause)
