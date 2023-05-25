package scala.cli.directivehandler

case class Diagnostic(
  message: String,
  severity: Severity,
  positions: Seq[Position] = Nil
)
