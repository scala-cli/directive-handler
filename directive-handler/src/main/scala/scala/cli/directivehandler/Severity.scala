package scala.cli.directivehandler

sealed abstract class Severity extends Product with Serializable

object Severity {
  case object Error   extends Severity
  case object Warning extends Severity
}
