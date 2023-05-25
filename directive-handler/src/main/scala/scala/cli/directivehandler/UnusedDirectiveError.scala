package scala.cli.directivehandler

final class UnusedDirectiveError(key: String, values: Seq[String], positions: Seq[Position])
    extends DirectiveException(
      s"Unrecognized directive: $key with values: ${values.mkString(", ")}",
      positions = positions
    )
