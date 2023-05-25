package scala.cli.directivehandler

final class MalformedDirectiveError(message: String, positions: Seq[Position])
    extends DirectiveException(message, positions)
