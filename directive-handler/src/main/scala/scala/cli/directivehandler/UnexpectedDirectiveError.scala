package scala.cli.directivehandler

final class UnexpectedDirectiveError(val key: String)
    extends DirectiveException(s"Unexpected directive: $key}")
