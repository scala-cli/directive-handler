package scala.cli.directivehandler

import scala.cli.directivehandler.EitherSequence._

final case class DirectiveHandlers[T](handlers: Seq[DirectiveHandler[T]]) {

  private lazy val handlersMap = handlers.flatMap(h => h.keys.map(_ -> h)).toMap

  def parse(
    input: String,
    path: Either[String, os.Path],
    scopePath: ScopePath
  ): Either[DirectiveException, Seq[ProcessedDirective[T]]] = {

    val directives = ExtractedDirectives.from(input.toCharArray, path)
      .fold(e => throw e, identity)
      .directives
      .map(dir => ScopedDirective(dir, path, scopePath))

    directives
      .map { dir =>
        handlersMap.get(dir.directive.key) match {
          case Some(h) =>
            h.handleValues(dir)
          case None =>
            Left(dir.unusedDirectiveError)
        }
      }
      .sequence
      .left.map(CompositeDirectiveException(_))
  }

  def map[U](f: T => U): DirectiveHandlers[U] =
    copy(handlers = handlers.map(_.map(f)))

  def mapDirectives[U](f: DirectiveHandler[T] => DirectiveHandler[U]): DirectiveHandlers[U] =
    copy(handlers = handlers.map(f))

  def ++(other: DirectiveHandlers[T]): DirectiveHandlers[T] =
    if (other.handlers.isEmpty) this
    else if (handlers.isEmpty) other
    else DirectiveHandlers(handlers ++ other.handlers)

}
