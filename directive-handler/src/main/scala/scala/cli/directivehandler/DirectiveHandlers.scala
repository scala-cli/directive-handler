package scala.cli.directivehandler

import scala.cli.directivehandler.EitherSequence._

final case class DirectiveHandlers[T](
  handlers: Seq[DirectiveHandler[T]],
  customHandler: String => Option[DirectiveHandler[T]] = (key: String) => None
) {

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
        val key = dir.directive.key
        handlersMap.get(key).orElse(customHandler(key)) match {
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
    copy(
      handlers = handlers.map(_.map(f)),
      customHandler = key => customHandler(key).map(_.map(f))
    )

  def mapDirectives[U](f: DirectiveHandler[T] => DirectiveHandler[U]): DirectiveHandlers[U] =
    copy(
      handlers = handlers.map(f),
      customHandler = key => customHandler(key).map(f)
    )

  def ++(other: DirectiveHandlers[T]): DirectiveHandlers[T] =
    if (other.handlers.isEmpty) this
    else if (handlers.isEmpty) other
    else DirectiveHandlers(handlers ++ other.handlers)

  def addCustomHandler(f: String => Option[DirectiveHandler[T]]): DirectiveHandlers[T] =
    copy(
      customHandler = key => customHandler(key).orElse(f(key))
    )

}
