package scala.cli.directivehandler

final case class ProcessedDirective[+T](global: Option[T], scoped: Seq[Scoped[T]]) {
  def map[U](f: T => U): ProcessedDirective[U] =
    ProcessedDirective(global.map(f), scoped.map(_.map(f)))
}
