package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.Value

final case class IgnoredDirective(directive: ScopedDirective)

object IgnoredDirective {

  implicit val valueParser: DirectiveValueParser[IgnoredDirective] =
    new DirectiveValueParser[IgnoredDirective] {
      override def parse(scopedDirective: ScopedDirective)
        : Either[DirectiveException, IgnoredDirective] =
        Right(IgnoredDirective(scopedDirective))
      def parse(
        values: Seq[Value[_]],
        scopePath: ScopePath,
        path: Either[String, os.Path]
      ): Either[DirectiveException, IgnoredDirective] =
        Right(
          IgnoredDirective(
            ScopedDirective(
              StrictDirective("", values),
              path,
              scopePath
            )
          )
        )
    }

}
