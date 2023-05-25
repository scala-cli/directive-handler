package scala.cli.directivehandler

import scala.annotation.StaticAnnotation

final case class DirectivePrefix(prefix: String)
    extends StaticAnnotation
