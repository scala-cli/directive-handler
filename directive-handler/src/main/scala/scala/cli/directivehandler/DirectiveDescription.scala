package scala.cli.directivehandler

import scala.annotation.StaticAnnotation

final case class DirectiveDescription(description: String, descriptionMd: String = "")
    extends StaticAnnotation
