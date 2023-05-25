package scala.cli.directivehandler

import scala.annotation.StaticAnnotation

final case class DirectiveUsage(usage: String, usageMd: String = "") extends StaticAnnotation
