package scala.cli.directivehandler

final class DependencyFormatError(
  val dependencyString: String,
  val error: String,
  val originOpt: Option[String] = None,
  positionOpt: Option[Position] = None
) extends DirectiveException(
      s"Error parsing ${originOpt.getOrElse("")}dependency '$dependencyString': $error",
      positions = positionOpt.toSeq
    )
