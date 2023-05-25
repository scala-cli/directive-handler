package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.utils.{Position => DirectivePosition}
import com.virtuslab.using_directives.reporter.Reporter

class CustomDirectivesReporter(path: Either[String, os.Path], onDiagnostic: Diagnostic => Unit)
    extends Reporter {

  private var errorCount = 0

  private def toScalaCliPosition(position: DirectivePosition): Position = {
    val coords = (position.getLine, position.getColumn)
    Position.File(path, coords, coords)
  }

  override def error(msg: String): Unit =
    onDiagnostic {
      errorCount += 1
      Diagnostic(msg, Severity.Error)
    }
  override def error(position: DirectivePosition, msg: String): Unit =
    onDiagnostic {
      errorCount += 1
      Diagnostic(msg, Severity.Error, Seq(toScalaCliPosition(position)))
    }
  override def warning(msg: String): Unit =
    onDiagnostic {
      Diagnostic(msg, Severity.Warning)
    }
  override def warning(position: DirectivePosition, msg: String): Unit =
    onDiagnostic {
      Diagnostic(msg, Severity.Warning, Seq(toScalaCliPosition(position)))
    }

  override def hasErrors(): Boolean =
    errorCount != 0

  override def reset(): Unit = {
    errorCount = 0
  }
}

object CustomDirectivesReporter {
  def create(path: Either[String, os.Path])(onDiagnostic: Diagnostic => Unit)
    : CustomDirectivesReporter =
    new CustomDirectivesReporter(path, onDiagnostic)
}
