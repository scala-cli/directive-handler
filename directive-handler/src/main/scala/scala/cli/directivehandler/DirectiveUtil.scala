package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.{BooleanValue, EmptyValue, StringValue, Value}
import dependency.AnyDependency
import dependency.parser.DependencyParser

import scala.cli.directivehandler.EitherSequence._

object DirectiveUtil {

  def position(
    v: Value[_],
    path: Either[String, os.Path],
    skipQuotes: Boolean = false
  ): Position.File = {
    val line       = v.getRelatedASTNode.getPosition.getLine
    val column     = v.getRelatedASTNode.getPosition.getColumn + (if (skipQuotes) 1 else 0)
    val endLinePos = column + v.toString.length
    Position.File(path, (line, column), (line, endLinePos))
  }

  def concatAllValues(
    scopedDirective: ScopedDirective
  ): Seq[Positioned[String]] =
    scopedDirective.directive.values.map {
      case v: StringValue =>
        val pos = position(v, scopedDirective.maybePath, skipQuotes = true)
        Positioned(pos, v.get)
      case v: BooleanValue =>
        val pos = position(v, scopedDirective.maybePath, skipQuotes = false)
        Positioned(pos, v.get.toString)
      case v: EmptyValue =>
        val pos = position(v, scopedDirective.maybePath, skipQuotes = false)
        Positioned(pos, v.get)
    }

  extension (deps: List[Positioned[String]]) {
    def asDependencies: Either[DirectiveException, Seq[Positioned[AnyDependency]]] =
      deps
        .map {
          _.map { str =>
            DependencyParser.parse(str).left.map(new DependencyFormatError(str, _))
          }.eitherSequence
        }
        .sequence
        .left.map(CompositeDirectiveException(_))
  }
}
