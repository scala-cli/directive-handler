package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.UsingDirectives
import com.virtuslab.using_directives.custom.utils.ast.UsingDefs

import scala.jdk.CollectionConverters.*

object UsingDirectivesOps {
  extension (ud: UsingDirectives) {
    def keySet: Set[String] = ud.getFlattenedMap.keySet().asScala.map(_.toString).toSet
    def containsTargetDirectivesOnly: Boolean = keySet.forall(_.toString.startsWith("target."))

    def getPosition(path: Either[String, os.Path]): Position.File =
      val line   = ud.getAst().getPosition().getLine()
      val column = ud.getAst().getPosition().getColumn()
      Position.File(path, (0, 0), (line, column))
  }
}
