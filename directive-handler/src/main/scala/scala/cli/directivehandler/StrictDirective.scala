package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.{StringValue, Value}

case class StrictDirective(
  key: String,
  values: Seq[Value[_]]
) {
  override def toString: String = {
    val suffix = if values.isEmpty then "" else s" \"${values.mkString("\",  \"")}\""
    s"//> using $key$suffix"
  }
  def stringValuesCount: Int =
    values.count {
      case _: StringValue => true
      case _              => false
    }
}
