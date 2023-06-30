package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.{StringValue, Value}

case class StrictDirective(
  key: String,
  values: Seq[Value[_]]
) {
  override def toString: String = {
    val q      = "\""
    val suffix = if (values.isEmpty) "" else s" $q${values.mkString("\",  \"")}$q"
    s"//> using $key$suffix"
  }
  def stringValuesCount: Int =
    values.count {
      case _: StringValue => true
      case _              => false
    }
}
