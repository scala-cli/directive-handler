package scala.cli.directivehandler.tests

import scala.cli.directivehandler._

object DirectiveTestsDefinitions {

  @DirectiveUsage("//> thing", "//> thing but markdown")
  @DirectiveDescription("desc", "markdown desc")
  @DirectiveExamples("//> using prefixx.thing a")
  @DirectiveExamples("//> using prefixx.thing b")
  @DirectiveExamples("//> using prefixx.thing c")
  @DirectiveExamples("//> using prefixx.thing a b c")
  @DirectiveGroupName("the name")
  @DirectiveTag("tag1")
  @DirectiveTag("tag2")
  @DirectivePrefix("prefixx.")
  case class Directives(
    thing: List[String] = Nil,
    @DirectiveName("other")
    enableTheOtherThing: Option[Boolean] = None
  )

}
