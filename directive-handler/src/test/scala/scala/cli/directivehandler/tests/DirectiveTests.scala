//> using dep "org.scalameta::munit:0.7.29"
//> using dep "com.eed3si9n.expecty::expecty:0.16.0"
//> using dep "com.lihaoyi::pprint:0.8.1"

package scala.cli.directivehandler.tests

import com.eed3si9n.expecty.Expecty.expect

import scala.cli.directivehandler._
import scala.cli.directivehandler.EitherSequence._

class DirectiveTests extends munit.FunSuite {

  import DirectiveTestsDefinitions._

  test("simple") {

    /*

LabelledGeneric[DirectiveTests.Directives]{
  type Repr =
    List[String] with KeyTag[Symbol with tag.Tagged[String("thing")], List[String]] ::
      Option[Boolean] with KeyTag[Symbol with tag.Tagged[String("enableTheOtherThing")], Option[Boolean]] ::
      HNil
}

LabelledGeneric.Aux[
  DirectiveTests.Directives,
  FieldType[String("thing"), List[String]] ::
    FieldType[String("enableTheOtherThing"), Option[Boolean]] ::
      HNil
]

     */

    /*import shapeless._
    import shapeless.labelled._
    val l: DirectiveHandlerMacros.HListDirectiveHandlerBuilder[
      FieldType[Witness.`'thing`.T ,List[String]] ::
        FieldType[Witness.`'enableTheOtherThing`.T ,Option[Boolean]] ::
          shapeless.HNil,
      Option[List[String]] :: Option[Option[Boolean]] :: shapeless.HNil,
      Nil.type :: scala.::[scala.cli.directivehandler.DirectiveName] :: shapeless.HNil
    ] = implicitly*/

    val handler: DirectiveHandler[Directives] = DirectiveHandler.deriver[Directives].derive /*[
      Directives,
      FieldType[Witness.`'thing`.T, List[String]] :: FieldType[Witness.`'enableTheOtherThing`.T, Option[Boolean]] :: HNil,
      Option[List[String]] :: Option[Option[Boolean]] :: HNil,
      Nil.type :: scala.::[DirectiveName] :: HNil
    ](
      DirectiveTests.labelledGen,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly,
      implicitly
    )*/

    expect(handler.usage == "//> thing")
    expect(handler.usageMd == "//> thing but markdown")
    expect(handler.description == "desc")
    expect(handler.descriptionMd == "markdown desc")

    if (DirectiveTestsScalaVersionUtil.isScala2)
      expect(handler.examples.headOption == Some("//> using prefixx.thing a"))
    else
      expect(handler.examples == Seq("a", "b", "c", "a b c").map("//> using prefixx.thing " + _))

    expect(handler.keys == Seq(
      "prefixx.thing",
      "prefixx.enableTheOtherThing",
      "prefixx.enable-the-other-thing",
      "prefixx.other"
    ))
    expect(handler.name == "the name")

    if (DirectiveTestsScalaVersionUtil.isScala2)
      expect(handler.tags.headOption == Some("tag1"))
    else
      expect(handler.tags == Seq("tag1", "tag2"))

    val input =
      """//> using prefixx.thing foo aa bb
        |//> using prefixx.other
        |""".stripMargin

    val path      = Left("script.sc")
    val scopePath = ScopePath(Left("."), os.sub)

    val expectedRes = Seq(
      ProcessedDirective(Some(Directives(thing = List("foo", "aa", "bb"))), Nil),
      ProcessedDirective(Some(Directives(enableTheOtherThing = Some(true))), Nil)
    )

    val res = handler.parse(input, path, scopePath)
      .fold(e => throw new Exception(e), identity)

    if (res != expectedRes) {
      pprint.err.log(res)
      pprint.err.log(expectedRes)
    }
    expect(res == expectedRes)
  }

}
