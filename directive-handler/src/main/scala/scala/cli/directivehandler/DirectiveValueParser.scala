package scala.cli.directivehandler

import com.virtuslab.using_directives.custom.model.{BooleanValue, EmptyValue, StringValue, Value}

abstract class DirectiveValueParser[+T] {

  def parse(scopedDirective: ScopedDirective): Either[DirectiveException, T] =
    parse(
      scopedDirective.directive.values,
      scopedDirective.cwd,
      scopedDirective.maybePath
    )

  def parse(
    values: Seq[Value[_]],
    scopePath: ScopePath,
    path: Either[String, os.Path]
  ): Either[DirectiveException, T]

  final def map[U](f: T => U): DirectiveValueParser[U] =
    new DirectiveValueParser.Mapped[T, U](this, f)
}

object DirectiveValueParser {

  private final class Mapped[T, +U](underlying: DirectiveValueParser[T], f: T => U)
      extends DirectiveValueParser[U] {
    override def parse(scopedDirective: ScopedDirective): Either[DirectiveException, U] =
      underlying.parse(scopedDirective).map(f)
    def parse(
      values: Seq[Value[_]],
      scopePath: ScopePath,
      path: Either[String, os.Path]
    ): Either[DirectiveException, U] =
      underlying.parse(values, scopePath, path).map(f)
  }

  abstract class DirectiveSingleValueParser[+T] extends DirectiveValueParser[T] {
    def parseValue(
      value: Value[_],
      cwd: ScopePath,
      path: Either[String, os.Path]
    ): Either[DirectiveException, T]

    final def parse(
      values: Seq[Value[_]],
      scopePath: ScopePath,
      path: Either[String, os.Path]
    ): Either[DirectiveException, T] =
      values.filter(!_.isEmpty) match {
        case Seq(value) => parseValue(value, scopePath, path)
        case _ =>
          Left(
            new UsingDirectiveValueNumError(
              path,
              "",
              "1",
              values.length
            )
          )
      }
  }

  implicit lazy val unit: DirectiveValueParser[Unit] = { (values, scopePath, path) =>
    values match {
      case Seq() => Right(())
      case Seq(value, _*) =>
        val pos = value.position(path)
        Left(new MalformedDirectiveError("Expected no value in directive", Seq(pos)))
    }
  }

  implicit class DirectiveValueParserValueOps(private val value: Value[_]) extends AnyVal {

    def isEmpty: Boolean =
      value match {
        case _: EmptyValue => true
        case _             => false
      }

    def isString: Boolean =
      value match {
        case _: StringValue => true
        case _              => false
      }
    def asString: Option[String] =
      value match {
        case s: StringValue => Some(s.get())
        case _              => None
      }
    def isBoolean: Boolean =
      value match {
        case _: BooleanValue => true
        case _               => false
      }
    def asBoolean: Option[Boolean] =
      value match {
        case s: BooleanValue => Some(s.get())
        case _               => None
      }

    def position(path: Either[String, os.Path]): Position =
      DirectiveUtil.position(value, path, skipQuotes = isString)
  }

  implicit lazy val boolean: DirectiveValueParser[Boolean] = { (values, scopePath, path) =>
    values.filter(!_.isEmpty) match {
      case Seq() => Right(true)
      case Seq(v) =>
        v.asBoolean.toRight {
          new UsingDirectiveWrongValueTypeError(
            path,
            "",
            Seq("boolean"),
            ""
          )
        }
      case values0 =>
        Left(
          new MalformedDirectiveError(
            s"Unexpected values ${values0.map(_.toString).mkString(", ")}",
            values0.map(_.position(path))
          )
        )
    }
  }

  implicit lazy val string: DirectiveSingleValueParser[String] =
    (value, scopePath, path) =>
      value.asString.toRight {
        val pos = value.position(path)
        new MalformedDirectiveError(
          s"Expected a string, got '${value.getRelatedASTNode.toString}'",
          Seq(pos)
        )
      }.map(DirectiveSpecialSyntax.handlingSpecialPathSyntax(_, path))

  final case class MaybeNumericalString(value: String)

  implicit lazy val maybeNumericalString: DirectiveSingleValueParser[MaybeNumericalString] =
    (value, scopePath, path) =>
      value.asString.map(MaybeNumericalString(_)).toRight {
        val pos = value.position(path)
        new MalformedDirectiveError(
          s"Expected a string value, got '${value.getRelatedASTNode.toString}'",
          Seq(pos)
        )
      }

  final case class WithScopePath[+T](scopePath: ScopePath, value: T)

  object WithScopePath {
    def empty[T](value: T): WithScopePath[T] =
      WithScopePath(ScopePath(Left("invalid"), os.sub), value)
  }

  implicit def withScopePath[T](implicit
    underlying: DirectiveValueParser[T]
  ): DirectiveValueParser[WithScopePath[T]] = {
    (values, scopePath, path) =>
      underlying.parse(values, scopePath, path)
        .map(WithScopePath(scopePath, _))
  }
  implicit def positioned[T](implicit
    underlying: DirectiveSingleValueParser[T]
  ): DirectiveSingleValueParser[Positioned[T]] = {
    (value, scopePath, path) =>
      underlying.parseValue(value, scopePath, path)
        .map(Positioned(value.position(path), _))
  }
  implicit def option[T](implicit
    underlying: DirectiveValueParser[T]
  ): DirectiveValueParser[Option[T]] =
    underlying.map(Some(_))
  implicit def list[T](implicit
    underlying: DirectiveSingleValueParser[T]
  ): DirectiveValueParser[List[T]] = {
    (values, scopePath, path) =>
      val res = values.filter(!_.isEmpty).map(underlying.parseValue(_, scopePath, path))
      val errors = res.collect {
        case Left(e) => e
      }
      if (errors.isEmpty) Right(res.collect { case Right(v) => v }.toList)
      else Left(CompositeDirectiveException(errors))
  }

}
