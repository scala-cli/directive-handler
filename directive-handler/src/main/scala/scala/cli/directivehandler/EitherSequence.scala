package scala.cli.directivehandler

import scala.collection.mutable.ListBuffer

object EitherSequence {
  implicit class EitherSeqOps[E, T](private val seq: Seq[Either[E, T]]) extends AnyVal {
    def sequence: Either[::[E], Seq[T]] =
      EitherSequence.sequence(seq)
  }

  def sequence[E, T](eithers: Seq[Either[E, T]]): Either[::[E], Seq[T]] = {
    val errors = new ListBuffer[E]
    val values = new ListBuffer[T]
    eithers.foreach {
      case Left(e) => errors += e
      case Right(t) =>
        if (errors.isEmpty)
          values += t
    }
    errors.result() match {
      case Nil    => Right(values.result())
      case h :: t => Left(::(h, t))
    }
  }
}
