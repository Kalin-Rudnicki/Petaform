package petaform.parsing.error

import cats.data.NonEmptyList
import harness.core.*
import petaform.model.ScopedError
import slyce.core.{Marked, Source}

sealed trait ParseError extends Throwable {

  override final def getMessage: String = this match {
    case ParseError.MarkedErr(error) => Source.markAll(error.toList)
    case ParseError.ScopedErr(error) => error.toString
    case ParseError.Generic(cause)   => cause.safeGetMessage
  }

}
object ParseError {
  final case class MarkedErr(error: NonEmptyList[Marked[String]]) extends ParseError
  final case class ScopedErr(error: ScopedError) extends ParseError
  final case class Generic(cause: Throwable) extends ParseError
}
