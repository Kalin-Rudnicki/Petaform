package petaform.core

import harness.core.*
import harness.zio.*
import slyce.core.*
import zio.*

object Errors {

  def scopedToTask[A](either: Either[ScopedError, A]): HTask[A] =
    either match {
      case Right(value) => ZIO.succeed(value)
      case Left(error)  => ZIO.fail(HError.UserError(error.toString))
    }

  def validatedToTask[A](validated: Validated[A]): HTask[A] =
    validated match {
      case Right(value) => ZIO.succeed(value)
      case Left(errors) => ZIO.fail(HError.UserError(Source.markAll(errors.toList)))
    }

}
