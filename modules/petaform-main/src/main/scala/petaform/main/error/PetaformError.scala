package petaform.main.error

import cats.data.NonEmptyList
import harness.core.*
import harness.zio.error.SysError
import petaform.model.ScopedError
import petaform.parsing.error.ParseError
import slyce.core.{Marked, Source}

sealed trait PetaformError extends Throwable {

  override final def getMessage: String = this match {
    case PetaformError.MarkedErr(error)                            => Source.markAll(error.toList)
    case PetaformError.ScopedErr(error)                            => error.toString
    case PetaformError.NoSuchEnvironment(environment)              => s"No such environment: $environment"
    case PetaformError.NoSuchConfigGroup(environment, configGroup) => s"No such config-group: $environment -> $configGroup"
    case PetaformError.Generic(cause)                              => cause.safeGetMessage
    case PetaformError.SysCommandError(cause)                      => s"Error running command system command: ${cause.safeGetMessage}"
  }

}
object PetaformError {

  final case class MarkedErr(error: NonEmptyList[Marked[String]]) extends PetaformError

  final case class ScopedErr(error: ScopedError) extends PetaformError

  final case class NoSuchEnvironment(environment: String) extends PetaformError

  final case class NoSuchConfigGroup(environment: String, configGroup: String) extends PetaformError

  final case class Generic(cause: Throwable) extends PetaformError

  final case class SysCommandError(cause: SysError) extends PetaformError

  def fromParseError(error: ParseError): PetaformError =
    error match {
      case ParseError.MarkedErr(error) => PetaformError.MarkedErr(error)
      case ParseError.ScopedErr(error) => PetaformError.ScopedErr(error)
      case ParseError.Generic(cause)   => PetaformError.Generic(cause)
    }

}
