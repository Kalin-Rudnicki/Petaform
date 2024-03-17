package petaform.main.model

import cats.syntax.option.*
import harness.zio.*
import petaform.main.error.PetaformError
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*
import petaform.model.repr.Environment
import petaform.parsing.*
import zio.*

final case class LoadedEnvironment private (
    envName: String,
    environment: Environment,
    config: PetaformAST,
)
object LoadedEnvironment {

  def fromPartiallyLoadedEnvironment(partial: PartiallyLoadedEnvironment, configPath: Path, envVars: EnvVars): ZIO[HarnessEnv, PetaformError, LoadedEnvironment] =
    for {
      rawConfigASTs <-
        ZIO.foreach(partial.configPaths) {
          configPath
            .child(_)
            .mapError(PetaformError.Generic(_))
            .flatMap(ParseRawAST.fromPath(_).mapError(PetaformError.fromParseError))
        }
      environment <- ZIO.fromEither(convert(rawConfigASTs, partial, envVars)).mapError(PetaformError.ScopedErr(_))
    } yield environment

  // =====|  |=====

  private def convert(
      rawConfigASTs: List[RawPetaformAST],
      partial: PartiallyLoadedEnvironment,
      envVars: EnvVars,
  ): Either[ScopedError, LoadedEnvironment] =
    for {
      rawConfigAST <- RawPetaformAST.merge(rawConfigASTs)
      configAST <- RawASTToAST(rawConfigAST, envVars, None)
      environmentAST <- RawASTToAST(partial.rawPetaformAST, envVars, configAST.some)
      environment <- environmentAST.decodeTo[SingleKeyMap[Environment]]
    } yield LoadedEnvironment(partial.envName, environment.value, configAST)

}
