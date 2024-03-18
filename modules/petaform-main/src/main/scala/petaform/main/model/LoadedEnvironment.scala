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

  def fromPartiallyLoadedEnvironment(partial: PartiallyLoadedEnvironment, configPath: Path, configGroups: List[String], envVars: EnvVars): ZIO[HarnessEnv, PetaformError, LoadedEnvironment] =
    for {
      allPaths <- ZIO.foreach(configGroups)(getConfigPaths(partial, _))
      rawConfigASTs <-
        ZIO.foreach(allPaths.flatten.distinct) {
          configPath
            .child(_)
            .mapError(PetaformError.Generic(_))
            .flatMap(ParseRawAST.fromPath(_).mapError(PetaformError.fromParseError))
        }
      environment <- ZIO.fromEither(convert(rawConfigASTs, partial, envVars)).mapError(PetaformError.ScopedErr(_))
    } yield environment

  // =====|  |=====

  private def getConfigPaths(partial: PartiallyLoadedEnvironment, configGroup: String): IO[PetaformError.NoSuchConfigGroup, List[String]] =
    ZIO.getOrFailWith(PetaformError.NoSuchConfigGroup(partial.envName, configGroup))(partial.configPaths.get(configGroup))

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
      // TODO (KR) : filter out config-groups that are not included?
    } yield LoadedEnvironment(partial.envName, environment.value, configAST)

}
