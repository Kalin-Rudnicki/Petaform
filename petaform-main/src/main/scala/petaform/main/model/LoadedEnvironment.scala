package petaform.main.model

import cats.syntax.option.*
import harness.zio.*
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

  def fromPartiallyLoadedEnvironment(partial: PartiallyLoadedEnvironment, configPath: Path, envVars: Map[String, String]): SHTask[LoadedEnvironment] =
    for {
      rawConfigASTs <- ZIO.foreach(partial.configPaths)(configPath.child(_).flatMap(ParseRawAST.fromPath))
      environment <- Errors.scopedToTask(convert(rawConfigASTs, partial, envVars))
    } yield environment

  // =====|  |=====

  private def convert(
      rawConfigASTs: List[RawPetaformAST],
      partial: PartiallyLoadedEnvironment,
      envVars: Map[String, String],
  ): Either[ScopedError, LoadedEnvironment] =
    for {
      rawConfigAST <- RawPetaformAST.merge(rawConfigASTs)
      configAST <- RawASTToAST(rawConfigAST, envVars, None)
      environmentAST <- RawASTToAST(partial.rawPetaformAST, envVars, configAST.some)
      environment <- environmentAST.decodeTo[SingleKeyMap[Environment]]
    } yield LoadedEnvironment(partial.envName, environment.value, configAST)

}
