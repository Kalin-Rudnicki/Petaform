package petaform.main.model

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.zio.*
import petaform.main.error.PetaformError
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*
import petaform.model.repr.*
import zio.*

final case class BuiltEnvironment private (
    env: LoadedEnvironment,
    resourceGroups: ResourceGroups,
    envVars: EnvVars,
    terraformString: String,
)
object BuiltEnvironment {

  def build(partialResources: PartialResources, environment: LoadedEnvironment, envVars: EnvVars): Either[ScopedError, BuiltEnvironment] =
    for {
      rawFilteredResourcesAST <- filterVariants(partialResources, environment.envName, environment.environment.resources)
      resourcesAST <- RawASTToAST(rawFilteredResourcesAST, envVars, environment.config.some)
      resourceGroups <- resourcesAST.decodeTo[ResourceGroups]
      terraformASTs <- ASTToTerraform(resourceGroups)
    } yield BuiltEnvironment(environment, resourceGroups, envVars, FormatTerraform(terraformASTs))

  def build(
      partialResources: PartialResources,
      environment: PartiallyLoadedEnvironment,
      configPath: Path,
      configGroups: List[String],
      envVars: EnvVars,
  ): ZIO[HarnessEnv, PetaformError, BuiltEnvironment] =
    for {
      loadedEnvironment <- LoadedEnvironment.fromPartiallyLoadedEnvironment(environment, configPath, configGroups, envVars)
      builtEnvironment <- ZIO.fromEither(build(partialResources, loadedEnvironment, envVars)).mapError(PetaformError.ScopedErr(_))
    } yield builtEnvironment

  // =====|  |=====

  private def filterVariants(partialResources: PartialResources, env: String, resources: Map[String, String]): Either[ScopedError, RawPetaformAST] =
    if (resources.isEmpty)
      ScopedError(ScopePath.inOrder(List(ASTScope.Key(env), ASTScope.Key("resources"))), "resources are empty").asLeft
    else
      resources.toList
        .traverse { case (resourceName, variantName) =>
          for {
            variantAST <- partialResources.getRawVariantAST(resourceName, variantName)
            obj1 = RawPetaformAST.Obj.makeUnsafe(variantName -> RawPetaformAST.Obj.Value.Provided(true, variantAST))
            obj2 = RawPetaformAST.Obj.makeUnsafe(resourceName -> RawPetaformAST.Obj.Value.Provided(true, obj1))
          } yield obj2
        }
        .flatMap(RawPetaformAST.merge(_))

}
