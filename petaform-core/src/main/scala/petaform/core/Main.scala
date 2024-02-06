package petaform.core

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import zio.*

object Main extends ExecutableApp {

  private val petaformDirParser: Parser[String] =
    Parser.value[String](LongName.unsafe("petaform-dir")).default("petaform")

  private final case class Cfg(
      petaformDir: String,
  )
  private object Cfg {

    val parser: Parser[Cfg] = {
      petaformDirParser
    }.map { Cfg.apply }

  }

  private def rawAstToMap(rawAst: RawPetaformAST, rScope: List[ASTScope]): Either[ScopedError, Map[String, RawPetaformAST]] =
    rawAst match {
      case RawPetaformAST.Obj(elems) =>
        elems
          .traverse {
            case (key, RawPetaformAST.Obj.Value.Provided(_, value)) => (key, value).asRight
            case (key, RawPetaformAST.Obj.Value.Required)           => ScopedError((ASTScope.Key(key) :: rScope).reverse, "Can not convert map with `@required` value").asLeft
          }
          .map(_.toMap)
      case _ => ScopedError(rScope.reverse, s"Expected Object, got ${rawAst.getClass.getSimpleName}").asLeft
    }

  private def rawAstToMapMap(rawAst: RawPetaformAST): Either[ScopedError, Map[String, Map[String, RawPetaformAST]]] =
    rawAstToMap(rawAst, Nil).flatMap { map =>
      map.toList.traverse { case (key, value) => rawAstToMap(value, ASTScope.Key(key) :: Nil).map((key, _)) }.map(_.toMap)
    }

  private def filterMapMap(env: String, resourcesMap: Map[String, Map[String, RawPetaformAST]], resources: Map[String, String]): HTask[RawPetaformAST] =
    if (resources.isEmpty)
      ZIO.fail(HError.UserError(s"Resources map is empty for env '$env'"))
    else
      Errors.scopedToTask {
        resources.toList
          .traverse { case (resourceName, variantName) =>
            for {
              resourceMap <- resourcesMap.get(resourceName).toRight(ScopedError(List(ASTScope.Key(resourceName)), "Missing resource"))
              value <- resourceMap.get(variantName).toRight(ScopedError(List(ASTScope.Key(resourceName), ASTScope.Key(variantName)), "Missing variant"))
              obj1 = RawPetaformAST.Obj.makeUnsafe(variantName -> RawPetaformAST.Obj.Value.Provided(true, value))
              obj2 = RawPetaformAST.Obj.makeUnsafe(resourceName -> RawPetaformAST.Obj.Value.Provided(true, obj1))
            } yield obj2
          }
          .flatMap(RawPetaformAST.merge(_))
      }

  private def getEnvs(petaformPathString: String): SHTask[List[Parts.Built]] =
    for {
      _ <- Logger.log.info("Calculating envs")
      _ <- Logger.log.detailed(s"petaform-dir: ${cfg.petaformDir}")

      // TODO (KR) : ensure exists
      petaformPath <- Path(petaformPathString)
      environmentsPath <- petaformPath.child("environments.conf")
      resourcesPath <- petaformPath.child("resources.conf")
      configPath <- petaformPath.child("config")

      envVars <- System.envs.mapError(HError.fromThrowable)

      // TODO (KR) : manually grab config paths, and allow interpolation from specified configs for rest of file
      environmentsRawAst <- RawPetaformAST.fromPath(environmentsPath)
      environmentsAst <- Errors.scopedToTask(PetaformAST.fromRaw(environmentsRawAst, envVars, PetaformAST.Obj().some))
      environments <- Errors.scopedToTask(environmentsAst.decodeTo[Parts.Environments])

      resourcesRawAst <- RawPetaformAST.fromPath(resourcesPath)
      resourceMapMap <- Errors.scopedToTask(rawAstToMapMap(resourcesRawAst))

      allConfigPaths = environments.environments.toList.flatMap(_._2.configs).distinct.sorted
      configPathMap <-
        ZIO
          .foreach(allConfigPaths) { pathString =>
            for {
              path <- configPath.child(pathString)
              rawAst <- RawPetaformAST.fromPath(path)
            } yield (pathString, rawAst)
          }
          .map(_.toMap)

      envs <- ZIO.foreach(environments.environments.toList) { case (envName, env) =>
        for {
          rawConfigAst <- Errors.scopedToTask(RawPetaformAST.merge(env.configs.map(configPathMap(_))))
          configAst <- Errors.scopedToTask(PetaformAST.fromRaw(rawConfigAst, envVars, None))
          _ <- Logger.log.debug(s"[:  $envName  -  config  :]\n${Formatting.ast(configAst)}")
          rawResourceAst <- filterMapMap(envName, resourceMapMap, env.resources)
          petaformEnvVars = Map("PETAFORM_ENV" -> envName)
          resourceAst <- Errors.scopedToTask(PetaformAST.fromRaw(rawResourceAst, envVars ++ petaformEnvVars, configAst.some))
          _ <- Logger.log.debug(s"[:  $envName  -  resources  :]\n${Formatting.ast(resourceAst)}")
          resources <- Errors.scopedToTask(resourceAst.decodeTo[Parts.ResourceGroups])
        } yield Parts.Built(envName, env, resources)
      }
    } yield envs

  private val init: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform init")
          envs <- getEnvs(cfg.petaformDir)
          _ <- ZIO.foreachDiscard(envs) { env =>
            for {
              terraform <- TerraformAST.fromBuilt(env)
              _ <- Logger.log.debug(s"[:  ${env.name}  -  terraform  :]\n${Formatting.terraformAST(terraform)}")
            } yield ()
          }
        } yield ()
      }

  override val executable: Executable =
    Executable.fromSubCommands(
      "init" -> init,
    )

}
