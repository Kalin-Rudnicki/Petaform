package petaform.main

import cats.syntax.traverse.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import petaform.main.model.*
import petaform.model.*
import petaform.model.conversion.*
import petaform.model.repr.*
import petaform.model.typeclass.*
import petaform.parsing.*
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

  // =====|  |=====

  private def getEnvs(petaformPathString: String): SHTask[List[BuiltEnvironment]] =
    for {
      _ <- Logger.log.info("Calculating envs")
      _ <- Logger.log.detailed(s"petaform-dir: ${cfg.petaformDir}")

      // TODO (KR) : ensure exists
      petaformPath <- Path(petaformPathString)
      environmentsPath <- petaformPath.child("environments.conf")
      resourcesPath <- petaformPath.child("resources.conf")
      configPath <- petaformPath.child("config")

      envVars <- System.envs.mapError(HError.fromThrowable)

      resourcesRawAST <- ParseRawAST.fromPath(resourcesPath)
      partialResources <- Errors.scopedToTask(PartialResources.fromRawResourcesAST(resourcesRawAST))

      environmentsRawAST <- ParseRawAST.fromPath(environmentsPath)
      partiallyLoadedEnvironments <- Errors.scopedToTask(PartiallyLoadedEnvironment.mapFromRawEnvsAST(environmentsRawAST, envVars).map(_.values.toList))
      loadedEnvironments <- ZIO.foreach(partiallyLoadedEnvironments)(LoadedEnvironment.fromPartiallyLoadedEnvironment(_, configPath, envVars))
      builtEnvironments <- Errors.scopedToTask(loadedEnvironments.traverse(BuiltEnvironment.build(partialResources, _, envVars)))

      _ <- ZIO.foreachDiscard(builtEnvironments) { built =>
        val envName = built.env.envName
        for {
          _ <- Logger.log.debug(s"[:  $envName  -  config  :]\n${built.env.config.format}")
          _ <- Logger.log.debug(s"[:  $envName  -  resources  :]\n${ASTEncoder[ResourceGroups].encode(built.resourceGroups).format}")
          _ <- Logger.log.debug(s"[:  $envName  -  terraform  :]\n${built.terraformString}")
        } yield ()
      }
    } yield builtEnvironments

  private val init: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform init")
          _ <- getEnvs(cfg.petaformDir)
        } yield ()
      }

  override val executable: Executable =
    Executable.fromSubCommands(
      "init" -> init,
    )

}
