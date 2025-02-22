package petaform.main

import cats.data.NonEmptyList
import cats.syntax.list.*
import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import petaform.main.error.PetaformError
import petaform.main.model.*
import petaform.model.*
import petaform.model.repr.*
import petaform.model.typeclass.*
import petaform.parsing.*
import scala.util.matching.Regex
import zio.*

object Main extends ExecutableApp {

  private val petaformDirParser: Parser[String] =
    Parser
      .value[String](
        LongName.unsafe("petaform-dir"),
        Defaultable.Some(ShortName.unsafe('p')),
      )
      .default("petaform", Defaultable.Auto)

  private val environmentParser: Parser[String] =
    Parser.value[String](
      LongName.unsafe("environment"),
      Defaultable.Some(ShortName.unsafe('E')),
      helpHint = List("Name of environment to operate on"),
    )

  private val allEnvironmentsParser: Parser[Unit] =
    Parser.present(
      LongName.unsafe("all-environments"),
      (),
      Defaultable.Some(ShortName.unsafe('A')),
      helpHint = List("Operate on all environments"),
    )

  private val environmentOrAllEnvironmentsParser: Parser[Option[String]] =
    environmentParser.indentedHelpMessage.<^||(allEnvironmentsParser.indentedHelpMessage).map(_.swap.toOption)

  private val skipExportParser: Parser[Boolean] =
    Parser.flag(LongName.unsafe("skip-export"))

  private val keyValueRegex: Regex = "^([^=]+)=(.*)$".r
  private implicit val keyValueStringDecoder: StringDecoder[(String, String)] =
    StringDecoder.fromOptionF(
      "key=value",
      {
        case keyValueRegex(key, value) => (key, value).some
        case _                         => None
      },
    )

  private val envVarsParser: Parser[Map[String, String]] =
    Parser.values.list[(String, String)](LongName.unsafe("var")).map(_.toMap)

  private val extraConfigGroups: Parser[List[String]] =
    Parser.values.list[String](LongName.unsafe("config-group"))

  private val autoApproveParser: Parser[Boolean] =
    Parser.flag(LongName.unsafe("auto-approve"))

  private val hardParser: Parser[Boolean] =
    Parser.flag(
      LongName.unsafe("hard"),
      shortParam = Defaultable.Some(ShortName.unsafe('f')),
      helpHint = List("also destroy resources which are marked as `requireHardDestroy: true`"),
    )

  private final case class Cfg(
      petaformDir: String,
      environment: Option[String],
      skipExport: Boolean,
      envVars: Map[String, String],
      extraConfigGroups: List[String],
  )
  private object Cfg {

    val parser: Parser[Cfg] = {
      petaformDirParser &&
      environmentOrAllEnvironmentsParser &&
      skipExportParser &&
      envVarsParser &&
      extraConfigGroups
    }.map { Cfg.apply }

  }

  // =====|  |=====

  extension (cmd: CommandAndArgs) {
    private def runCmd(envVars: EnvVars, hint: String): ZIO[Logger, PetaformError.SysCommandError, Unit] = {
      val sysCmd = Sys.Command(cmd.cmd, cmd.args)
      val env = envVars.additional ++ cmd.env.getOrElse(Map.empty)
      Logger.log.debug(s"Running system command ($hint): ${sysCmd.show}\n  ENV:${env.toList.sortBy(_._1.toUpperCase).map { case (k, v) => s"\n    $k: $v" }.mkString}") *>
        Sys.execute0.runComplex(env)(sysCmd).mapError(PetaformError.SysCommandError(_))
    }
  }

  extension (app: App) {
    private def runApp(envVars: EnvVars): ZIO[Logger, PetaformError.SysCommandError, Unit] =
      for {
        _ <- app.build.getOrElse(Nil).toNel match {
          case Some(buildCommands) =>
            Logger.log.info(s"Building app '${app.name}'") *>
              ZIO.foreachDiscard(buildCommands.toList) { _.runCmd(envVars, s"build '${app.name}'") }
          case None =>
            Logger.log.detailed(s"Nothing to build for app '${app.name}'")
        }
        _ <- Logger.log.info(s"Running app '${app.name}'")
        _ <- app.run.runCmd(envVars, s"run '${app.name}'")
      } yield ()
  }

  private def runApps(apps: NonEmptyList[App], envVars: EnvVars): ZIO[Logger, PetaformError.SysCommandError, Unit] =
    Logger.log.info("Running Apps") *>
      ZIO
        .foreachPar(apps.toList) { _.runApp(envVars).fork }
        .withParallelism(10)
        .flatMap { fibers => Logger.log.important("Awaiting completion of all apps") *> Fiber.joinAll(fibers) }

  private def getForEnvironment[A](environment: Option[String], map: Map[String, A]): IO[PetaformError.NoSuchEnvironment, List[A]] =
    environment match {
      case Some(environment) =>
        map.get(environment) match {
          case Some(value) => ZIO.succeed(value :: Nil)
          case None        => ZIO.fail(PetaformError.NoSuchEnvironment(environment))
        }
      case None =>
        ZIO.succeed(map.toList.sortBy(_._1).map(_._2))
    }

  private def exportTerraform(paths: PetaformPaths, env: BuiltEnvironment): ZIO[HarnessEnv, PetaformError, Unit] =
    for {
      now <- Clock.localDateTime
      _ <- Logger.log.info(s"Exporting terraform file for env '${env.env.envName}'")
      envDir <- paths.internalEnvironmentsPath.child(env.env.envName).mapError(PetaformError.Generic(_))
      _ <- envDir.mkdirs.unlessZIO(envDir.exists).mapError(PetaformError.Generic(_))
      mainTf <- envDir.child("main.tf").mapError(PetaformError.Generic(_))
      terraformString = s"# Generated by `petaform` @ $now\n${env.terraformString}"
      _ <- mainTf.writeString(terraformString).mapError(PetaformError.Generic(_))
    } yield ()

  private def getEnvs(cfg: Cfg, paths: PetaformPaths): ZIO[HarnessEnv, PetaformError, List[BuiltEnvironment]] =
    for {
      _ <- Logger.log.info("Calculating envs")
      _ <- Logger.log.detailed(s"petaform-dir: ${cfg.petaformDir}")

      envVars <- System.envs.mapBoth(PetaformError.Generic(_), EnvVars(_, cfg.envVars))

      resourcesRawAST <- ParseRawAST.fromPath(paths.resourcesPath).mapError(PetaformError.fromParseError)
      partialResources <- ZIO.fromEither(PartialResources.fromRawResourcesAST(resourcesRawAST)).mapError(PetaformError.ScopedErr(_))

      environmentsRawAST <- ParseRawAST.fromPath(paths.environmentsPath).mapError(PetaformError.fromParseError)
      partiallyLoadedEnvironmentMap <- ZIO.fromEither(PartiallyLoadedEnvironment.mapFromRawEnvsAST(environmentsRawAST, envVars)).mapError(PetaformError.ScopedErr(_))
      partiallyLoadedEnvironments <- getForEnvironment(cfg.environment, partiallyLoadedEnvironmentMap)
      builtEnvironments <- ZIO.foreach(partiallyLoadedEnvironments) { ple =>
        val newEnvVars = envVars.add("PETAFORM_ENV", ple.envName)
        for {
          loadedEnvironment <- LoadedEnvironment.fromPartiallyLoadedEnvironment(ple, paths.configPath, "default" :: cfg.extraConfigGroups, newEnvVars)
          builtEnvironment <- ZIO.fromEither(BuiltEnvironment.build(partialResources, loadedEnvironment, newEnvVars)).mapError(PetaformError.ScopedErr(_))
        } yield builtEnvironment
      }

      _ <- ZIO.foreachDiscard(builtEnvironments) { built =>
        val envName = built.env.envName
        for {
          _ <- Logger.log.trace(s"[:  $envName  -  config  :]\n${built.env.config.format}")
          _ <- Logger.log.trace(s"[:  $envName  -  resources  :]\n${ASTEncoder[ResourceGroups].encode(built.resourceGroups).format}")
          _ <- Logger.log.trace(s"[:  $envName  -  terraform  :]\n${built.terraformString}")
          _ <- exportTerraform(paths, built).unless(cfg.skipExport)
        } yield ()
      }
    } yield builtEnvironments

  private def runTerraformCommand(paths: PetaformPaths, env: BuiltEnvironment)(cmd: String, args: String*): ZIO[HarnessEnv, PetaformError, Unit] =
    for {
      _ <- Logger.log.info(s"Running terraform '$cmd' for '${env.env.envName}'")
      envDir <- paths.internalEnvironmentsPath.child(env.env.envName).mapError(PetaformError.Generic(_))
      _ <- Sys.execute0.runComplex(env.envVars.additional)(Sys.Command("terraform", s"-chdir=${envDir.show}" :: cmd :: args.toList*)).mapError(PetaformError.Generic(_))
    } yield ()

  private implicit val errorLogger: ErrorLogger[PetaformError] =
    ErrorLogger.ThrowableInstances.getMessageErrorLogger(Logger.LogLevel.Fatal)

  private val _export: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform export")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          _ <- getEnvs(cfg, paths)
        } yield ()
      }

  private val build: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform build")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          envs <- getEnvs(cfg, paths)
          _ <- ZIO.foreachDiscard(envs) { built =>
            val buildCommands =
              for {
                a <- built.resourceGroups.resourceGroups.toList
                b <- a._2.value.resources
                c <- b.build.getOrElse(Nil)
              } yield c

            ZIO.foldLeft(buildCommands)(built.envVars) {
              case (envVars, CommandAndArgs("export", args, _)) =>
                args match {
                  case Some(List(key, value)) => Logger.log.detailed(s"Adding env var: $key=${value.unesc}").as(envVars.add(key, value))
                  case _                      => ZIO.fail(PetaformError.Generic(new RuntimeException("Invalid args for `export`, expected 2, key & value")))
                }
              case (envVars, CommandAndArgs(cmd, args, env)) =>
                Sys.execute0.runComplex(envVars.addAll(env.getOrElse(Map.empty)).additional)(Sys.Command(cmd, args)).mapBoth(PetaformError.Generic(_), _ => envVars)
            }
          }
        } yield ()
      }

  private val init: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform init")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          envs <- getEnvs(cfg, paths)
          _ <- ZIO.foreachDiscard(envs) { env =>
            runTerraformCommand(paths, env)("init")
          }
        } yield ()
      }

  private val plan: Executable =
    Executable
      .withParser(Cfg.parser)
      .withEffect { cfg =>
        for {
          _ <- Logger.log.info("Running Petaform plan")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          envs <- getEnvs(cfg, paths)
          _ <- ZIO.foreachDiscard(envs) { env =>
            for {
              _ <- runTerraformCommand(paths, env)("plan")
              apps = env.resourceGroups.resourceGroups.values.toList.flatMap { skm => skm.value.apps.flatMap(_.toNel).map(skm.key -> _) }
              _ <- ZIO.when(apps.nonEmpty) {
                Logger.log.info("Apps:") *>
                  ZIO.foreachDiscard(apps) { case (key, apps) =>
                    Logger.log.info(s"--- $key ---") *>
                      Logger.log.info(apps.toList.toPetaformAST.format)
                  }
              }
            } yield ()
          }
        } yield ()
      }

  private val _apply: Executable =
    Executable
      .withParser(Cfg.parser && autoApproveParser)
      .withEffect { case (cfg, autoApprove) =>
        for {
          _ <- Logger.log.info("Running Petaform apply")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          envs <- getEnvs(cfg, paths)
          _ <- ZIO.foreachDiscard(envs) { env =>
            for {
              _ <- runTerraformCommand(paths, env)("apply", Option.when(autoApprove)("-auto-approve").toList*)
              _ <- Logger.log.info("Terraform apply complete")
              apps = env.resourceGroups.resourceGroups.values.toList.flatMap { _.value.apps.getOrElse(Nil) }
              _ <- ZIO.foreachDiscard(apps.toNel) { runApps(_, env.envVars) }
            } yield ()
          }
        } yield ()
      }

  private val destroy: Executable =
    Executable
      .withParser(Cfg.parser && autoApproveParser && hardParser)
      .withEffect { case (cfg, autoApprove, hard) =>
        for {
          _ <- Logger.log.info("Running Petaform destroy")
          paths <- PetaformPaths.fromPetaformPathString(cfg.petaformDir)
          envs <- getEnvs(cfg, paths)
          _ <- ZIO.foreachDiscard(envs) { env =>
            for {
              tfStatePath <- paths.internalEnvironmentsPath.child(s"${env.env.envName}/terraform.tfstate").mapError(PetaformError.Generic(_))
              tfState <- tfStatePath.readJson[TfState].mapError(PetaformError.Generic(_))
              typeNameHardMap = env.resourceGroups.resourceGroups.toList.flatMap { _._2.value.resources.map { r => ((r.base.`type`, r.base.name), r.requireHardDestroy.getOrElse(false)) } }.toMap
              targets <-
                if (hard) ZIO.succeed(Nil)
                else {
                  val (toNotDestroy, toDestroy) = tfState.resources.partition(res => typeNameHardMap.getOrElse((res.`type`, res.name), false))
                  for {
                    _ <- Logger.log.detailed(s"To destroy: ${toDestroy.map(r => s"\n  - ${r.`type`}.${r.name}").mkString}")
                    _ <- Logger.log.detailed(s"To not destroy: ${toNotDestroy.map(r => s"\n  - ${r.`type`}.${r.name}").mkString}")
                    targets =
                      if (toNotDestroy.isEmpty) Nil
                      else toDestroy.map(r => s"-target=${r.`type`}.${r.name}")
                  } yield targets
                }
              _ <- runTerraformCommand(paths, env)("destroy", targets ::: Option.when(autoApprove)("-auto-approve").toList*)
            } yield ()
          }
        } yield ()
      }

  override val executable: Executable =
    Executable.fromSubCommands(
      "export" -> _export,
      "build" -> build,
      "init" -> init,
      "plan" -> plan,
      "apply" -> _apply,
      "destroy" -> destroy,
    )

}
