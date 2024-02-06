package petaform.core

import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import petaform.core.parser.ASTParser
import slyce.parse.exe.ParseExe
import zio.*

object TmpMain extends ExecutableApp {

  override val executable: Executable =
    Executable.fromSubCommands(
      "tmp" -> Executable.withEffect {
        for {
          _ <- Logger.log.info("Petaform Core TmpMain")
        } yield ()
      },
      "parse" -> ParseExe.fromParser(ASTParser)("cfg"),
      "parse-2" ->
        Executable
          .withParser {
            Parser.values.nel[String](LongName.unsafe("cfg-file")) &&
            Parser.value[String](LongName.unsafe("files"))
          }
          .withEffect { case (cfgFiles, file) =>
            for {
              _ <- Logger.log.info("=====| parse-2 |=====")

              envVars <- System.envs.mapError(HError.fromThrowable)

              rawCfgAsts <- ZIO.foreach(cfgFiles.toList) { RawPetaformAST.fromPathString }
              rawCfgAst <- RawPetaformAST.merge(rawCfgAsts) match {
                case Right(value) => ZIO.succeed(value)
                case Left(error)  => ZIO.fail(HError.UserError(error.toString))
              }
              cfgAst <- PetaformAST.fromRaw(rawCfgAst, envVars, None) match {
                case Right(value) => ZIO.succeed(value)
                case Left(error)  => ZIO.fail(HError.UserError(error.toString))
              }

              rawAst <- RawPetaformAST.fromPathString(file)
              ast <- PetaformAST.fromRaw(rawAst, envVars, cfgAst.some) match {
                case Right(value) => ZIO.succeed(value)
                case Left(error)  => ZIO.fail(HError.UserError(error.toString))
              }

              _ <- Logger.log.info(Formatting.ast(ast))
            } yield ()
          },
    )

}
