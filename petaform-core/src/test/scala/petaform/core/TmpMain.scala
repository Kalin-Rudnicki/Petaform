package petaform.core

import cats.syntax.option.*
import harness.cli.*
import harness.core.*
import harness.zio.*
import petaform.core.parser.ASTParser
import slyce.parse.exe.ParseExe
import zio.*

object TmpMain extends ExecutableApp {

  private val resourceGroups: Parts.ResourceGroups =
    Parts.ResourceGroups(
      "nginx" -> Parts.ResourceGroup(
        "local" -> Parts.ResourceVariant(
          "docker" -> Parts.Provider(
            base = Parts.ProviderBase(
              source = "kreuzwerker/docker",
              version = "3.0.2",
            ),
            config = PetaformAST.Obj.empty,
          ),
        )(
          Parts.Resource("docker_image", "nginx")(
            "name" -> PetaformAST.Str("nginx:latest"),
          ),
          Parts.Resource("docker_container", "nginx")(
            "name" -> PetaformAST.Str("nginx"),
            "image" -> PetaformAST.RawValue("docker_image.nginx.image_id"),
            "ports" -> PetaformAST.Arr(
              PetaformAST.Obj(
                "external" -> PetaformAST.RawValue("8080"),
                "internal" -> PetaformAST.RawValue("80"),
              ),
            ),
          ),
        ),
      ),
    )

  override val executable: Executable =
    Executable.fromSubCommands(
      "tmp" -> Executable.withEffect {
        for {
          _ <- Logger.log.info("Petaform Core TmpMain")
          _ <- Logger.log.info(resourceGroups)
          ast = ASTEncoder[Parts.ResourceGroups].encode(resourceGroups)
          encoded = Formatting.ast(ast)
          _ <- Logger.log.info(encoded)
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
