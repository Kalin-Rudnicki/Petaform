package petaform.core

import harness.core.*
import harness.zio.*
import zio.*

object TmpMain extends ExecutableApp {

  private val envVars: Map[String, String] =
    Map(
      "VAR_1" -> "var-1-value",
    )

  private val cases: List[Stage1PetaformAST] =
    List(
      Stage1PetaformAST.Obj.makeUnsafe(
        "outer1" -> Stage1PetaformAST.Obj.makeUnsafe(
          "key1" -> env.VAR_1,
          "key2" -> is"value2",
        ),
        "outer2" -> is"value3",
        "outer3" -> Stage1PetaformAST.Obj.makeUnsafe(
          "key1" -> cfg.outer1,
          "key2" -> cfg.outer1.key2,
          "key3" -> cfg.outer3.key4,
          "key4" -> is"[${cfg.outer1.key1}--${cfg.outer3.key2}]",
          "key5" -> env.VAR_1,
          "key6" -> is"<${env.VAR_1}>",
        ),
      ),
    )

  override val executable: Executable =
    Executable.withEffect {
      for {
        _ <- Logger.log.info("Petaform Core TmpMain")
        _ <- ZIO.foreachDiscard(cases.zipWithIndex) { case (ast1, i) =>
          (for {
            _ <- Logger.log.info(s"=====| Case ${i + 1} |=====")
            _ <- Logger.log.info(Formatting.ast1(ast1))
            ast2 <- Stage2PetaformAST.fromStage1(ast1, envVars) match {
              case Right(value) => ZIO.succeed(value)
              case Left(error)  => ZIO.fail(HError.UserError(error.toString))
            }
            _ <- Logger.log.info(Formatting.ast2(ast2))
          } yield ()).dumpErrorsAndContinue
        }
      } yield ()
    }

}
