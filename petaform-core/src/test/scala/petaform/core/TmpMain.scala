package petaform.core

import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import scala.reflect.ClassTag
import zio.*

object TmpMain extends ExecutableApp {

  private val envVars: Map[String, String] =
    Map(
      "VAR_1" -> "var-1-value",
    )

  private val cases: List[RawPetaformAST] =
    List(
      RawPetaformAST.Obj.makeUnsafe(
        "outer1" -> RawPetaformAST.Obj.makeUnsafe(
          "key1" -> env.VAR_1,
          "key2" -> is"value2",
        ),
        "outer2" -> is"value3",
        "outer3" -> RawPetaformAST.Obj.makeUnsafe(
          "key1" -> cfg.outer1,
          "key2" -> cfg.outer1.key2,
          "key3" -> cfg.outer3.key4,
          "key4" -> is"[${cfg.outer1.key1}--${cfg.outer3.key2}]",
          "key5" -> env.VAR_1,
          "key6" -> is"<${env.VAR_1}>",
        ),
      ),
    )

  private val showCases: URIO[HarnessEnv, Unit] =
    ZIO.foreachDiscard(cases.zipWithIndex) { case (ast1, i) =>
      (for {
        _ <- Logger.log.info(s"=====| Case ${i + 1} |=====")
        _ <- Logger.log.info(Formatting.rawAST(ast1))
        ast2 <- PetaformAST.fromStage1(ast1, envVars) match {
          case Right(value) => ZIO.succeed(value)
          case Left(error)  => ZIO.fail(HError.UserError(error.toString))
        }
        _ <- Logger.log.info(Formatting.ast(ast2))
      } yield ()).dumpErrorsAndContinue
    }

  private final case class Type1(
      a: String,
      b: Option[Int],
  )
  private object Type1 {
    implicit val astEncoder: ASTEncoder[Type1] = ASTEncoder.derived
    implicit val astDecoder: ASTDecoder[Type1] = ASTDecoder.derived
  }

  private final case class Type2(
      bool: Boolean,
      type1: Option[Type1],
  )
  private object Type2 {
    implicit val astEncoder: ASTEncoder[Type2] = ASTEncoder.derived
    implicit val astDecoder: ASTDecoder[Type2] = ASTDecoder.derived
  }

  private sealed trait Type3
  private object Type3 {
    final case class T1(t1: Type1) extends Type3
    final case class T2(t2: Type2) extends Type3

    implicit val astEncoder: ASTEncoder[Type3] = ASTEncoder.derived
    implicit val astDecoder: ASTDecoder[Type3] = ASTDecoder.derived
  }

  private def showEncoded[A: ASTEncoder: ASTDecoder: ClassTag](values: A*): URIO[HarnessEnv, Unit] =
    Logger.log.info(s"Showing cases for [${implicitly[ClassTag[A]].runtimeClass.getSimpleName}]") *>
      ZIO.foreachDiscard(values) { value =>
        val encoded = ASTEncoder[A].encode(value)
        Logger.log.info(Formatting.ast(encoded)) *>
          Logger.log.info(ASTDecoder[A].decode(encoded).merge)
      }

  override val executable: Executable =
    Executable.withEffect {
      for {
        _ <- Logger.log.info("Petaform Core TmpMain")
        _ <- showCases.when(false)
        _ <- showEncoded(
          Type1("abc", 5.some),
          Type1("def", None),
        )
        _ <- showEncoded(
          Type2(true, Type1("abc$", 5.some).some),
          Type2(true, Type1("def", None).some),
          Type2(false, None),
        )
        _ <- showEncoded(
          Type3.T1(Type1("abc$", 5.some)),
          Type3.T2(Type2(true, Type1("abc$", 5.some).some)),
          Type3.T2(Type2(true, Type1("def", None).some)),
          Type3.T2(Type2(false, None)),
        )
      } yield ()
    }

}
