package petaform.model.typeclass

import cats.syntax.option.*
import harness.zio.test.DefaultHarnessSpec
import zio.test.*
import zio.test.Assertion.*

object ASTCodecSpec extends DefaultHarnessSpec {

  private final case class ExProduct(
      key1: String,
      key2: Option[Int],
  )
  private object ExProduct {
    implicit val astCodec: ASTCodec[ExProduct] = ASTCodec.derived
  }

  private sealed trait ExSum
  private object ExSum {

    final case class Case1(key1: String) extends ExSum
    final case class Case2(key2: Option[Int]) extends ExSum
    final case class Case3(product: ExProduct) extends ExSum

    implicit val astCodec: ASTCodec[ExSum] = ASTCodec.derived

  }

  // =====|  |=====

  private def roundTripTest[A](name: String)(a: A)(implicit encoder: ASTEncoder[A], decoder: ASTDecoder[A]): TestSpec =
    test(name) {
      assert(decoder.decode(encoder.encode(a)))(isRight(equalTo(a)))
    }

  override def spec: TestSpec =
    suite("ASTCodecSpec")(
      suite("round-trip")(
        roundTripTest("string")("str"),
        roundTripTest("int")(0),
        roundTripTest("ExProduct-1")(ExProduct("a", None)),
        roundTripTest("ExProduct-2")(ExProduct("a", 1.some)),
        roundTripTest[ExSum]("ExSum-Case1")(ExSum.Case1("v1")),
        roundTripTest[ExSum]("ExSum-Case2-1")(ExSum.Case2(None)),
        roundTripTest[ExSum]("ExSum-Case2-2")(ExSum.Case2(1.some)),
        roundTripTest[ExSum]("ExSum-Case3-1")(ExSum.Case3(ExProduct("v1", None))),
        roundTripTest[ExSum]("ExSum-Case3-2")(ExSum.Case3(ExProduct("v1", 1.some))),
      ),
    )

}
