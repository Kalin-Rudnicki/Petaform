package petaform.model.typeclass

import harness.deriving.{K0, *}

final case class ASTCodec[A](
    encoder: ASTEncoder[A],
    decoder: ASTDecoder[A],
) {

  def transform[B](to: A => B, from: B => A): ASTCodec[B] =
    ASTCodec[B](encoder.contramap[B](from), decoder.map[B](to))

  def transformOrFail[B](to: A => Either[String, B], from: B => A): ASTCodec[B] =
    ASTCodec[B](encoder.contramap[B](from), decoder.mapOrFail[B](to))

}
object ASTCodec extends K0.Derivable[ASTCodec] {

  implicit def apply[A](implicit codec: ASTCodec[A]): ASTCodec[A] = codec

  def from[A: ASTEncoder: ASTDecoder]: ASTCodec[A] =
    ASTCodec(ASTEncoder[A], ASTDecoder[A])

  override implicit inline def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[ASTCodec[A]] =
    Derived {
      ASTCodec(
        ASTEncoder.genProduct[A].derived,
        ASTDecoder.genProduct[A].derived,
      )
    }

  override implicit inline def genSum[A](implicit m: K0.SumGeneric[A]): Derived[ASTCodec[A]] =
    Derived {
      ASTCodec(
        ASTEncoder.genSum[A].derived,
        ASTDecoder.genSum[A].derived,
      )
    }

}
