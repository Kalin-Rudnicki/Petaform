package petaform.core

import shapeless3.deriving.*

final case class ASTCodec[A](
    encoder: ASTEncoder[A],
    decoder: ASTDecoder[A],
) {

  def transform[B](to: A => B, from: B => A): ASTCodec[B] =
    ASTCodec[B](encoder.contramap[B](from), decoder.map[B](to))

  def transformOrFail[B](to: A => Either[String, B], from: B => A): ASTCodec[B] =
    ASTCodec[B](encoder.contramap[B](from), decoder.mapOrFail[B](to))

}
object ASTCodec {

  implicit def apply[A](implicit codec: ASTCodec[A]): ASTCodec[A] = codec

  def from[A: ASTEncoder: ASTDecoder]: ASTCodec[A] =
    ASTCodec(ASTEncoder[A], ASTDecoder[A])

  inline def derived[A](using gen: K0.Generic[A]): ASTCodec[A] =
    ASTCodec(
      ASTEncoder.derived[A],
      ASTDecoder.derived[A],
    )

}
