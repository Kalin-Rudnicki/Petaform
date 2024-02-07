package petaform.model

import cats.syntax.either.*
import petaform.model.typeclass.*

final case class SingleKeyMap[A](
    key: String,
    value: A,
)
object SingleKeyMap {
  implicit def astCodec[A: ASTEncoder: ASTDecoder]: ASTCodec[SingleKeyMap[A]] =
    ASTCodec
      .from[Map[String, A]]
      .transformOrFail(
        _.toList match {
          case (key, value) :: Nil => SingleKeyMap(key, value).asRight
          case _                   => "Expected single key map".asLeft
        },
        skm => Map(skm.key -> skm.value),
      )
}
