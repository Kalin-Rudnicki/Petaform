package petaform.model.repr

import petaform.model.typeclass.*

final case class ResourceBase(
    `type`: String,
    name: String,
)
object ResourceBase {
  implicit val astCodec: ASTCodec[ResourceBase] = ASTCodec.derived
}
