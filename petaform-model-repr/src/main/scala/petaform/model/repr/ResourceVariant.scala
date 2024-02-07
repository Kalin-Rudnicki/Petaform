package petaform.model.repr

import petaform.model.typeclass.*

final case class ResourceVariant(
    providers: Map[String, Provider],
    resources: List[Resource],
)
object ResourceVariant {
  implicit val astCodec: ASTCodec[ResourceVariant] = ASTCodec.derived
}
