package petaform.model.repr

import petaform.model.typeclass.*

final case class ResourceVariant(
    providers: Map[String, Provider],
    resources: List[Resource],
    // TODO (KR) : outputs
    // TODO (KR) : a way to build/publish docker images
)
object ResourceVariant {
  implicit val astCodec: ASTCodec[ResourceVariant] = ASTCodec.derived
}
