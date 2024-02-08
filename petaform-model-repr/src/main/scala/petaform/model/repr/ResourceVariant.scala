package petaform.model.repr

import petaform.model.ast.*
import petaform.model.typeclass.*

final case class ResourceVariant(
    providers: Map[String, Provider],
    resources: List[Resource],
    outputs: Option[Map[String, PetaformAST.Obj]],
    // TODO (KR) : a way to build/publish docker images
)
object ResourceVariant {
  implicit val astCodec: ASTCodec[ResourceVariant] = ASTCodec.derived
}
