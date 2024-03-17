package petaform.model.repr

import petaform.model.ast.*
import petaform.model.typeclass.*

final case class Resource(
    base: ResourceBase,
    config: PetaformAST.Obj,
    requireHardDestroy: Option[Boolean],
    build: Option[List[CommandAndArgs]],
)
object Resource {
  implicit val astCodec: ASTCodec[Resource] = ASTCodec.derived
}
