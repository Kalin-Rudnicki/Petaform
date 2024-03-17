package petaform.model.repr

import petaform.model.ast.*
import petaform.model.typeclass.*

final case class Provider(
    base: ProviderBase,
    config: PetaformAST.Obj,
)
object Provider {
  implicit val astCodec: ASTCodec[Provider] = ASTCodec.derived
}
