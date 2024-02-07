package petaform.model.repr

import petaform.model.typeclass.*

final case class ProviderBase(
    source: String,
    version: String,
)
object ProviderBase {
  implicit val astCodec: ASTCodec[ProviderBase] = ASTCodec.derived
}
