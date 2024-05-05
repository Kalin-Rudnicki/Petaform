package petaform.model.repr

import petaform.model.typeclass.*

final case class Environment(
    configs: Map[String, List[String]],
    resources: Map[String, String],
)
object Environment {
  implicit val astCodec: ASTCodec[Environment] = ASTCodec.derive
}
