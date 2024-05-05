package petaform.model.repr

import petaform.model.typeclass.*

final case class CommandAndArgs(
    cmd: String,
    args: Option[List[String]],
)
object CommandAndArgs {
  implicit val astCodec: ASTCodec[CommandAndArgs] = ASTCodec.derive
}
