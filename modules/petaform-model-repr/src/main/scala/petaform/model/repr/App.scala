package petaform.model.repr

import petaform.model.typeclass.ASTCodec

final case class App(
    name: String,
    build: Option[List[CommandAndArgs]],
    run: CommandAndArgs,
)
object App {
  implicit val astCodec: ASTCodec[App] = ASTCodec.derive
}
