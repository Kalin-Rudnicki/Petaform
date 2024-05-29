package petaform.model.repr

import harness.core.*
import petaform.model.typeclass.*

final case class CommandAndArgs(
    cmd: String,
    args: Option[List[String]],
    env: Option[Map[String, String]],
) {
  def toList: List[String] = cmd :: args.getOrElse(Nil)
  def show: String = toList.map(_.unesc).mkString(" ")
}
object CommandAndArgs {
  implicit val astCodec: ASTCodec[CommandAndArgs] = ASTCodec.derive
}
