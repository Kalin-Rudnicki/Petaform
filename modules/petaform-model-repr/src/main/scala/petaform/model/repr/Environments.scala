package petaform.model.repr

import petaform.model.typeclass.*

final case class Environments(
    environments: Map[String, Environment],
)
object Environments {

  implicit val astCodec: ASTCodec[Environments] =
    ASTCodec.from[Map[String, Environment]].transform(Environments(_), _.environments)

}
