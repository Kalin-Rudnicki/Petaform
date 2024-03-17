package petaform.model.repr

import petaform.model.*
import petaform.model.typeclass.*

final case class ResourceGroups(
    resourceGroups: Map[String, SingleKeyMap[ResourceVariant]],
)
object ResourceGroups {
  implicit val astCodec: ASTCodec[ResourceGroups] =
    ASTCodec.from[Map[String, SingleKeyMap[ResourceVariant]]].transform(ResourceGroups(_), _.resourceGroups)
}
