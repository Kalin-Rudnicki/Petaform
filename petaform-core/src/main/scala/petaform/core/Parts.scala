package petaform.core

import cats.syntax.either.*

object Parts {

  final case class Environments(
      environments: Map[String, Environment],
  )
  object Environments {

    implicit val astCodec: ASTCodec[Environments] =
      ASTCodec.from[Map[String, Environment]].transform(Environments(_), _.environments)

  }

  final case class Environment(
      configs: List[String],
      resources: Map[String, String],
  )
  object Environment {
    implicit val astCodec: ASTCodec[Environment] = ASTCodec.derived
  }

  final case class ResourceGroups(
      resourceGroups: Map[String, SingleKeyMap[ResourceVariant]],
  )
  object ResourceGroups {

    implicit val astCodec: ASTCodec[ResourceGroups] =
      ASTCodec.from[Map[String, SingleKeyMap[ResourceVariant]]].transform(ResourceGroups(_), _.resourceGroups)

  }

  final case class SingleKeyMap[A](
      key: String,
      value: A,
  )
  object SingleKeyMap {
    implicit def astCodec[A: ASTEncoder: ASTDecoder]: ASTCodec[SingleKeyMap[A]] =
      ASTCodec
        .from[Map[String, A]]
        .transformOrFail(
          _.toList match {
            case (key, value) :: Nil => SingleKeyMap(key, value).asRight
            case _                   => "Expected single key map".asLeft
          },
          skm => Map(skm.key -> skm.value),
        )
  }

  final case class ResourceVariant(
      providers: Map[String, Provider],
      resources: List[Resource],
  )
  object ResourceVariant {

    def apply(providers: (String, Provider)*)(resources: Resource*): ResourceVariant = ResourceVariant(providers.toMap, resources.toList)

    implicit val astCodec: ASTCodec[ResourceVariant] = ASTCodec.derived

  }

  final case class Provider(
      base: ProviderBase,
      config: PetaformAST.Obj,
  )
  object Provider {
    implicit val astCodec: ASTCodec[Provider] = ASTCodec.derived
  }

  final case class ProviderBase(
      source: String,
      version: String,
  )
  object ProviderBase {
    implicit val astCodec: ASTCodec[ProviderBase] = ASTCodec.derived
  }

  final case class Resource(
      base: ResourceBase,
      config: PetaformAST.Obj,
  )
  object Resource {

    def apply(`type`: String, name: String)(config: (String, PetaformAST)*): Resource = Resource(ResourceBase(`type`, name), PetaformAST.Obj(config.toList))

    implicit val astCodec: ASTCodec[Resource] = ASTCodec.derived

  }

  final case class ResourceBase(
      `type`: String,
      name: String,
  )
  object ResourceBase {
    implicit val astCodec: ASTCodec[ResourceBase] = ASTCodec.derived
  }

}
