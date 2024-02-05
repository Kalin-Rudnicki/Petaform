package petaform.core

object Parts {

  final case class Environments(
      environments: Map[String, Environments.Environment],
  )
  object Environments {

    final case class Environment(
        configs: List[String],
        resources: Map[String, String],
    )
    object Environment {
      implicit val astCodec: ASTCodec[Environment] = ASTCodec.derived
    }

    implicit val astCodec: ASTCodec[Environments] =
      ASTCodec.from[Map[String, Environments.Environment]].transform(Environments(_), _.environments)

  }

  final case class ResourceGroups(
      resourceGroups: Map[String, ResourceGroup],
  )
  object ResourceGroups {

    def apply(resourceGroups: (String, ResourceGroup)*): ResourceGroups = ResourceGroups(resourceGroups.toMap)

    implicit val astCodec: ASTCodec[ResourceGroups] =
      ASTCodec.from[Map[String, ResourceGroup]].transform(ResourceGroups(_), _.resourceGroups)

  }

  final case class ResourceGroup(
      variants: Map[String, ResourceVariant],
  )
  object ResourceGroup {

    def apply(variants: (String, ResourceVariant)*): ResourceGroup = ResourceGroup(variants.toMap)

    implicit val astCodec: ASTCodec[ResourceGroup] = ASTCodec.from[Map[String, ResourceVariant]].transform(ResourceGroup(_), _.variants)

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
