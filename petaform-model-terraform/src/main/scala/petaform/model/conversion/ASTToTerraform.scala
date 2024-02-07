package petaform.model.conversion

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.ast.TerraformAST.*
import petaform.model.repr.*

object ASTToTerraform {

  def apply(resourceGroups: ResourceGroups): EitherError[List[BlockSet]] =
    for {
      (requiredProviders, providers) <- buildProviders(resourceGroups)
      resources <- buildResources(resourceGroups)
      terraformBlockSet = BlockSet("terraform", requiredProviders :: Nil)
    } yield terraformBlockSet :: providers ::: resources

  // =====|  |=====

  private object ArrayOfObjects {

    def unapply(ast: PetaformAST): Option[List[PetaformAST.Obj]] =
      ast match {
        case PetaformAST.Arr(elems) =>
          elems.traverse {
            case obj: PetaformAST.Obj => obj.some
            case _                    => None
          }
        case _ => None
      }

  }

  private def toValue(ast: PetaformAST): EitherError[Value] =
    ast match {
      case PetaformAST.RawValue(value) => Value.Raw(value).asRight
      case PetaformAST.Str(str)        => Value.Str(str).asRight
      case PetaformAST.Null            => Value.Raw("null").asRight
      case PetaformAST.Obj(elems)      => elems.traverse { case (k, v) => toValue(v).map((k, _)) }.map(Value.Map(_))
      case PetaformAST.Arr(elems)      => elems.traverse(toValue).map(Value.Arr(_))
      case PetaformAST.Empty           => HError.InternalDefect("Empty -> TerraformAST not supported").asLeft
      case PetaformAST.Undef           => HError.InternalDefect("Undef -> TerraformAST not supported").asLeft
    }

  private def toAsts(key: String, ast: PetaformAST): EitherError[List[TerraformAST]] =
    ast match {
      case ArrayOfObjects(objs) => toBlockSets(key, objs)
      case _                    => toValue(ast).map(KeyValue(key, _) :: Nil)
    }

  private def toBlockSet(key: String, ast: PetaformAST.Obj): EitherError[BlockSet] =
    ast.elems.traverse(toAsts(_, _)).map(_.flatten).map(BlockSet(key, _))
  private def toBlockSets(key: String, asts: List[PetaformAST.Obj]): EitherError[List[BlockSet]] =
    asts.traverse(toBlockSet(key, _))

  private def buildProviders(resourceGroups: ResourceGroups): EitherError[(BlockSet, List[BlockSet])] = {
    val all = resourceGroups.resourceGroups.toList.flatMap(_._2.value.providers.toList)
    val grouped = all.groupMap(_._1)(_._2)

    for {
      providerMap <-
        grouped.toList
          .traverse { case (name, values) =>
            values.distinct match {
              case head :: Nil => (name, head).asRight
              case _ :: _      => HError.UserError(s"provider '$name' has multiple values").asLeft
              case Nil         => HError.InternalDefect(s"provider '$name' has no values?").asLeft
            }
          }
          .map(_.toMap)
      providerList = providerMap.toList.sortBy(_._1)

      requiredProviders = BlockSet(
        "required_providers",
        providerList.map { case (key, value) =>
          val elems = List(
            "source" -> Value.Str(value.base.source),
            "version" -> Value.Str(value.base.version),
          )
          KeyValue(key, Value.Map(elems))
        },
      )
      providerBlocks <- providerList.traverse { case (key, value) =>
        toBlockSet(s"provider ${key.unesc}", value.config)
      }
    } yield (requiredProviders, providerBlocks)
  }

  private def buildResource(resource: Resource): EitherError[BlockSet] =
    toBlockSet(s"resource ${resource.base.`type`.unesc} ${resource.base.name.unesc}", resource.config)

  private def buildResources(resourceGroups: ResourceGroups): EitherError[List[BlockSet]] =
    resourceGroups.resourceGroups.toList.flatMap(_._2.value.resources).traverse(buildResource)

}
