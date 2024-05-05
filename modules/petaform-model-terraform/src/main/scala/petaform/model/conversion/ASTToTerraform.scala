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

  def apply(resourceGroups: ResourceGroups): Either[ScopedError, List[BlockSet]] =
    for {
      (requiredProviders, providers) <- buildProviders(resourceGroups)
      resources <- buildResources(resourceGroups)
      outputs <- buildOutputs(resourceGroups)
      terraformBlockSet = BlockSet("terraform", requiredProviders :: Nil)
    } yield terraformBlockSet :: providers ::: resources ::: outputs

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

  private def toValue(ast: PetaformAST, scope: ScopePath): Either[ScopedError, Value] =
    ast match {
      case PetaformAST.Raw(value)    => Value.Raw(value).asRight
      case PetaformAST.Str(str)      => Value.Str(str).asRight
      case PetaformAST.EofStr(lines) => Value.EofStr(lines).asRight
      case PetaformAST.Null          => Value.Raw("null").asRight
      case PetaformAST.Obj(elems) =>
        elems.traverse { case (k, v) => toValue(v, scope :+ ASTScope.Key(k)).map((k, _)) }.map(Value.Map(_))
      case PetaformAST.Arr(elems) =>
        elems.zipWithIndex.traverse { case (ast, idx) => toValue(ast, scope :+ ASTScope.Idx(idx)) }.map(Value.Arr(_))
      case PetaformAST.Empty => ScopedError(scope, "Empty -> TerraformAST not supported").asLeft
      case PetaformAST.Undef => ScopedError(scope, "Undef -> TerraformAST not supported").asLeft
    }

  private def toAsts(key: String, ast: PetaformAST, scope: ScopePath): Either[ScopedError, List[TerraformAST]] = {
    val keyRScope = scope :+ ASTScope.Key(key)
    ast match {
      case ArrayOfObjects(objs) => toBlockSets(key, objs, keyRScope)
      case _                    => toValue(ast, keyRScope).map(KeyValue(key, _) :: Nil)
    }
  }

  private def toBlockSet(key: String, ast: PetaformAST.Obj, scope: ScopePath): Either[ScopedError, BlockSet] =
    ast.elems.traverse(toAsts(_, _, scope)).map(_.flatten).map(BlockSet(key, _))
  private def toBlockSets(key: String, asts: List[PetaformAST.Obj], scope: ScopePath): Either[ScopedError, List[BlockSet]] =
    asts.zipWithIndex.traverse { case (ast, idx) => toBlockSet(key, ast, scope :+ ASTScope.Idx(idx)) }

  private def buildProviders(resourceGroups: ResourceGroups): Either[ScopedError, (BlockSet, List[BlockSet])] = {
    val all = resourceGroups.resourceGroups.toList.flatMap(_._2.value.providers.toList)
    val grouped = all.groupMap(_._1)(_._2)
    val providersScope = ScopePath.reversed(ASTScope.Key("providers") :: ASTScope.Key("*") :: ASTScope.Key("*") :: Nil)

    for {
      providerMap <-
        grouped.toList
          .traverse { case (name, values) =>
            val providerScope = providersScope :+ ASTScope.Key(name)
            values.distinct match {
              case head :: Nil => (name, head).asRight
              case _ :: _      => ScopedError(providerScope, s"provider '$name' has multiple values").asLeft
              case Nil         => ScopedError(providerScope, s"provider '$name' has no values?").asLeft
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
        toBlockSet(s"provider ${key.unesc}", value.config, providersScope :+ ASTScope.Key(key))
      }
    } yield (requiredProviders, providerBlocks)
  }

  private def buildResource(resource: Resource, scope: ScopePath): Either[ScopedError, BlockSet] =
    toBlockSet(s"resource ${resource.base.`type`.unesc} ${resource.base.name.unesc}", resource.config, scope :+ ASTScope.Key("config"))

  private def buildResources(resourceGroups: ResourceGroups): Either[ScopedError, List[BlockSet]] = {
    val resourcesAndScopes = resourceGroups.resourceGroups.toList.flatMap { case (resourceName, variant) =>
      variant.value.resources.zipWithIndex.map { case (resource, idx) =>
        (
          resource,
          ScopePath.reversed(ASTScope.Idx(idx) :: ASTScope.Key(variant.key) :: ASTScope.Key(resourceName) :: Nil),
        )
      }
    }

    resourcesAndScopes.traverse { case (resource, scope) => buildResource(resource, scope) }
  }

  private def buildOutput(key: String, value: PetaformAST.Obj, scope: ScopePath): Either[ScopedError, BlockSet] =
    toBlockSet(s"output ${key.unesc}", value, scope :+ ASTScope.Key(key))

  private def buildOutputs(resourceGroups: ResourceGroups): Either[ScopedError, List[BlockSet]] =
    resourceGroups.resourceGroups.toList
      .traverse { case (resourceName, variant) =>
        variant.value.outputs.getOrElse(Map.empty).toList.traverse { case (b, obj) =>
          buildOutput(
            b,
            obj,
            ScopePath.reversed(ASTScope.Key(variant.key) :: ASTScope.Key(resourceName) :: Nil),
          )
        }
      }
      .map(_.flatten)

}
