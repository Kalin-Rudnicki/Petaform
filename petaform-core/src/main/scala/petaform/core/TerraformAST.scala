package petaform.core

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import harness.zio.*
import zio.*

sealed trait TerraformAST
object TerraformAST {

  final case class BlockSet(key: String, elems: List[TerraformAST]) extends TerraformAST
  final case class KeyValue(key: String, value: Value) extends TerraformAST

  sealed trait Value
  object Value {
    final case class Raw(value: String) extends Value
    final case class Str(str: String) extends Value
    final case class Arr(elems: List[Value]) extends Value
    final case class Map(elems: List[(String, Value)]) extends Value
  }

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

  private def toValue(ast: PetaformAST): HTask[Value] =
    ast match {
      case PetaformAST.RawValue(value) => ZIO.succeed(Value.Raw(value))
      case PetaformAST.Str(str)        => ZIO.succeed(Value.Str(str))
      case PetaformAST.Null            => ZIO.succeed(Value.Raw("null"))
      case PetaformAST.Obj(elems)      => ZIO.foreach(elems) { case (k, v) => toValue(v).map((k, _)) }.map(Value.Map(_))
      case PetaformAST.Arr(elems)      => ZIO.foreach(elems)(toValue).map(Value.Arr(_))
      case PetaformAST.Empty           => ZIO.fail(HError.InternalDefect("Empty -> TerraformAST not supported"))
      case PetaformAST.Undef           => ZIO.fail(HError.InternalDefect("Undef -> TerraformAST not supported"))
    }

  private def toAsts(key: String, ast: PetaformAST): HTask[List[TerraformAST]] =
    ast match {
      case ArrayOfObjects(objs) => toBlockSets(key, objs)
      case _                    => toValue(ast).map(KeyValue(key, _) :: Nil)
    }

  private def toBlockSet(key: String, ast: PetaformAST.Obj): HTask[BlockSet] =
    ZIO.foreach(ast.elems)(toAsts(_, _)).map(_.flatten).map(BlockSet(key, _))
  private def toBlockSets(key: String, asts: List[PetaformAST.Obj]): HTask[List[BlockSet]] =
    ZIO.foreach(asts)(toBlockSet(key, _))

  private def buildProviders(built: Parts.Built): HRIO[Logger, (BlockSet, List[BlockSet])] =
    for {
      _ <- Logger.log.detailed("Building providers")
      all = built.resources.resourceGroups.toList.flatMap(_._2.value.providers.toList)
      grouped = all.groupMap(_._1)(_._2)
      providerMap <- ZIO.fromEither {
        grouped.toList
          .traverse { case (name, values) =>
            values.distinct match {
              case head :: Nil => (name, head).asRight
              case _ :: _      => HError.UserError(s"provider '$name' has multiple values").asLeft
              case Nil         => HError.InternalDefect(s"provider '$name' has no values?").asLeft
            }
          }
          .map(_.toMap)
      }
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
      providerBlocks <- ZIO.foreach(providerList) { case (key, value) =>
        toBlockSet(s"provider ${key.unesc}", value.config)
      }
    } yield (requiredProviders, providerBlocks)

  private def buildResource(resource: Parts.Resource): HTask[BlockSet] =
    toBlockSet(s"resource ${resource.base.`type`.unesc} ${resource.base.name.unesc}", resource.config)

  private def buildResources(built: Parts.Built): HRIO[Logger, List[BlockSet]] =
    for {
      _ <- Logger.log.detailed("Building resources")
      all = built.resources.resourceGroups.toList.flatMap(_._2.value.resources)
      blockSets <- ZIO.foreach(all)(buildResource)
    } yield blockSets

  def fromBuilt(built: Parts.Built): HRIO[Logger, List[TerraformAST.BlockSet]] =
    for {
      (requiredProviders, providers) <- buildProviders(built)
      resources <- buildResources(built)
      terraformBlockSet = BlockSet("terraform", requiredProviders :: Nil)
    } yield terraformBlockSet :: providers ::: resources

}
