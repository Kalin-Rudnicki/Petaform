package petaform.model.typeclass

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import harness.deriving.{K0, *}
import petaform.model.*
import petaform.model.ast.*
import scala.reflect.ClassTag

trait ASTDecoder[A] { self =>

  def decodeInternal(scope: ScopePath, ast: PetaformAST): Either[ScopedError, A]

  def ifMissing: Option[A] = None

  final def decode(ast: PetaformAST): Either[ScopedError, A] = self.decodeInternal(ScopePath.empty, ast)

  final def map[B](f: A => B): ASTDecoder[B] =
    self.decodeInternal(_, _).map(f)

  final def mapOrFail[B](f: A => Either[String, B]): ASTDecoder[B] =
    (scope, ast) => self.decodeInternal(scope, ast).flatMap(f(_).leftMap(ScopedError(scope, _)))

}
object ASTDecoder extends ASTDecoderLowPriority {

  def apply[A](implicit dec: ASTDecoder[A]): ASTDecoder[A] = dec

  implicit val objectAST: ASTDecoder[PetaformAST.Obj] = {
    case (_, obj: PetaformAST.Obj) => obj.asRight
    case (_, PetaformAST.Empty)    => PetaformAST.Obj().asRight
    case (scope, a)                => ScopedError(scope, s"Invalid sub-type of PetaformAST, expected Object, got ${a.getClass.getSimpleName}").asLeft
  }
  implicit val arrayAST: ASTDecoder[PetaformAST.Arr] = {
    case (_, arr: PetaformAST.Arr) => arr.asRight
    case (_, PetaformAST.Empty)    => PetaformAST.Arr().asRight
    case (scope, a)                => ScopedError(scope, s"Invalid sub-type of PetaformAST, expected Object, got ${a.getClass.getSimpleName}").asLeft
  }

  implicit def map[K: StringDecoder, V: ASTDecoder]: ASTDecoder[Map[K, V]] = {
    case (scope, PetaformAST.Obj(elems)) =>
      elems
        .traverse { case (key, value) =>
          val newScope = scope :+ ASTScope.Key(key)
          for {
            decodedKey <- StringDecoder[K].decode(key).leftMap(e => ScopedError(newScope, s"Unable to decode map-key: $e"))
            decodedValue <- ASTDecoder[V].decodeInternal(newScope, value)
          } yield (decodedKey, decodedValue)
        }
        .map(_.toMap)
    case (_, PetaformAST.Empty) => Map.empty[K, V].asRight
    case (scope, ast) =>
      ScopedError(scope, s"Expected Object, but got ${ast.getClass.getSimpleName}").asLeft
  }

  implicit def list[V: ASTDecoder]: ASTDecoder[List[V]] = {
    case (scope, PetaformAST.Arr(elems)) =>
      elems.zipWithIndex.traverse { case (value, idx) =>
        val newScope = scope :+ ASTScope.Idx(idx)
        ASTDecoder[V].decodeInternal(newScope, value)
      }
    case (_, PetaformAST.Empty) => List.empty[V].asRight
    case (scope, ast) =>
      ScopedError(scope, s"Expected Array, but got ${ast.getClass.getSimpleName}").asLeft
  }

  implicit def option[A: ASTDecoder]: ASTDecoder[Option[A]] =
    new ASTDecoder[Option[A]] {
      override def decodeInternal(scope: ScopePath, ast: PetaformAST): Either[ScopedError, Option[A]] =
        ast match {
          case PetaformAST.Null => None.asRight
          case _                => ASTDecoder[A].decodeInternal(scope, ast).map(_.some)
        }
      override def ifMissing: Option[Option[A]] = None.some
    }

  implicit def fromStringDecoder[A: StringDecoder]: ASTDecoder[A] = {
    case (scope, PetaformAST.StringLike(str)) => StringDecoder[A].decode(str).leftMap(ScopedError(scope, _))
    case (scope, ast) =>
      ScopedError(scope, s"Expected Raw/Str, but got ${ast.getClass.getSimpleName}").asLeft
  }

  // TODO (KR) : instances

}

trait ASTDecoderLowPriority extends ASTDecoderLowPriority2 {

  implicit def fromCodec[A: ASTCodec]: ASTDecoder[A] = ASTCodec[A].decoder

  implicit def ast[A <: PetaformAST](implicit ct: ClassTag[A]): ASTDecoder[A] = {
    case (_, ct(a)) => a.asRight
    case (scope, a) => ScopedError(scope, s"Invalid sub-type of PetaformAST, expected ${ct.runtimeClass.getSimpleName}, got ${a.getClass.getSimpleName}").asLeft
  }

}

trait ASTDecoderLowPriority2 extends K0.Derivable[ASTDecoder] {

  override implicit inline def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[ASTDecoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.ProductInstances.of[A, ASTDecoder]

    Derived {
      new ASTDecoder[A] {
        override def decodeInternal(scope: ScopePath, ast: PetaformAST): Either[ScopedError, A] =
          ast match {
            case PetaformAST.Obj(elems) =>
              instances.withoutInstance.mapInstantiateEither.withLabels(labels) {
                [t] => { (l: String, i: ASTDecoder[t]) =>
                  val newScope = scope :+ ASTScope.Key(l)

                  elems.find(_._1 == l) match {
                    case Some((_, value)) =>
                      i.decodeInternal(newScope, value)
                    case None =>
                      i.ifMissing.toRight { ScopedError(newScope, s"<${labels.label}> Missing required value, provided keys: ${elems.map(_._1).sorted.mkString("[", ", ", "]")}") }
                  }
                }
              }
            case _ =>
              ScopedError(scope, s"Expected <${labels.label}> Object, but got ${ast.getClass.getSimpleName}").asLeft
          }
      }
    }
  }

  override implicit inline def genSum[A](implicit m: K0.SumGeneric[A]): Derived[ASTDecoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.SumInstances.of[A, ASTDecoder]

    Derived {
      new ASTDecoder[A] {
        override def decodeInternal(scope: ScopePath, ast: PetaformAST): Either[ScopedError, A] =
          ast match {
            case PetaformAST.Obj((key, value) :: Nil) =>
              val newScope = scope :+ ASTScope.Key(key)
              instances.instanceFromLabels(labels, key) match {
                case Some(i) =>
                  i.decodeInternal(newScope, value)
                case None =>
                  ScopedError(newScope, s"Invalid key, expected one of: ${labels.elemLabels.mkString(", ")}").asLeft
              }
            case PetaformAST.Obj(elems) =>
              ScopedError(scope, s"Expected single element Object, but had size ${elems.size} with keys: ${elems.map(_._1.unesc).mkString(", ")}").asLeft
            case _ =>
              ScopedError(scope, s"Expected <${labels.label}> Object, but got ${ast.getClass.getSimpleName}").asLeft
          }
      }
    }
  }

}
