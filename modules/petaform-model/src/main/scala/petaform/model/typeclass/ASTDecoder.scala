package petaform.model.typeclass

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import harness.deriving.{K0, *}
import petaform.model.*
import petaform.model.ast.*

trait ASTDecoder[A] { self =>

  def decodeInternal(rPath: List[ASTScope], ast: PetaformAST): Either[ScopedError, A]

  def ifMissing: Option[A] = None

  final def decode(ast: PetaformAST): Either[ScopedError, A] = self.decodeInternal(Nil, ast)

  final def map[B](f: A => B): ASTDecoder[B] =
    self.decodeInternal(_, _).map(f)

  final def mapOrFail[B](f: A => Either[String, B]): ASTDecoder[B] =
    (rPath, ast) => self.decodeInternal(rPath, ast).flatMap(f(_).leftMap(ScopedError(rPath.reverse, _)))

}
object ASTDecoder extends ASTDecoderLowPriority {

  def apply[A](implicit dec: ASTDecoder[A]): ASTDecoder[A] = dec

  implicit val ast: ASTDecoder[PetaformAST] = (_, ast) => ast.asRight
  implicit val objAst: ASTDecoder[PetaformAST.Obj] = {
    case (_, obj: PetaformAST.Obj) => obj.asRight
    case (_, PetaformAST.Empty)    => PetaformAST.Obj(Nil).asRight
    case (rPath, ast)              => ScopedError(rPath.reverse, s"Expected Object, but got ${ast.getClass.getSimpleName}").asLeft
  }

  implicit def map[K: StringDecoder, V: ASTDecoder]: ASTDecoder[Map[K, V]] = {
    case (rPath, PetaformAST.Obj(elems)) =>
      elems
        .traverse { case (key, value) =>
          val newRPath = ASTScope.Key(key) :: rPath
          for {
            decodedKey <- StringDecoder[K].decode(key).leftMap(e => ScopedError(newRPath.reverse, s"Unable to decode map-key: $e"))
            decodedValue <- ASTDecoder[V].decodeInternal(newRPath, value)
          } yield (decodedKey, decodedValue)
        }
        .map(_.toMap)
    case (_, PetaformAST.Empty) => Map.empty[K, V].asRight
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Object, but got ${ast.getClass.getSimpleName}").asLeft
  }

  implicit def list[V: ASTDecoder]: ASTDecoder[List[V]] = {
    case (rPath, PetaformAST.Arr(elems)) =>
      elems.zipWithIndex.traverse { case (value, idx) =>
        val newRPath = ASTScope.Idx(idx) :: rPath
        ASTDecoder[V].decodeInternal(newRPath, value)
      }
    case (_, PetaformAST.Empty) => List.empty[V].asRight
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Array, but got ${ast.getClass.getSimpleName}").asLeft
  }

  implicit def option[A: ASTDecoder]: ASTDecoder[Option[A]] =
    new ASTDecoder[Option[A]] {
      override def decodeInternal(rPath: List[ASTScope], ast: PetaformAST): Either[ScopedError, Option[A]] =
        ast match {
          case PetaformAST.Null => None.asRight
          case _                => ASTDecoder[A].decodeInternal(rPath, ast).map(_.some)
        }
      override def ifMissing: Option[Option[A]] = None.some
    }

  implicit def fromStringDecoder[A: StringDecoder]: ASTDecoder[A] = {
    case (rPath, PetaformAST.Raw(str))      => StringDecoder[A].decode(str).leftMap(ScopedError(rPath.reverse, _))
    case (rPath, PetaformAST.Str(str))      => StringDecoder[A].decode(str).leftMap(ScopedError(rPath.reverse, _))
    case (rPath, PetaformAST.EofStr(lines)) => StringDecoder[A].decode(lines.mkString("\n")).leftMap(ScopedError(rPath.reverse, _))
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Raw/Str, but got ${ast.getClass.getSimpleName}").asLeft
  }

  // TODO (KR) : instances

}

trait ASTDecoderLowPriority extends ASTDecoderLowPriority2 {

  implicit def fromCodec[A: ASTCodec]: ASTDecoder[A] = ASTCodec[A].decoder

}

trait ASTDecoderLowPriority2 extends K0.Derivable[ASTDecoder] {

  override implicit inline def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[ASTDecoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.ProductInstances.of[A, ASTDecoder]

    Derived {
      new ASTDecoder[A] {
        override def decodeInternal(rPath: List[ASTScope], ast: PetaformAST): Either[ScopedError, A] =
          ast match {
            case PetaformAST.Obj(elems) =>
              instances.withoutInstance.mapInstantiateEither.withLabels(labels) {
                [t] => { (l: String, i: ASTDecoder[t]) =>
                  val newRPath = ASTScope.Key(l) :: rPath

                  elems.find(_._1 == l) match {
                    case Some((_, value)) =>
                      i.decodeInternal(newRPath, value)
                    case None =>
                      i.ifMissing.toRight { ScopedError(newRPath.reverse, s"<${labels.label}> Missing required value, provided keys: ${elems.map(_._1).sorted.mkString("[", ", ", "]")}") }
                  }
                }
              }
            case _ =>
              ScopedError(rPath.reverse, s"Expected <${labels.label}> Object, but got ${ast.getClass.getSimpleName}").asLeft
          }
      }
    }
  }

  override implicit inline def genSum[A](implicit m: K0.SumGeneric[A]): Derived[ASTDecoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.SumInstances.of[A, ASTDecoder]

    Derived {
      new ASTDecoder[A] {
        override def decodeInternal(rPath: List[ASTScope], ast: PetaformAST): Either[ScopedError, A] =
          ast match {
            case PetaformAST.Obj((key, value) :: Nil) =>
              val newRPath = ASTScope.Key(key) :: rPath
              instances.instanceFromLabels(labels, key) match {
                case Some(i) =>
                  i.decodeInternal(newRPath, value)
                case None =>
                  ScopedError(newRPath.reverse, s"Invalid key, expected one of: ${labels.elemLabels.mkString(", ")}").asLeft
              }
            case PetaformAST.Obj(elems) =>
              ScopedError(rPath.reverse, s"Expected single element Object, but had size ${elems.size} with keys: ${elems.map(_._1.unesc).mkString(", ")}").asLeft
            case _ =>
              ScopedError(rPath.reverse, s"Expected <${labels.label}> Object, but got ${ast.getClass.getSimpleName}").asLeft
          }
      }
    }
  }

}
