package petaform.core

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import shapeless3.deriving.*

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
    case (rPath, PetaformAST.RawValue(str)) => StringDecoder[A].decode(str).leftMap(ScopedError(rPath.reverse, _))
    case (rPath, PetaformAST.Str(str))      => StringDecoder[A].decode(str).leftMap(ScopedError(rPath.reverse, _))
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Raw/Str, but got ${ast.getClass.getSimpleName}").asLeft
  }

  // TODO (KR) : instances

}

trait ASTDecoderLowPriority extends ASTDecoderLowPriority2 {

  implicit def fromCodec[A: ASTCodec]: ASTDecoder[A] = ASTCodec[A].decoder

}

trait ASTDecoderLowPriority2 {

  given astProductDecoderGen[A](using inst: => K0.ProductInstances[ASTDecoder, A], labels: => Labelling[A]): ASTDecoder[A] = {
    case (rPath, PetaformAST.Obj(elems)) =>
      inst.unfold(0.asRight[ScopedError]) {
        [t] =>
          (acc: Either[ScopedError, Int], dec: ASTDecoder[t]) =>
            acc match {
              case Right(idx) =>
                val label = labels.elemLabels(idx)
                val newRPath = ASTScope.Key(label) :: rPath
                elems.find(_._1 == label) match {
                  case Some(value) =>
                    dec.decodeInternal(newRPath, value._2) match {
                      case Right(value) => ((idx + 1).asRight, value.some)
                      case Left(error)  => (error.asLeft, None)
                    }
                  case None =>
                    dec.ifMissing match {
                      case Some(value) => ((idx + 1).asRight, value.some)
                      case None        => (ScopedError(newRPath.reverse, "Missing required value").asLeft, None)
                    }
                }
              case Left(error) =>
                (error.asLeft, None)
          }
      } match {
        case (Right(_), Some(res)) => res.asRight
        case (Left(error), None)   => error.asLeft
        case _                     => ??? // not possible
      }
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Object, but got ${ast.getClass.getSimpleName}").asLeft
  }

  given astCoproductDecoderGen[A](using inst: => K0.CoproductInstances[ASTDecoder, A], labels: => Labelling[A]): ASTDecoder[A] = {
    case (rPath, PetaformAST.Obj(elems)) =>
      elems match {
        case (key, value) :: Nil =>
          val newRPath = ASTScope.Key(key) :: rPath
          labels.elemLabels.indexOf(key) match {
            case -1  => ScopedError(newRPath.reverse, s"Invalid key, expected one of: ${labels.elemLabels.mkString(", ")}").asLeft
            case idx => inst.is(idx).asInstanceOf[ASTDecoder[A]].decodeInternal(newRPath, value)
          }
        case _ => ScopedError(rPath.reverse, s"Expected single element Object, but had size ${elems.size}").asLeft
      }
    case (rPath, ast) =>
      ScopedError(rPath.reverse, s"Expected Object, but got ${ast.getClass.getSimpleName}").asLeft
  }

  inline def derived[A](using gen: K0.Generic[A]): ASTDecoder[A] =
    gen.derive(astProductDecoderGen[A], astCoproductDecoderGen[A])

}
