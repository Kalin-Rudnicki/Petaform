package petaform.model.ast

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.conversion.*
import scala.annotation.tailrec

sealed trait RawPetaformAST {
  final def format: String = FormatRawAST(this)
}
object RawPetaformAST {

  sealed trait Simple extends RawPetaformAST
  sealed trait Complex extends RawPetaformAST

  final case class Raw(value: String) extends RawPetaformAST.Simple

  final case class Str(str: InterpolatedString) extends RawPetaformAST.Simple

  final case class EofStr(lines: List[InterpolatedString]) extends RawPetaformAST.Complex

  final case class FlatInterpolation(interpolation: Interpolation) extends RawPetaformAST.Simple

  final case class Obj(elems: List[(String, Obj.Value)]) extends RawPetaformAST.Complex
  object Obj {

    // NOTE : Need to handle merging duplicate keys
    def makeUnsafe(elems: (String, Obj.Value)*): Obj = Obj(elems.toList)
    def makeSafe(elems: (String, Obj.Value)*): Either[ScopedError, Obj] =
      if (elems.isEmpty) Obj(Nil).asRight
      else
        RawPetaformAST.merge(elems.toList.map(tup => Obj(tup :: Nil))).flatMap {
          case obj: Obj => obj.asRight
          case ast      => ScopedError(ScopePath.empty, s"Expected Object but got ${ast.getClass.getSimpleName}").asLeft
        }

    sealed trait Value
    object Value {
      case object Required extends Value
      final case class Provided(const: Boolean, value: RawPetaformAST) extends Value
    }

  }

  final case class Arr(elems: List[RawPetaformAST]) extends RawPetaformAST.Complex
  object Arr {

    def make(elems: RawPetaformAST*): Arr = Arr(elems.toList)

  }

  case object Null extends RawPetaformAST.Simple

  case object Empty extends RawPetaformAST.Simple

  case object Undef extends RawPetaformAST.Simple

  // =====|  |=====

  private def mergeObjValues(scope: ScopePath, value1: RawPetaformAST.Obj.Value, value2: RawPetaformAST.Obj.Value): Either[ScopedError, RawPetaformAST.Obj.Value] =
    (value1, value2) match {
      case (RawPetaformAST.Obj.Value.Provided(true, _), _)                                  => ScopedError(scope, "Can not override const value").asLeft
      case (RawPetaformAST.Obj.Value.Provided(false, _), RawPetaformAST.Obj.Value.Required) => value1.asRight
      case (RawPetaformAST.Obj.Value.Provided(false, value1), RawPetaformAST.Obj.Value.Provided(const, value2)) =>
        merge2(scope, value1, value2).map(RawPetaformAST.Obj.Value.Provided(const, _))
      case (RawPetaformAST.Obj.Value.Required, _) => value2.asRight
    }

  @tailrec
  private def mergeArrayValues(
      scope: ScopePath,
      idx: Int,
      elems1: List[RawPetaformAST],
      elems2: List[RawPetaformAST],
      rStack: List[RawPetaformAST],
  ): Either[ScopedError, List[RawPetaformAST]] =
    (elems1, elems2) match {
      case (e1H :: e1T, e2H :: e2T) =>
        merge2(scope :+ ASTScope.Idx(idx), e1H, e2H) match {
          case Right(value) => mergeArrayValues(scope, idx + 1, e1T, e2T, value :: rStack)
          case Left(error)  => error.asLeft
        }
      case (e1H :: e1T, Nil) => mergeArrayValues(scope, idx + 1, e1T, Nil, e1H :: rStack)
      case (Nil, e2H :: e2T) => mergeArrayValues(scope, idx + 1, e2T, Nil, e2H :: rStack)
      case (Nil, Nil)        => rStack.reverse.asRight
    }

  private def merge2(scope: ScopePath, ast1: RawPetaformAST, ast2: RawPetaformAST): Either[ScopedError, RawPetaformAST] =
    (ast1, ast2) match {
      case (ast1: RawPetaformAST.Obj, ast2: RawPetaformAST.Obj) =>
        val map1 = ast1.elems.toMap
        val map2 = ast2.elems.toMap

        val keys1 = ast1.elems.map(_._1)
        val keys2 = ast2.elems.map(_._1).filterNot(keys1.contains)

        (keys1 ::: keys2)
          .traverse { key =>
            (map1.get(key), map2.get(key)) match {
              case (Some(value1), Some(value2)) => mergeObjValues(scope :+ ASTScope.Key(key), value1, value2).map((key, _).some)
              case (Some(value1), None)         => (key, value1).some.asRight
              case (None, Some(value2))         => (key, value2).some.asRight
              case (None, None)                 => None.asRight
            }
          }
          .map { elems => RawPetaformAST.Obj(elems.flatten) }
      case (ast1: RawPetaformAST.Arr, ast2: RawPetaformAST.Arr) =>
        mergeArrayValues(scope, 0, ast1.elems, ast2.elems, Nil).map(RawPetaformAST.Arr(_))
      case _ => ast2.asRight
    }

  def merge(asts: List[RawPetaformAST]): Either[ScopedError, RawPetaformAST] = {
    @tailrec
    def loop(
        current: RawPetaformAST,
        queue: List[RawPetaformAST],
    ): Either[ScopedError, RawPetaformAST] =
      queue match {
        case head :: tail =>
          merge2(ScopePath.empty, current, head) match {
            case Right(newCurrent) => loop(newCurrent, tail)
            case Left(error)       => error.asLeft
          }
        case Nil =>
          current.asRight
      }

    loop(RawPetaformAST.Undef, asts)
  }

  def merge(asts: RawPetaformAST*): Either[ScopedError, RawPetaformAST] =
    merge(asts.toList)

}
