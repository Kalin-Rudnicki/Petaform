package petaform.core

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import scala.annotation.tailrec

sealed trait Stage1PetaformAST
object Stage1PetaformAST {

  sealed trait Simple extends Stage1PetaformAST
  sealed trait Complex extends Stage1PetaformAST

  final case class RawValue(value: String) extends Stage1PetaformAST.Simple

  final case class Str(str: InterpolatedString) extends Stage1PetaformAST.Simple

  final case class Interp(interpolation: Interpolation) extends Stage1PetaformAST.Simple

  final case class Obj(elems: List[(String, Obj.Value)]) extends Stage1PetaformAST.Complex
  object Obj {

    // NOTE : Need to handle merging duplicate keys
    def makeUnsafe(elems: (String, Obj.Value)*): Obj = Obj(elems.toList)

    sealed trait Value
    object Value {
      case object Required extends Value
      final case class Provided(const: Boolean, value: Stage1PetaformAST) extends Value
    }

  }

  final case class Arr(elems: List[Stage1PetaformAST]) extends Stage1PetaformAST.Complex
  object Arr {

    def make(elems: Stage1PetaformAST*): Arr = Arr(elems.toList)

  }

  case object Undef extends Stage1PetaformAST.Simple

  // =====|  |=====

  private def mergeObjValues(rScope: List[ASTScope], value1: Stage1PetaformAST.Obj.Value, value2: Stage1PetaformAST.Obj.Value): Either[ScopedError, Stage1PetaformAST.Obj.Value] =
    (value1, value2) match {
      case (Stage1PetaformAST.Obj.Value.Provided(true, _), _)                                     => ScopedError(rScope.reverse, "Can not override const value").asLeft
      case (Stage1PetaformAST.Obj.Value.Provided(false, _), Stage1PetaformAST.Obj.Value.Required) => value1.asRight
      case (Stage1PetaformAST.Obj.Value.Provided(false, value1), Stage1PetaformAST.Obj.Value.Provided(const, value2)) =>
        merge2(rScope, value1, value2).map(Stage1PetaformAST.Obj.Value.Provided(const, _))
      case (Stage1PetaformAST.Obj.Value.Required, _) => value2.asRight
    }

  @tailrec
  private def mergeArrayValues(
      rScope: List[ASTScope],
      idx: Int,
      elems1: List[Stage1PetaformAST],
      elems2: List[Stage1PetaformAST],
      rStack: List[Stage1PetaformAST],
  ): Either[ScopedError, List[Stage1PetaformAST]] =
    (elems1, elems2) match {
      case (e1H :: e1T, e2H :: e2T) =>
        merge2(ASTScope.Idx(idx) :: rScope, e1H, e2H) match {
          case Right(value) => mergeArrayValues(rScope, idx + 1, e1T, e2T, value :: rStack)
          case Left(error)  => error.asLeft
        }
      case (e1H :: e1T, Nil) => mergeArrayValues(rScope, idx + 1, e1T, Nil, e1H :: rStack)
      case (Nil, e2H :: e2T) => mergeArrayValues(rScope, idx + 1, e2T, Nil, e2H :: rStack)
      case (Nil, Nil)        => rStack.reverse.asRight
    }

  private def merge2(rScope: List[ASTScope], ast1: Stage1PetaformAST, ast2: Stage1PetaformAST): Either[ScopedError, Stage1PetaformAST] =
    (ast1, ast2) match {
      case (ast1: Stage1PetaformAST.Obj, ast2: Stage1PetaformAST.Obj) =>
        val map1 = ast1.elems.toMap
        val map2 = ast2.elems.toMap

        val keys1 = ast1.elems.map(_._1)
        val keys2 = ast2.elems.map(_._1).filterNot(keys1.contains)

        (keys1 ::: keys2)
          .traverse { key =>
            (map1.get(key), map2.get(key)) match {
              case (Some(value1), Some(value2)) => mergeObjValues(ASTScope.Key(key) :: rScope, value1, value2).map((key, _).some)
              case (Some(value1), None)         => (key, value1).some.asRight
              case (None, Some(value2))         => (key, value2).some.asRight
              case (None, None)                 => None.asRight
            }
          }
          .map { elems => Stage1PetaformAST.Obj(elems.flatten) }
      case (ast1: Stage1PetaformAST.Arr, ast2: Stage1PetaformAST.Arr) =>
        mergeArrayValues(rScope, 0, ast1.elems, ast2.elems, Nil).map(Stage1PetaformAST.Arr(_))
      case _ => ast2.asRight
    }

  def merge(asts: List[Stage1PetaformAST]): Either[ScopedError, Stage1PetaformAST] = {
    @tailrec
    def loop(
        current: Stage1PetaformAST,
        queue: List[Stage1PetaformAST],
    ): Either[ScopedError, Stage1PetaformAST] =
      queue match {
        case head :: tail =>
          merge2(Nil, current, head) match {
            case Right(newCurrent) => loop(newCurrent, tail)
            case Left(error)       => error.asLeft
          }
        case Nil =>
          current.asRight
      }

    loop(Stage1PetaformAST.Undef, asts)
  }

  def merge(asts: Stage1PetaformAST*): Either[ScopedError, Stage1PetaformAST] =
    merge(asts.toList)

}
