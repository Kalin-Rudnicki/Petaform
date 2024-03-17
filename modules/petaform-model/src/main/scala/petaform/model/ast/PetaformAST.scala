package petaform.model.ast

import petaform.model.*
import petaform.model.conversion.*
import petaform.model.typeclass.ASTDecoder

sealed trait PetaformAST {

  final def decodeTo[A: ASTDecoder]: Either[ScopedError, A] = ASTDecoder[A].decode(this)

  final def format: String = FormatAST(this)

}
object PetaformAST {

  sealed trait Simple extends PetaformAST
  sealed trait Complex extends PetaformAST

  sealed trait StringLike extends PetaformAST

  final case class Raw(value: String) extends PetaformAST.Simple with StringLike

  final case class Str(str: String) extends PetaformAST.Simple with StringLike

  final case class EofStr(lines: List[String]) extends PetaformAST.Complex with StringLike

  final case class Obj(elems: List[(String, PetaformAST)]) extends PetaformAST.Complex {
    val map: Map[String, PetaformAST] = elems.toMap
  }
  object Obj {
    val empty: Obj = Obj(Nil)
    def apply(elems: (String, PetaformAST)*): Obj = Obj(elems.toList)
  }

  final case class Arr(elems: List[PetaformAST]) extends PetaformAST.Complex
  object Arr {
    def apply(elems: PetaformAST*): Arr = Arr(elems.toList)
  }

  case object Null extends PetaformAST.Simple

  case object Empty extends PetaformAST.Simple

  case object Undef extends PetaformAST.Simple

}
