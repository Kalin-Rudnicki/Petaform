package petaform.model.conversion

import cats.syntax.either.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import zio.json.*
import zio.json.ast.*

object ASTToJson {

  def apply(ast: PetaformAST): Either[ScopedError, String] =
    toAst(Nil, ast).map(_.toJson)

  // =====|  |=====

  private def toAst(rScope: List[ASTScope], ast: PetaformAST): Either[ScopedError, Json] =
    ast match {
      case PetaformAST.Raw(value)    => Json.Str(value).asRight
      case PetaformAST.Str(str)      => Json.Str(str).asRight
      case PetaformAST.EofStr(lines) => Json.Str(lines.mkString("\n")).asRight
      case PetaformAST.Null          => Json.Null.asRight
      case PetaformAST.Obj(elems) =>
        elems
          .traverse { case (key, value) => toAst(ASTScope.Key(key) :: rScope, value).map((key, _)) }
          .map { elems => Json.Obj(elems*) }
      case PetaformAST.Arr(elems) =>
        elems.zipWithIndex
          .traverse { case (value, idx) => toAst(ASTScope.Idx(idx) :: rScope, value) }
          .map { elems => Json.Arr(elems*) }
      case PetaformAST.Empty => ScopedError(rScope.reverse, "Can not convert 'Empty' to json").asLeft
      case PetaformAST.Undef => ScopedError(rScope.reverse, "Can not convert 'Undef' to json").asLeft
    }

}
