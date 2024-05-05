package petaform.model.conversion

import cats.syntax.either.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import zio.json.*
import zio.json.ast.*

object ASTToJson {

  def apply(ast: PetaformAST): Either[ScopedError, String] =
    toAst(ScopePath.empty, ast).map(_.toJson)

  // =====|  |=====

  private def toAst(scope: ScopePath, ast: PetaformAST): Either[ScopedError, Json] =
    ast match {
      case PetaformAST.Raw(value) =>
        value.toIntOption
          .map(Json.Num(_))
          .orElse(value.toDoubleOption.map(Json.Num(_)))
          .orElse(value.toBooleanOption.map(Json.Bool(_)))
          .getOrElse(Json.Str(value))
          .asRight
      case PetaformAST.Str(str)      => Json.Str(str).asRight
      case PetaformAST.EofStr(lines) => Json.Str(lines.mkString("\n")).asRight
      case PetaformAST.Null          => Json.Null.asRight
      case PetaformAST.Obj(elems) =>
        elems
          .traverse { case (key, value) => toAst(scope :+ ASTScope.Key(key), value).map((key, _)) }
          .map { elems => Json.Obj(elems*) }
      case PetaformAST.Arr(elems) =>
        elems.zipWithIndex
          .traverse { case (value, idx) => toAst(scope :+ ASTScope.Idx(idx), value) }
          .map { elems => Json.Arr(elems*) }
      case PetaformAST.Empty => ScopedError(scope, "Can not convert 'Empty' to json").asLeft
      case PetaformAST.Undef => ScopedError(scope, "Can not convert 'Undef' to json").asLeft
    }

}
