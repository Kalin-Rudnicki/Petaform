package petaform.model.conversion

import cats.syntax.option.*
import harness.core.*
import petaform.model.ast.*
import petaform.model.ast.TerraformAST.*

object FormatTerraform {

  def apply(ast: TerraformAST): String =
    astToIdtStr(ast).toString("  ")

  def apply(asts: List[TerraformAST]): String =
    IndentedString.Inline(asts.map(astToIdtStr)).toString("  ")

  // =====|  |=====

  private def astToIdtStr(ast: TerraformAST): IndentedString =
    ast match {
      case TerraformAST.BlockSet(key, Nil) => s"$key {}"
      case TerraformAST.BlockSet(key, elems) =>
        IndentedString.inline(
          s"$key {",
          IndentedString.indented(
            elems.map(astToIdtStr),
          ),
          "}",
        )
      case TerraformAST.KeyValue(key, value) =>
        valueToIdtStr(value, "") match {
          case (firstLine, None) => s"$key = $firstLine"
          case (firstLine, Some(other)) =>
            IndentedString.inline(
              s"$key = $firstLine",
              other,
            )
        }
    }

  private def valueToIdtStr(value: TerraformAST.Value, suffix: String): (String, Option[IndentedString]) =
    value match {
      case Value.Raw(value) => (s"$value$suffix", None)
      case Value.Str(str)   => (s"${str.unesc}$suffix", None)
      case Value.Arr(Nil)   => (s"[]$suffix", None)
      case Value.Arr(elems) =>
        (
          "[",
          IndentedString
            .inline(
              IndentedString.indented(
                elems.map(valueToIdtStr(_, ",")).map { case (firstLine, other) => IndentedString.inline(firstLine, other) },
              ),
              s"]$suffix",
            )
            .some,
        )
      case Value.Map(Nil) => (s"{}$suffix", None)
      case Value.Map(elems) =>
        (
          "{",
          IndentedString
            .inline(
              IndentedString.indented(
                elems.map[IndentedString] { case (key, value) =>
                  valueToIdtStr(value, "") match {
                    case (firstLine, None) => s"$key = $firstLine"
                    case (firstLine, Some(other)) =>
                      IndentedString.inline(
                        s"$key = $firstLine",
                        other,
                      )
                  }
                },
              ),
              s"}$suffix",
            )
            .some,
        )
    }

}
