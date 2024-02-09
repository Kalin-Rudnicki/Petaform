package petaform.model.conversion

import cats.syntax.option.*
import harness.core.*
import java.util.regex.{Matcher, Pattern}
import petaform.model.ast.*

object FormatAST {

  def apply(ast: PetaformAST): String = {
    val idtStr: IndentedString =
      ast match {
        case ast: PetaformAST.Simple =>
          simple(ast)
        case ast: PetaformAST.Complex =>
          complex(ast).map { case (header, children) => IndentedString.inline(header, children) }
      }

    idtStr.toString("  ")
  }

  private def simple(ast: PetaformAST.Simple): String =
    ast match {
      case PetaformAST.Raw(value) => value.replaceAll(Pattern.quote("$"), Matcher.quoteReplacement("$$"))
      case PetaformAST.Str(str)   => str.unesc.replaceAll(Pattern.quote("$"), Matcher.quoteReplacement("$$"))
      case PetaformAST.Undef      => "undef"
      case PetaformAST.Null       => "null"
      case PetaformAST.Empty      => ""
    }

  private def complex(ast: PetaformAST.Complex): List[(String, Option[IndentedString])] =
    ast match {
      case PetaformAST.Obj(elems) =>
        elems.map[(String, Option[IndentedString])] { case (key, value) =>
          value match {
            case value: PetaformAST.Simple =>
              (s"$key: ${simple(value)}", None)
            case value: PetaformAST.Complex =>
              val children = complex(value)

              (
                s"$key:",
                Option.when(children.nonEmpty) {
                  IndentedString.indented(children.map { case (header, children) => IndentedString.inline(header, children) })
                },
              )
          }
        }
      case PetaformAST.Arr(elems) =>
        elems.map[(String, Option[IndentedString])] {
          case value: PetaformAST.Simple =>
            (s"- ${simple(value)}", None)
          case value: PetaformAST.Complex =>
            complex(value) match {
              case (line0Header, None) :: Nil =>
                (
                  s"- $line0Header",
                  None,
                )
              case (line0Header, line0Children) :: tail =>
                (
                  s"- $line0Header",
                  IndentedString
                    .indented(
                      line0Children,
                      tail.map { case (header, children) => IndentedString.inline(header, children) },
                    )
                    .some,
                )
              case Nil =>
                (s"- ", None)
            }
        }
      case PetaformAST.EofStr(lines) =>
        (
          "<<-EOF",
          IndentedString.indented(lines :+ "EOF").some,
        ) :: Nil
    }

}
