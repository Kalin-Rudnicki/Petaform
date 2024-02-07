package petaform.model.conversion

import cats.syntax.option.*
import harness.core.*
import petaform.model.ast.*

object FormatRawAST {

  def apply(ast: RawPetaformAST): String = {
    val idtStr: IndentedString =
      ast match {
        case ast: RawPetaformAST.Simple =>
          simple(ast)
        case ast: RawPetaformAST.Complex =>
          complex(ast).map { case (header, children) => IndentedString.inline(header, children) }
      }

    idtStr.toString("  ")
  }

  private def simple(ast: RawPetaformAST.Simple): String =
    ast match {
      case RawPetaformAST.Raw(value)                       => value
      case RawPetaformAST.Str(str)                         => str.show
      case RawPetaformAST.FlatInterpolation(interpolation) => interpolation.show
      case RawPetaformAST.Undef                            => "undef"
      case RawPetaformAST.Null                             => "null"
      case RawPetaformAST.Empty                            => ""
    }

  private def complex(ast: RawPetaformAST.Complex): List[(String, Option[IndentedString])] =
    ast match {
      case RawPetaformAST.Obj(elems) =>
        elems.map[(String, Option[IndentedString])] {
          case (key, RawPetaformAST.Obj.Value.Required) =>
            (s"$key @required :", None)
          case (key, RawPetaformAST.Obj.Value.Provided(const, value)) =>
            val constAnnotation: String = if (const) " @const " else ""
            value match {
              case value: RawPetaformAST.Simple =>
                (s"$key$constAnnotation: ${simple(value)}", None)
              case value: RawPetaformAST.Complex =>
                val children = complex(value)

                (
                  s"$key$constAnnotation:",
                  Option.when(children.nonEmpty) {
                    IndentedString.indented(children.map { case (header, children) => IndentedString.inline(header, children) })
                  },
                )
            }
        }
      case RawPetaformAST.Arr(elems) =>
        elems.map[(String, Option[IndentedString])] {
          case value: RawPetaformAST.Simple =>
            (s"- ${simple(value)}", None)
          case value: RawPetaformAST.Complex =>
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
    }

}
