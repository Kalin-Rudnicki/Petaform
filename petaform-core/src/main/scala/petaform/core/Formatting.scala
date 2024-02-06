package petaform.core

import cats.syntax.option.*
import harness.core.*
import java.util.regex.{Matcher, Pattern}
import petaform.core.TerraformAST.Value

object Formatting {

  object rawAST {

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
        case RawPetaformAST.RawValue(value)       => value
        case RawPetaformAST.Str(str)              => str.show
        case RawPetaformAST.Interp(interpolation) => interpolation.show
        case RawPetaformAST.Undef                 => "undef"
        case RawPetaformAST.Null                  => "null"
        case RawPetaformAST.Empty                 => ""
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

  object ast {

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
        case PetaformAST.RawValue(value) => value.replaceAll(Pattern.quote("$"), Matcher.quoteReplacement("$$"))
        case PetaformAST.Str(str)        => str.unesc.replaceAll(Pattern.quote("$"), Matcher.quoteReplacement("$$"))
        case PetaformAST.Undef           => "undef"
        case PetaformAST.Null            => "null"
        case PetaformAST.Empty           => ""
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
      }

  }

  object terraformAST {

    def apply(ast: TerraformAST): String =
      astToIdtStr(ast).toString("  ")

    def apply(asts: List[TerraformAST]): String =
      IndentedString.Inline(asts.map(astToIdtStr)).toString("  ")

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

}
