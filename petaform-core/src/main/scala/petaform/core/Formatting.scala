package petaform.core

import cats.syntax.option.*
import harness.core.*
import java.util.regex.{Matcher, Pattern}

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

}
