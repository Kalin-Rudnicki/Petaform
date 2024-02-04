package petaform.core

import cats.syntax.option.*
import harness.core.*

object Formatting {

  object ast1 {

    def apply(ast: Stage1PetaformAST): String = {
      val idtStr: IndentedString =
        ast match {
          case ast: Stage1PetaformAST.Simple =>
            simple(ast)
          case ast: Stage1PetaformAST.Complex =>
            complex(ast).map { case (header, children) => IndentedString.inline(header, children) }
        }

      idtStr.toString("  ")
    }

    private def simple(ast: Stage1PetaformAST.Simple): String =
      ast match {
        case Stage1PetaformAST.RawValue(value)       => value
        case Stage1PetaformAST.Str(str)              => str.show
        case Stage1PetaformAST.Interp(interpolation) => interpolation.show
        case Stage1PetaformAST.Undef                 => "undef"
      }

    private def complex(ast: Stage1PetaformAST.Complex): List[(String, Option[IndentedString])] =
      ast match {
        case Stage1PetaformAST.Obj(elems) =>
          elems.map[(String, Option[IndentedString])] {
            case (key, Stage1PetaformAST.Obj.Value.Required) =>
              (s"$key @required :", None)
            case (key, Stage1PetaformAST.Obj.Value.Provided(const, value)) =>
              val constAnnotation: String = if (const) " @const " else ""
              value match {
                case value: Stage1PetaformAST.Simple =>
                  (s"$key$constAnnotation: ${simple(value)}", None)
                case value: Stage1PetaformAST.Complex =>
                  val children = complex(value)

                  (
                    s"$key$constAnnotation:",
                    Option.when(children.nonEmpty) {
                      IndentedString.indented(children.map { case (header, children) => IndentedString.inline(header, children) })
                    },
                  )
              }
          }
        case Stage1PetaformAST.Arr(elems) =>
          elems.map[(String, Option[IndentedString])] {
            case value: Stage1PetaformAST.Simple =>
              (s"- ${simple(value)}", None)
            case value: Stage1PetaformAST.Complex =>
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

  object ast2 {

    def apply(ast: Stage2PetaformAST): String = {
      val idtStr: IndentedString =
        ast match {
          case ast: Stage2PetaformAST.Simple =>
            simple(ast)
          case ast: Stage2PetaformAST.Complex =>
            complex(ast).map { case (header, children) => IndentedString.inline(header, children) }
        }

      idtStr.toString("  ")
    }

    private def simple(ast: Stage2PetaformAST.Simple): String =
      ast match {
        case Stage2PetaformAST.RawValue(value) => value
        case Stage2PetaformAST.Str(str)        => str.unesc
        case Stage2PetaformAST.Undef           => "undef"
      }

    private def complex(ast: Stage2PetaformAST.Complex): List[(String, Option[IndentedString])] =
      ast match {
        case Stage2PetaformAST.Obj(elems) =>
          elems.map[(String, Option[IndentedString])] { case (key, value) =>
            value match {
              case value: Stage2PetaformAST.Simple =>
                (s"$key: ${simple(value)}", None)
              case value: Stage2PetaformAST.Complex =>
                val children = complex(value)

                (
                  s"$key:",
                  Option.when(children.nonEmpty) {
                    IndentedString.indented(children.map { case (header, children) => IndentedString.inline(header, children) })
                  },
                )
            }
          }
        case Stage2PetaformAST.Arr(elems) =>
          elems.map[(String, Option[IndentedString])] {
            case value: Stage2PetaformAST.Simple =>
              (s"- ${simple(value)}", None)
            case value: Stage2PetaformAST.Complex =>
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
