package petaform.core

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import harness.zio.*
import petaform.core.parser.ASTParser
import scala.annotation.tailrec
import slyce.core.*
import zio.*

sealed trait RawPetaformAST
object RawPetaformAST {

  sealed trait Simple extends RawPetaformAST
  sealed trait Complex extends RawPetaformAST

  final case class RawValue(value: String) extends RawPetaformAST.Simple

  final case class Str(str: InterpolatedString) extends RawPetaformAST.Simple

  final case class Interp(interpolation: Interpolation) extends RawPetaformAST.Simple

  final case class Obj(elems: List[(String, Obj.Value)]) extends RawPetaformAST.Complex
  object Obj {

    // NOTE : Need to handle merging duplicate keys
    def makeUnsafe(elems: (String, Obj.Value)*): Obj = Obj(elems.toList)
    def makeSafe(elems: (String, Obj.Value)*): Either[ScopedError, Obj] =
      if (elems.isEmpty) Obj(Nil).asRight
      else
        RawPetaformAST.merge(elems.toList.map(tup => Obj(tup :: Nil))).flatMap {
          case obj: Obj => obj.asRight
          case ast      => ScopedError(Nil, s"Expected Object but got ${ast.getClass.getSimpleName}").asLeft
        }

    sealed trait Value
    object Value {
      case object Required extends Value
      final case class Provided(const: Boolean, value: RawPetaformAST) extends Value
    }

  }

  final case class Arr(elems: List[RawPetaformAST]) extends RawPetaformAST.Complex
  object Arr {

    def make(elems: RawPetaformAST*): Arr = Arr(elems.toList)

  }

  case object Null extends RawPetaformAST.Simple

  case object Empty extends RawPetaformAST.Simple

  case object Undef extends RawPetaformAST.Simple

  // =====|  |=====

  private def mergeObjValues(rScope: List[ASTScope], value1: RawPetaformAST.Obj.Value, value2: RawPetaformAST.Obj.Value): Either[ScopedError, RawPetaformAST.Obj.Value] =
    (value1, value2) match {
      case (RawPetaformAST.Obj.Value.Provided(true, _), _)                                  => ScopedError(rScope.reverse, "Can not override const value").asLeft
      case (RawPetaformAST.Obj.Value.Provided(false, _), RawPetaformAST.Obj.Value.Required) => value1.asRight
      case (RawPetaformAST.Obj.Value.Provided(false, value1), RawPetaformAST.Obj.Value.Provided(const, value2)) =>
        merge2(rScope, value1, value2).map(RawPetaformAST.Obj.Value.Provided(const, _))
      case (RawPetaformAST.Obj.Value.Required, _) => value2.asRight
    }

  @tailrec
  private def mergeArrayValues(
      rScope: List[ASTScope],
      idx: Int,
      elems1: List[RawPetaformAST],
      elems2: List[RawPetaformAST],
      rStack: List[RawPetaformAST],
  ): Either[ScopedError, List[RawPetaformAST]] =
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

  private def merge2(rScope: List[ASTScope], ast1: RawPetaformAST, ast2: RawPetaformAST): Either[ScopedError, RawPetaformAST] =
    (ast1, ast2) match {
      case (ast1: RawPetaformAST.Obj, ast2: RawPetaformAST.Obj) =>
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
          .map { elems => RawPetaformAST.Obj(elems.flatten) }
      case (ast1: RawPetaformAST.Arr, ast2: RawPetaformAST.Arr) =>
        mergeArrayValues(rScope, 0, ast1.elems, ast2.elems, Nil).map(RawPetaformAST.Arr(_))
      case _ => ast2.asRight
    }

  def merge(asts: List[RawPetaformAST]): Either[ScopedError, RawPetaformAST] = {
    @tailrec
    def loop(
        current: RawPetaformAST,
        queue: List[RawPetaformAST],
    ): Either[ScopedError, RawPetaformAST] =
      queue match {
        case head :: tail =>
          merge2(Nil, current, head) match {
            case Right(newCurrent) => loop(newCurrent, tail)
            case Left(error)       => error.asLeft
          }
        case Nil =>
          current.asRight
      }

    loop(RawPetaformAST.Undef, asts)
  }

  def merge(asts: RawPetaformAST*): Either[ScopedError, RawPetaformAST] =
    merge(asts.toList)

  // =====|  |=====

  private def toInterpolation(interp: ASTParser.NonTerminal.Interpolation): Interpolation =
    interp match {
      case ASTParser.NonTerminal.Interpolation._1(_, _, path, _)   => Interpolation.Config(path.toNonEmptyList.map(tok => ASTScope.Key(tok.text)))
      case ASTParser.NonTerminal.Interpolation._2(_, _, _, key, _) => Interpolation.EnvVar(key.text)
    }

  private def toValue(value: ASTParser.NonTerminal.Value): RawPetaformAST =
    value.lift match {
      case interp: ASTParser.NonTerminal.Interpolation => RawPetaformAST.Interp(toInterpolation(interp))
      case ASTParser.NonTerminal.String(_, parts, _) =>
        @tailrec
        def mkString(
            rParts: List[Either[String, Interpolation]],
            currentString: Option[String],
            stack: List[(Interpolation, String)],
        ): RawPetaformAST.Str =
          rParts match {
            case Right(interp) :: tail => mkString(tail, None, (interp, currentString.getOrElse("")) :: stack)
            case Left(str) :: tail     => mkString(tail, currentString.fold(str)(str + _).some, stack)
            case Nil                   => RawPetaformAST.Str(InterpolatedString(currentString.getOrElse(""), stack))
          }

        mkString(
          parts.toList
            .map(_.lift)
            .map {
              case ASTParser.Terminal.chars(text, _)           => text.asLeft
              case ASTParser.Terminal.escChar(text, _)         => text(1).toString.asLeft
              case interp: ASTParser.NonTerminal.Interpolation => toInterpolation(interp).asRight
            }
            .reverse,
          None,
          Nil,
        )
    }

  private sealed trait TmpLine
  private object TmpLine {
    final case class RequiredKey(numSpaces: Int, dash: Boolean, key: String) extends TmpLine
    final case class Key(numSpaces: Int, dash: Boolean, key: String, const: Boolean, value: Option[RawPetaformAST]) extends TmpLine
    final case class Arr(numSpaces: Int, value: Option[RawPetaformAST]) extends TmpLine

    def fromRaw(raw: ASTParser.NonTerminal.Line): Option[TmpLine] =
      raw match {
        case ASTParser.NonTerminal.Line._1(_)                                  => None
        case ASTParser.NonTerminal.Line._2(spaces, dash, key, _, _)            => RequiredKey(spaces.toList.size, dash.toOption.nonEmpty, key.text).some
        case ASTParser.NonTerminal.Line._3(spaces, dash, key, const, _, value) => Key(spaces.toList.size, dash.toOption.nonEmpty, key.text, const.toOption.isDefined, value.toOption.map(toValue)).some
        case ASTParser.NonTerminal.Line._4(spaces, _, value)                   => Arr(spaces.toList.size, value.toOption.map(toValue)).some
      }
  }

  private sealed trait State {
    val depth: Int

    final def toAst: RawPetaformAST =
      this match {
        case State.Obj(_, elems) => RawPetaformAST.Obj(elems)
        case State.Arr(_, elems) => RawPetaformAST.Arr(elems)
      }

  }
  private object State {
    final case class Obj(depth: Int, elems: List[(String, RawPetaformAST.Obj.Value)]) extends State
    final case class Arr(depth: Int, elems: List[RawPetaformAST]) extends State
  }

  private def freshState(line: TmpLine): State =
    line match {
      case TmpLine.RequiredKey(numSpaces, dash, key) =>
        val elems: List[(String, RawPetaformAST.Obj.Value)] =
          List(key -> RawPetaformAST.Obj.Value.Required)
        if (dash) State.Arr(numSpaces, RawPetaformAST.Obj(elems) :: Nil)
        else State.Obj(numSpaces, elems)
      case TmpLine.Key(numSpaces, dash, key, const, value) =>
        val elems: List[(String, RawPetaformAST.Obj.Value)] =
          List(key -> RawPetaformAST.Obj.Value.Provided(const, value.getOrElse(RawPetaformAST.Empty)))
        if (dash) State.Arr(numSpaces, RawPetaformAST.Obj(elems) :: Nil)
        else State.Obj(numSpaces, elems)
      case TmpLine.Arr(numSpaces, value) =>
        State.Arr(numSpaces, value.getOrElse(RawPetaformAST.Empty) :: Nil)
    }

  private def reduceStack(stack: List[State]): List[State] =
    stack match {
      case (s1: State.Obj) :: (s2: State.Obj) :: tail if s1.depth == s2.depth => State.Obj(s1.depth, s1.elems ::: s2.elems) :: tail
      case (s1: State.Arr) :: (s2: State.Arr) :: tail if s1.depth == s2.depth => State.Arr(s1.depth, s1.elems ::: s2.elems) :: tail
      case _                                                                  => stack
    }

  @tailrec
  private def assembleLines(
      rLines: List[TmpLine],
      inputStack: List[State],
  ): Validated[RawPetaformAST] = {
    val stack: List[State] = reduceStack(inputStack)

    // TODO (KR) : Remove once confident all necessary cases are supported
    /*
    val showLines =
      rLines.reverse.map {
        case TmpLine.RequiredKey(numSpaces, dash, key)       => s"\n  - ($numSpaces, $dash) <RequiredKey> @required '$key'"
        case TmpLine.Key(numSpaces, dash, key, const, value) => s"\n  - ($numSpaces, $dash) <Key>${if (const) " @const" else ""} '$key'${value.fold("")(v => s" : $v")}"
        case TmpLine.Arr(numSpaces, value)                   => s"\n  - ($numSpaces) <Arr> ${value.fold("")(_.toString)}"
      }.mkString

    val showStack =
      IndentedString.inline(
        s"stack [${stack.size}]:",
        IndentedString.indented(
          stack.map[IndentedString] {
            case State.Obj(depth, elems) =>
              IndentedString.inline(
                s"- Obj($depth)",
                IndentedString.indented(
                  Formatting.rawAST(RawPetaformAST.Obj(elems)).split("\n").toList,
                ),
              )
            case State.Arr(depth, elems) =>
              IndentedString.inline(
                s"- Arr($depth)",
                IndentedString.indented(
                  Formatting.rawAST(RawPetaformAST.Arr(elems)).split("\n").toList,
                ),
              )
          },
        ),
      )

    println(" ")
    println(s"rLines [${rLines.size}]:$showLines")
    println(showStack.toString("   "))
     */

    rLines match {
      case lHead :: lTail =>
        stack match {
          case sHead :: sTail =>
            (lHead, sHead) match {
              case (line: TmpLine.Key, state: State.Obj) if line.dash && line.numSpaces == sHead.depth - 1 =>
                val elems = (line.key -> RawPetaformAST.Obj.Value.Provided(line.const, line.value.getOrElse(RawPetaformAST.Empty))) :: state.elems
                val obj = RawPetaformAST.Obj.makeUnsafe(elems*) // TODO (KR) :
                assembleLines(lTail, State.Arr(line.numSpaces, obj :: Nil) :: sTail)
              case (line: TmpLine.Key, state) if line.dash && line.numSpaces == sHead.depth - 2 =>
                val tmpState =
                  State.Obj(line.numSpaces + 1, List(line.key -> RawPetaformAST.Obj.Value.Provided(line.const, state.toAst)))

                assembleLines(
                  lTail,
                  reduceStack(tmpState :: sTail) match {
                    case sHead :: sTail =>
                      State.Arr(
                        line.numSpaces,
                        sHead.toAst :: Nil,
                      ) :: sTail
                    case Nil => ??? // TODO (KR) :
                  },
                )
              case (line: TmpLine.Key, state) if !line.dash && line.numSpaces == state.depth - 1 =>
                line.value match {
                  case None =>
                    val child = state.toAst
                    assembleLines(lTail, State.Obj(line.numSpaces, List(line.key -> RawPetaformAST.Obj.Value.Provided(line.const, child))) :: sTail)
                  case Some(_) => ??? // TODO (KR) :
                }
              case (line: TmpLine.Key, state: State.Obj) if !line.dash && line.numSpaces == state.depth =>
                assembleLines(
                  lTail,
                  State.Obj(line.numSpaces, (line.key -> RawPetaformAST.Obj.Value.Provided(line.const, line.value.getOrElse(RawPetaformAST.Empty))) :: state.elems) :: sTail,
                )
              case (line: TmpLine.Key, state) if line.numSpaces > state.depth =>
                assembleLines(lTail, freshState(line) :: stack)
              case _ => throw new RuntimeException(s"Invalid combo\n  - $lHead\n  - $sHead)")
            }
          case Nil =>
            assembleLines(
              lTail,
              freshState(lHead) :: Nil,
            )
        }
      case Nil =>
        stack match {
          case head :: Nil if head.depth == 0 => head.toAst.asRight
          case _                              => throw new RuntimeException("Unexpected start of file") // TODO (KR) :
        }
    }
  }

  def fromSourceEither(source: Source): Validated[RawPetaformAST] =
    for {
      ast <- ASTParser.parse(source)
      lines = ast._1.toNonEmptyList.toList.flatMap(TmpLine.fromRaw)
      assembled <- assembleLines(lines.reverse, Nil)
    } yield assembled
  def fromSource(source: Source): HTask[RawPetaformAST] =
    fromSourceEither(source) match {
      case Right(value) => ZIO.succeed(value)
      case Left(errors) => ZIO.fail(HError.UserError(source.mark(errors.toList.map(marked => Marked(s"${marked.value} - ${marked.span}", Span.Unknown)))))
    }
  def fromPath(path: Path): HTask[RawPetaformAST] =
    for {
      _ <- path.ensureExists
      _ <- ZIO.fail(HError.UserError(s"Not a file: ${path.show}")).unlessZIO(path.isFile)
      contents <- path.readString
      source = Source(contents, path.show.some)
      res <- fromSource(source)
    } yield res
  def fromPathString(pathString: String): HRIO[FileSystem, RawPetaformAST] =
    Path(pathString).flatMap(fromPath)

}
