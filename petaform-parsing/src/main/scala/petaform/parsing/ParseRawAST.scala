package petaform.parsing

import cats.syntax.either.*
import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*
import scala.annotation.tailrec
import slyce.core.*
import zio.*

object ParseRawAST {

  def fromSourceEither(source: Source): Validated[RawPetaformAST] =
    for {
      ast <- RawASTParser.parse(source)
      lines = ast._1.toNonEmptyList.toList.flatMap(TmpLine.fromRaw)
      assembled <- assembleLines(lines.reverse, Nil)
    } yield assembled
  def fromSource(source: Source): HTask[RawPetaformAST] =
    Errors.validatedToTask(fromSourceEither(source))
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

  // =====|  |=====

  private sealed trait TmpLine {
    val depth: Int
    val span: Span.Highlight
  }
  private object TmpLine {
    final case class RequiredKey(depth: Int, dash: Boolean, key: String, span: Span.Highlight) extends TmpLine
    final case class Key(depth: Int, dash: Boolean, key: String, const: Boolean, value: Option[RawPetaformAST], span: Span.Highlight) extends TmpLine
    final case class Arr(depth: Int, value: Option[RawPetaformAST], span: Span.Highlight) extends TmpLine

    def fromRaw(raw: RawASTParser.NonTerminal.Line): Option[TmpLine] =
      raw match {
        case RawASTParser.NonTerminal.Line._1(_)                           => None
        case RawASTParser.NonTerminal.Line._2(spaces, dash, key, _, colon) => RequiredKey(spaces.toList.size, dash.toOption.nonEmpty, key.text, colon.span).some
        case RawASTParser.NonTerminal.Line._3(spaces, dash, key, const, colon, value) =>
          Key(spaces.toList.size, dash.toOption.nonEmpty, key.text, const.toOption.isDefined, value.toOption.map(toValue), colon.span).some
        case RawASTParser.NonTerminal.Line._4(spaces, dash, value) => Arr(spaces.toList.size, value.toOption.map(toValue), dash.span).some
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

  // =====|  |=====

  // TODO (KR) : Remove once confident all necessary cases are supported
  @scala.annotation.unused
  private def logAssembleLinesVars(rLines: List[TmpLine], stack: List[State]): Unit = {
    val showLines =
      rLines.reverse.map {
        case TmpLine.RequiredKey(numSpaces, dash, key, _)       => s"\n  - ($numSpaces, $dash) <RequiredKey> @required '$key'"
        case TmpLine.Key(numSpaces, dash, key, const, value, _) => s"\n  - ($numSpaces, $dash) <Key>${if (const) " @const" else ""} '$key'${value.fold("")(v => s" : $v")}"
        case TmpLine.Arr(numSpaces, value, _)                   => s"\n  - ($numSpaces) <Arr> ${value.fold("")(_.toString)}"
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
                  RawPetaformAST.Obj(elems).format.split("\n").toList,
                ),
              )
            case State.Arr(depth, elems) =>
              IndentedString.inline(
                s"- Arr($depth)",
                IndentedString.indented(
                  RawPetaformAST.Arr(elems).format.split("\n").toList,
                ),
              )
          },
        ),
      )

    println(" ")
    println(s"rLines [${rLines.size}]:$showLines")
    println(showStack.toString("   "))
  }

  // =====|  |=====

  private def toInterpolation(interp: RawASTParser.NonTerminal.Interpolation): Interpolation =
    interp match {
      case RawASTParser.NonTerminal.Interpolation._1(_, _, path, functions, _) =>
        Interpolation(Interpolation.Source.Config(path.toNonEmptyList.map(tok => ASTScope.Key(tok.text))), functions.toList.map(_.lift.text))
      case RawASTParser.NonTerminal.Interpolation._2(_, _, _, key, functions, _) =>
        Interpolation(Interpolation.Source.EnvVar(key.text), functions.toList.map(_.lift.text))
    }

  def mkString(parts: List[RawASTParser.NonTerminal.StringPart]): InterpolatedString = {
    @tailrec
    def loop(
        rParts: List[Either[String, Interpolation]],
        currentString: Option[String],
        stack: List[(Interpolation, String)],
    ): InterpolatedString =
      rParts match {
        case Right(interp) :: tail => loop(tail, None, (interp, currentString.getOrElse("")) :: stack)
        case Left(str) :: tail     => loop(tail, currentString.fold(str)(str + _).some, stack)
        case Nil                   => InterpolatedString(currentString.getOrElse(""), stack)
      }

    loop(
      parts
        .map(_.lift)
        .map {
          case RawASTParser.Terminal.chars(text, _) => text.asLeft
          case RawASTParser.Terminal.escChar(text, _) =>
            (text(1) match {
              case 'n' => "\n"
              case 't' => "\t"
              case c   => c
            }).toString.asLeft
          case interp: RawASTParser.NonTerminal.Interpolation => toInterpolation(interp).asRight
        }
        .reverse,
      None,
      Nil,
    )
  }

  private def toValue(value: RawASTParser.NonTerminal.Value): RawPetaformAST =
    value.lift match {
      case interp: RawASTParser.NonTerminal.Interpolation => RawPetaformAST.FlatInterpolation(toInterpolation(interp))
      case RawASTParser.NonTerminal.String(_, parts, _) =>
        RawPetaformAST.Str(mkString(parts.toList))
      case raw: RawASTParser.Terminal.raw =>
        RawPetaformAST.Raw(raw.text)
      case eofString: RawASTParser.NonTerminal.EofString =>
        RawPetaformAST.EofStr(eofString._3.toList.map(line => mkString(line.toNonEmptyList.toList)))
    }

  private def freshState(line: TmpLine): State =
    line match {
      case TmpLine.RequiredKey(numSpaces, dash, key, _) =>
        val elems: List[(String, RawPetaformAST.Obj.Value)] =
          List(key -> RawPetaformAST.Obj.Value.Required)
        if (dash) State.Arr(numSpaces, RawPetaformAST.Obj(elems) :: Nil)
        else State.Obj(numSpaces, elems)
      case TmpLine.Key(numSpaces, dash, key, const, value, _) =>
        val elems: List[(String, RawPetaformAST.Obj.Value)] =
          List(key -> RawPetaformAST.Obj.Value.Provided(const, value.getOrElse(RawPetaformAST.Empty)))
        if (dash) State.Arr(numSpaces, RawPetaformAST.Obj(elems) :: Nil)
        else State.Obj(numSpaces, elems)
      case TmpLine.Arr(numSpaces, value, _) =>
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
    // logAssembleLinesVars(rLines, stack)

    rLines match {
      case lHead :: lTail =>
        stack match {
          case sHead :: sTail =>
            (lHead, sHead) match {
              // =====| Deeper |=====
              case _ if lHead.depth > sHead.depth =>
                assembleLines(
                  lTail,
                  freshState(lHead) :: stack,
                )

              // =====| Key |=====
              case (line: TmpLine.Key, _) if !line.dash && line.value.isEmpty && line.depth + 1 == sHead.depth =>
                val child = sHead.toAst
                val elems = List(line.key -> RawPetaformAST.Obj.Value.Provided(line.const, child))
                assembleLines(lTail, State.Obj(line.depth, elems) :: sTail)
              case (line: TmpLine.Key, _) if line.dash && line.value.isEmpty && line.depth + 2 == sHead.depth =>
                val child = sHead.toAst
                val elems = List(line.key -> RawPetaformAST.Obj.Value.Provided(line.const, child))
                reduceStack(State.Obj(line.depth + 1, elems) :: sTail) match {
                  case sHead :: sTail =>
                    assembleLines(lTail, State.Arr(line.depth, sHead.toAst :: Nil) :: sTail)
                  case Nil =>
                    Marked("reduceStack returned empty list", line.span).leftNel
                }
              case (line: TmpLine.Key, head: State.Obj) if !line.dash && line.depth == head.depth =>
                val elems = (line.key -> RawPetaformAST.Obj.Value.Provided(line.const, line.value.getOrElse(RawPetaformAST.Empty))) :: head.elems
                assembleLines(
                  lTail,
                  State.Obj(line.depth, elems) :: sTail,
                )
              case (line: TmpLine.Key, head: State.Obj) if line.dash && line.depth + 1 == head.depth =>
                val elems = (line.key -> RawPetaformAST.Obj.Value.Provided(line.const, line.value.getOrElse(RawPetaformAST.Empty))) :: head.elems
                val obj = RawPetaformAST.Obj(elems) // TODO (KR) : use safe constructor
                assembleLines(
                  lTail,
                  State.Arr(line.depth, obj :: Nil) :: sTail,
                )

              // =====| Arr |=====
              case (line: TmpLine.Arr, state: State.Arr) if line.depth == state.depth =>
                assembleLines(
                  lTail,
                  State.Arr(
                    line.depth,
                    line.value.getOrElse(RawPetaformAST.Empty) :: state.elems,
                  ) :: sTail,
                )

              // =====| RequiredKey |=====
              case (line: TmpLine.RequiredKey, head: State.Obj) if !line.dash && line.depth == head.depth =>
                val elems = (line.key -> RawPetaformAST.Obj.Value.Required) :: head.elems
                assembleLines(
                  lTail,
                  State.Obj(line.depth, elems) :: sTail,
                )
              case (line: TmpLine.RequiredKey, head: State.Obj) if line.dash && line.depth + 1 == head.depth =>
                val elems = (line.key -> RawPetaformAST.Obj.Value.Required) :: head.elems
                val obj = RawPetaformAST.Obj(elems) // TODO (KR) : use safe constructor
                assembleLines(
                  lTail,
                  State.Arr(line.depth, obj :: Nil) :: sTail,
                )

              // =====| Specific Errors |=====

              // =====| Fallback |=====
              case _ => Marked(s"Invalid combo\n  - $lHead\n  - $sHead)", lHead.span).leftNel
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
          case _                              => Marked("Unexpected start of file", Span.Unknown).leftNel
        }
    }
  }

}
