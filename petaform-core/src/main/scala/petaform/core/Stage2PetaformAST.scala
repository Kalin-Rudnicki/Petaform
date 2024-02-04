package petaform.core

import cats.syntax.either.*
import cats.syntax.traverse.*
import scala.annotation.tailrec

sealed trait Stage2PetaformAST
object Stage2PetaformAST {

  sealed trait Simple extends Stage2PetaformAST
  sealed trait Complex extends Stage2PetaformAST

  final case class RawValue(value: String) extends Stage2PetaformAST.Simple

  final case class Str(str: String) extends Stage2PetaformAST.Simple

  final case class Obj(elems: List[(String, Stage2PetaformAST)]) extends Stage2PetaformAST.Complex {
    val map: Map[String, Stage2PetaformAST] = elems.toMap
  }

  final case class Arr(elems: List[Stage2PetaformAST]) extends Stage2PetaformAST.Complex

  case object Undef extends Stage2PetaformAST.Simple

  // =====|  |=====

  private final case class Dependency(
      pair: Dependency.Pair,
      dependsOn: Set[List[ASTScope]],
  )
  private object Dependency {

    final case class Pair(
        path: List[ASTScope],
        interp: Either[Interpolation, InterpolatedString],
    )

  }

  private def getInterpolations(scopePath: List[ASTScope], ast: Stage1PetaformAST): List[Dependency] =
    ast match {
      case Stage1PetaformAST.RawValue(_) => Nil
      case Stage1PetaformAST.Str(str) =>
        val deps = str.pairs.map(_._1).collect { case Interpolation.Config(path) => path.toList }.toSet
        if (deps.isEmpty) Nil
        else Dependency(Dependency.Pair(scopePath, str.asRight), deps) :: Nil
      case Stage1PetaformAST.Interp(interpolation) =>
        interpolation match {
          case Interpolation.Config(path) => Dependency(Dependency.Pair(scopePath, interpolation.asLeft), Set(path.toList)) :: Nil
          case Interpolation.EnvVar(_)    => Nil
        }
      case Stage1PetaformAST.Undef => Nil
      case Stage1PetaformAST.Obj(elems) =>
        elems.flatMap {
          case (_, Stage1PetaformAST.Obj.Value.Required)             => Nil
          case (key, Stage1PetaformAST.Obj.Value.Provided(_, value)) => getInterpolations(scopePath :+ ASTScope.Key(key), value)
        }
      case Stage1PetaformAST.Arr(elems) =>
        elems.zipWithIndex.flatMap { case (elem, idx) =>
          getInterpolations(scopePath :+ ASTScope.Idx(idx), elem)
        }
    }

  private def topologicalSort(dependencies: List[Dependency]): Either[ScopedError, List[Dependency.Pair]] = {
    @tailrec
    def loop(
        toSort: Map[List[ASTScope], Set[List[ASTScope]]],
        rSorted: List[List[ASTScope]],
    ): Either[ScopedError, List[Dependency.Pair]] =
      if (toSort.isEmpty) {
        val depMap = dependencies.map(dep => dep.pair.path -> dep).toMap
        rSorted.reverse.map(depMap(_).pair).asRight
      } else {
        val keysWithoutValues: Set[List[ASTScope]] = toSort.collect { case (k, v) if v.isEmpty => k }.toSet
        val valuesWithoutKey: Set[List[ASTScope]] = toSort.values.flatten.toSet.filterNot(toSort.contains)
        val valuesToConsume: Set[List[ASTScope]] = keysWithoutValues | valuesWithoutKey

        if (valuesToConsume.isEmpty) {
          val unusedString =
            toSort.toList.map { case (k, v) =>
              s"\n  - ${ASTScope.format("CFG", k)} -> ${v.toList.map(ASTScope.format("CFG", _)).mkString(" / ")}"
            }.mkString

          ScopedError(Nil, s"Cyclic interpolation dependencies:$unusedString").asLeft
        } else
          loop(
            toSort.removedAll(valuesToConsume).map { case (key, value) => (key, value &~ valuesToConsume) },
            keysWithoutValues.toList.sortBy(ASTScope.format("CFG", _)).reverse ::: rSorted,
          )
      }

    loop(
      dependencies.map { dep => dep.pair.path -> dep.dependsOn }.toMap,
      Nil,
    )
  }

  private def getEnvVar(rScope: List[ASTScope], varName: String, envVars: Map[String, String]): Either[ScopedError, String] =
    envVars.get(varName) match {
      case Some(varValue) => varValue.asRight
      case None           => ScopedError(rScope, s"Missing env var '$varName'").asLeft
    }

  private def initialStage2(rScope: List[ASTScope], ast: Stage1PetaformAST, envVars: Map[String, String]): Either[ScopedError, Stage2PetaformAST] =
    ast match {
      case Stage1PetaformAST.RawValue(value) => Stage2PetaformAST.RawValue(value).asRight
      case Stage1PetaformAST.Str(str) =>
        @tailrec
        def loop(
            toInterp: List[(Interpolation, String)],
            rStack: List[String],
        ): Either[ScopedError, Stage2PetaformAST] =
          toInterp match {
            case (_: Interpolation.Config, _) :: _ => Undef.asRight
            case (Interpolation.EnvVar(varName), sH) :: tail =>
              getEnvVar(rScope, varName, envVars) match {
                case Right(varValue) => loop(tail, sH :: varValue :: rStack)
                case Left(error)     => error.asLeft
              }
            case Nil =>
              Stage2PetaformAST.Str(rStack.reverse.mkString).asRight
          }

        loop(str.pairs, str.prefix :: Nil)
      case Stage1PetaformAST.Interp(interpolation) =>
        interpolation match {
          case Interpolation.EnvVar(varName) =>
            getEnvVar(rScope, varName, envVars).map(Stage2PetaformAST.Str(_))
          case _: Interpolation.Config => Undef.asRight
        }
      case Stage1PetaformAST.Undef => Stage2PetaformAST.Undef.asRight
      case Stage1PetaformAST.Obj(elems) =>
        elems
          .traverse {
            case (key, Stage1PetaformAST.Obj.Value.Required) =>
              ScopedError((ASTScope.Key(key) :: rScope).reverse, "Missing required value").asLeft
            case (key, Stage1PetaformAST.Obj.Value.Provided(_, value)) =>
              initialStage2(ASTScope.Key(key) :: rScope, value, envVars).map((key, _))
          }
          .map(Stage2PetaformAST.Obj(_))
      case Stage1PetaformAST.Arr(elems) =>
        elems.zipWithIndex
          .traverse { case (value, idx) =>
            initialStage2(ASTScope.Idx(idx) :: rScope, value, envVars)
          }
          .map(Stage2PetaformAST.Arr(_))
    }

  @tailrec
  private def getFromAst2(fromScope: List[ASTScope], rLookupScope: List[ASTScope], lookupScope: List[ASTScope], ast: Stage2PetaformAST): Either[ScopedError, Stage2PetaformAST] =
    lookupScope match {
      case head :: tail =>
        val newRScope = head :: rLookupScope
        (head, ast) match {
          case (ASTScope.Key(key), Stage2PetaformAST.Obj(elems)) =>
            elems.find(_._1 == key) match {
              case Some((_, value)) => getFromAst2(fromScope, newRScope, tail, value)
              case None             => ScopedError(fromScope, s"Missing key for interpolation: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            }
          case (ASTScope.Idx(idx), Stage2PetaformAST.Arr(elems)) =>
            if (idx >= elems.size) ScopedError(fromScope, s"Idx out of bounds: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            else getFromAst2(fromScope, newRScope, tail, elems(idx))
          case (_: ASTScope.Key, _) =>
            ScopedError(newRScope.reverse, s"Expected object, but got ${ast.getClass.getSimpleName}").asLeft
          case (_: ASTScope.Idx, _) =>
            ScopedError(newRScope.reverse, s"Expected array, but got ${ast.getClass.getSimpleName}").asLeft
        }
      case Nil => ast.asRight
    }

  private def interpolate(fromScope: List[ASTScope], interp: Interpolation, ast: Stage2PetaformAST, envVars: Map[String, String]): Either[ScopedError, Stage2PetaformAST] =
    interp match {
      case Interpolation.EnvVar(varName)      => getEnvVar(fromScope, varName, envVars).map(Stage2PetaformAST.Str(_))
      case Interpolation.Config(__configPath) => getFromAst2(fromScope, Nil, __configPath.toList, ast)
    }

  private def interpolate(pair: Dependency.Pair, ast: Stage2PetaformAST, envVars: Map[String, String]): Either[ScopedError, Stage2PetaformAST] =
    pair.interp match {
      case Right(interpString) =>
        @tailrec
        def loop(
            pairs: List[(Interpolation, String)],
            rStack: List[String],
        ): Either[ScopedError, String] =
          pairs match {
            case (iH, sH) :: tail =>
              interpolate(pair.path, iH, ast, envVars) match {
                case Right(value) =>
                  value match {
                    case RawValue(value) => loop(tail, sH :: value :: rStack)
                    case Str(str)        => loop(tail, sH :: str :: rStack)
                    case Obj(_)          => ScopedError(pair.path, s"Can not interpolate Object into String").asLeft
                    case Arr(_)          => ScopedError(pair.path, s"Can not interpolate Array into String").asLeft
                    case Undef           => ScopedError(pair.path, s"Can not interpolate Undef into String").asLeft
                  }
                case Left(error) => error.asLeft
              }
            case Nil =>
              rStack.reverse.mkString.asRight
          }

        loop(interpString.pairs, interpString.prefix :: Nil).map(Stage2PetaformAST.Str(_))
      case Left(interp) => interpolate(pair.path, interp, ast, envVars)
    }

  private def replaceValue(
      fromScope: List[ASTScope],
      rLookupScope: List[ASTScope],
      lookupScope: List[ASTScope],
      calculatedValue: Stage2PetaformAST,
      ast: Stage2PetaformAST,
  ): Either[ScopedError, Stage2PetaformAST] =
    lookupScope match {
      case head :: tail =>
        val newRScope = head :: rLookupScope
        (head, ast) match {
          case (ASTScope.Key(key), Stage2PetaformAST.Obj(elems)) =>
            elems.indexWhere(_._1 == key) match {
              case -1 => ScopedError(fromScope, s"Missing key for interpolation: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
              case idx =>
                replaceValue(fromScope, newRScope, tail, calculatedValue, elems(idx)._2).map { replaced =>
                  Stage2PetaformAST.Obj(elems.updated(idx, (key, replaced)))
                }
            }
          case (ASTScope.Idx(idx), Stage2PetaformAST.Arr(elems)) =>
            if (idx >= elems.size) ScopedError(fromScope, s"Idx out of bounds: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            else
              replaceValue(fromScope, newRScope, tail, calculatedValue, elems(idx)).map { replaced =>
                Stage2PetaformAST.Arr(elems.updated(idx, replaced))
              }
          case (_: ASTScope.Key, _) =>
            ScopedError(newRScope.reverse, s"Expected object, but got ${ast.getClass.getSimpleName}").asLeft
          case (_: ASTScope.Idx, _) =>
            ScopedError(newRScope.reverse, s"Expected array, but got ${ast.getClass.getSimpleName}").asLeft
        }
      case Nil =>
        ast match {
          case Stage2PetaformAST.Undef => calculatedValue.asRight
          case _                       => ScopedError(fromScope, s"(this should never happen) Expected to replace Undef, but got ${ast.getClass.getSimpleName}").asLeft
        }
    }

  @tailrec
  private def interpolateAll(
      ordering: List[Dependency.Pair],
      ast: Stage2PetaformAST,
      envVars: Map[String, String],
  ): Either[ScopedError, Stage2PetaformAST] =
    ordering match {
      case head :: tail =>
        interpolate(head, ast, envVars).flatMap(replaceValue(head.path, Nil, head.path, _, ast)) match {
          case Right(ast)  => interpolateAll(tail, ast, envVars)
          case Left(error) => error.asLeft
        }
      case Nil =>
        ast.asRight
    }

  def fromStage1(
      ast: Stage1PetaformAST,
      envVars: Map[String, String],
  ): Either[ScopedError, Stage2PetaformAST] =
    for {
      ordering <- topologicalSort(getInterpolations(Nil, ast))
      init <- initialStage2(Nil, ast, envVars)
      result <- interpolateAll(ordering, init, envVars)
    } yield result

}
