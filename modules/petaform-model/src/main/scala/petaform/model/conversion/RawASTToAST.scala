package petaform.model.conversion

import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.*
import petaform.model.*
import petaform.model.ast.*
import scala.annotation.tailrec

object RawASTToAST {

  def apply(
      ast: RawPetaformAST,
      envVars: EnvVars,
      config: Option[PetaformAST],
  ): Either[ScopedError, PetaformAST] =
    for {
      ordering <- topologicalSort(getInterpolations(Nil, ast))
      init <- initialStage2(Nil, ast, envVars)
      result <- interpolateAll(ordering, init, envVars, config)
    } yield result

  def convertStringWithoutConfig(
      interpolation: Interpolation,
      rScope: List[ASTScope],
      envVars: EnvVars,
  ): Either[ScopedError, Option[String]] = {
    @tailrec
    def loop(
        queue: List[Interpolation.Source],
        rStack: List[String],
    ): Either[ScopedError, Option[String]] =
      queue match {
        case Interpolation.Source.Config(_) :: _ => None.asRight
        case Interpolation.Source.EnvVar(varName) :: tail =>
          getEnvVar(rScope, varName, envVars) match {
            case Right(varValue) => loop(tail, varValue :: rStack)
            case Left(error)     => error.asLeft
          }
        case Nil => rStack.reverse.mkString.some.asRight
      }

    loop(interpolation.sources.toList, Nil)
  }

  // TODO (KR) : I think this is missing application of functions
  def convertStringWithoutConfig(
      str: InterpolatedString,
      rScope: List[ASTScope],
      envVars: EnvVars,
  ): Either[ScopedError, Option[String]] = {
    @tailrec
    def loop(
        toInterp: List[(Interpolation, String)],
        rStack: List[String],
    ): Either[ScopedError, Option[String]] =
      toInterp match {
        case (interp, sH) :: tail =>
          convertStringWithoutConfig(interp, rScope, envVars) match {
            case Right(Some(value)) => loop(tail, sH :: value :: rStack)
            case Right(None)        => None.asRight
            case Left(error)        => error.asLeft
          }
        case Nil =>
          rStack.reverse.mkString.some.asRight
      }

    loop(str.pairs, str.prefix :: Nil)
  }

  // =====|  |=====

  private final case class Dependency(
      pair: Dependency.Pair,
      dependsOn: Set[List[ASTScope]],
  )
  private object Dependency {

    final case class Pair(
        path: List[ASTScope],
        interpolationType: InterpolationType,
    )

    sealed trait InterpolationType
    object InterpolationType {
      final case class FlatInterpolation(interpolation: Interpolation) extends InterpolationType
      final case class Str(str: InterpolatedString) extends InterpolationType
      final case class Lines(strs: List[InterpolatedString]) extends InterpolationType
    }

  }

  // =====|  |=====

  private object InterpolationHelpers {

    def interpolationToConfigPaths(interp: Interpolation): List[List[ASTScope]] =
      interp.sources.toList.collect { case Interpolation.Source.Config(paths) => paths.toList }

  }

  private def getInterpolations(scopePath: List[ASTScope], ast: RawPetaformAST): List[Dependency] =
    ast match {
      case RawPetaformAST.Raw(_) => Nil
      case RawPetaformAST.Str(str) =>
        val deps = str.pairs.map(_._1).flatMap(InterpolationHelpers.interpolationToConfigPaths).toSet
        if (deps.isEmpty) Nil
        else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.Str(str)), deps) :: Nil
      case RawPetaformAST.FlatInterpolation(interpolation) =>
        val deps = InterpolationHelpers.interpolationToConfigPaths(interpolation).toSet
        if (deps.isEmpty) Nil
        else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.FlatInterpolation(interpolation)), deps) :: Nil
      case RawPetaformAST.Undef => Nil
      case RawPetaformAST.Null  => Nil
      case RawPetaformAST.Empty => Nil
      case RawPetaformAST.Obj(elems) =>
        elems.flatMap {
          case (_, RawPetaformAST.Obj.Value.Required)             => Nil
          case (key, RawPetaformAST.Obj.Value.Provided(_, value)) => getInterpolations(scopePath :+ ASTScope.Key(key), value)
        }
      case RawPetaformAST.Arr(elems) =>
        elems.zipWithIndex.flatMap { case (elem, idx) =>
          getInterpolations(scopePath :+ ASTScope.Idx(idx), elem)
        }
      case RawPetaformAST.EofStr(lines) =>
        val deps = lines.flatMap(_.pairs.map(_._1)).flatMap(InterpolationHelpers.interpolationToConfigPaths).toSet
        if (deps.isEmpty) Nil
        else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.Lines(lines)), deps) :: Nil
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

  private def getEnvVar(rScope: List[ASTScope], varName: String, envVars: EnvVars): Either[ScopedError, String] =
    envVars.get(varName) match {
      case Some(varValue) => varValue.asRight
      case None           => ScopedError(rScope, s"Missing env var '$varName'").asLeft
    }

  private def initialStage2(rScope: List[ASTScope], ast: RawPetaformAST, envVars: EnvVars): Either[ScopedError, PetaformAST] =
    ast match {
      case RawPetaformAST.Raw(value) => PetaformAST.Raw(value).asRight
      case RawPetaformAST.Str(str) =>
        convertStringWithoutConfig(str, rScope, envVars).map {
          case Some(str) => PetaformAST.Str(str)
          case None      => PetaformAST.Undef
        }
      case RawPetaformAST.FlatInterpolation(interpolation) =>
        convertStringWithoutConfig(interpolation, rScope, envVars).map {
          case Some(value) => PetaformAST.Str(value)
          case None        => PetaformAST.Undef
        }
      case RawPetaformAST.Undef => PetaformAST.Undef.asRight
      case RawPetaformAST.Null  => PetaformAST.Null.asRight
      case RawPetaformAST.Empty => PetaformAST.Empty.asRight
      case RawPetaformAST.Obj(elems) =>
        elems
          .traverse {
            case (key, RawPetaformAST.Obj.Value.Required) =>
              ScopedError((ASTScope.Key(key) :: rScope).reverse, "Value was not provided for @required key").asLeft
            case (key, RawPetaformAST.Obj.Value.Provided(_, value)) =>
              initialStage2(ASTScope.Key(key) :: rScope, value, envVars).map((key, _))
          }
          .map(PetaformAST.Obj(_))
      case RawPetaformAST.Arr(elems) =>
        elems.zipWithIndex
          .traverse { case (value, idx) =>
            initialStage2(ASTScope.Idx(idx) :: rScope, value, envVars)
          }
          .map(PetaformAST.Arr(_))
      case RawPetaformAST.EofStr(lines) =>
        lines.traverse(convertStringWithoutConfig(_, rScope, envVars)).map {
          _.traverse(identity) match {
            case Some(lines) => PetaformAST.EofStr(lines)
            case None        => PetaformAST.Undef
          }
        }
    }

  @tailrec
  private def getFromAst2(fromScope: List[ASTScope], rLookupScope: List[ASTScope], lookupScope: List[ASTScope], ast: PetaformAST): Either[ScopedError, PetaformAST] =
    lookupScope match {
      case head :: tail =>
        val newRScope = head :: rLookupScope
        (head, ast) match {
          case (ASTScope.Key(key), PetaformAST.Obj(elems)) =>
            elems.find(_._1 == key) match {
              case Some((_, value)) => getFromAst2(fromScope, newRScope, tail, value)
              case None             => ScopedError(fromScope, s"Missing key for interpolation: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            }
          case (ASTScope.Idx(idx), PetaformAST.Arr(elems)) =>
            if (idx >= elems.size) ScopedError(fromScope, s"Idx out of bounds: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            else getFromAst2(fromScope, newRScope, tail, elems(idx))
          case (_: ASTScope.Key, _) =>
            ScopedError(newRScope.reverse, s"Expected object, but got ${ast.getClass.getSimpleName}").asLeft
          case (_: ASTScope.Idx, _) =>
            ScopedError(newRScope.reverse, s"Expected array, but got ${ast.getClass.getSimpleName}").asLeft
        }
      case Nil => ast.asRight
    }

  private def interpolateString(
      depPair: Dependency.Pair,
      interpString: InterpolatedString,
      ast: PetaformAST,
      envVars: EnvVars,
  ): Either[ScopedError, String] = {
    @tailrec
    def loop(
        pairs: List[(Interpolation, String)],
        rStack: List[String],
    ): Either[ScopedError, String] =
      pairs match {
        case (iH, sH) :: tail =>
          interpolate(depPair.path, iH, ast, envVars) match {
            case Right(value) =>
              value match {
                case PetaformAST.Raw(value)    => loop(tail, sH :: value :: rStack)
                case PetaformAST.Str(str)      => loop(tail, sH :: str :: rStack)
                case PetaformAST.EofStr(lines) => loop(tail, sH :: lines.mkString("\n") :: rStack)
                case PetaformAST.Obj(_)        => ScopedError(depPair.path, s"Can not interpolate Object into String").asLeft
                case PetaformAST.Arr(_)        => ScopedError(depPair.path, s"Can not interpolate Array into String").asLeft
                case PetaformAST.Undef         => ScopedError(depPair.path, s"Can not interpolate Undef into String").asLeft
                case PetaformAST.Null          => ScopedError(depPair.path, s"Can not interpolate Null into String").asLeft
                case PetaformAST.Empty         => ScopedError(depPair.path, s"Can not interpolate Empty into String").asLeft
              }
            case Left(error) => error.asLeft
          }
        case Nil =>
          rStack.reverse.mkString.asRight
      }

    loop(interpString.pairs, interpString.prefix :: Nil)
  }

  private def getStr(fromScope: List[ASTScope], ast: PetaformAST): Either[ScopedError, PetaformAST.StringLike] =
    ast match {
      case ast: PetaformAST.StringLike => ast.asRight
      case _                           => ScopedError(fromScope, s"Expected StringLike, but got ${ast.getClass.getSimpleName}").asLeft
    }

  private implicit class StringLikeOps(stringLike: PetaformAST.StringLike) {

    def mapStr(f: String => String): PetaformAST.StringLike =
      stringLike match {
        case PetaformAST.Raw(value)    => PetaformAST.Raw(f(value))
        case PetaformAST.Str(str)      => PetaformAST.Str(f(str))
        case PetaformAST.EofStr(lines) => PetaformAST.EofStr(lines.map(f))
      }

  }

  @tailrec
  private def applyFunctions(fromScope: List[ASTScope], ast: PetaformAST, functions: List[String]): Either[ScopedError, PetaformAST] =
    functions match {
      case head :: tail =>
        (head match {
          // TODO (KR) : do we need different handling of `EofStr`?
          case "format" => PetaformAST.Str(FormatAST(ast)).asRight
          case "upper"  => getStr(fromScope, ast).map(_.mapStr(_.toUpperCase))
          case "lower"  => getStr(fromScope, ast).map(_.mapStr(_.toLowerCase))
          case "unesc"  => getStr(fromScope, ast).map(_.mapStr(_.unesc))
          case "json"   => ASTToJson(ast).map(PetaformAST.Raw(_))
          case _        => ScopedError(fromScope, s"Invalid function '$head'").asLeft
        }) match {
          case Right(newAst) => applyFunctions(fromScope, newAst, tail)
          case Left(error)   => error.asLeft
        }
      case Nil => ast.asRight
    }

  private def add(fromScope: List[ASTScope], left: PetaformAST, right: PetaformAST): Either[ScopedError, PetaformAST] =
    (left, right) match {
      // empty
      case (PetaformAST.Empty, PetaformAST.Empty) => PetaformAST.Empty.asRight
      // obj
      case (PetaformAST.Empty, ast: PetaformAST.Obj)       => ast.asRight
      case (ast: PetaformAST.Obj, PetaformAST.Empty)       => ast.asRight
      case (left: PetaformAST.Obj, right: PetaformAST.Obj) => PetaformAST.Obj(left.elems ::: right.elems).asRight // TODO (KR) : safe?
      // arr
      case (PetaformAST.Empty, ast: PetaformAST.Arr)       => ast.asRight
      case (ast: PetaformAST.Arr, PetaformAST.Empty)       => ast.asRight
      case (left: PetaformAST.Arr, right: PetaformAST.Arr) => PetaformAST.Arr(left.elems ::: right.elems).asRight
      // string
      // TODO (KR) :
      // error
      case _ => ScopedError(fromScope, s"Can not add ${left.getClass.getSimpleName} + ${right.getClass.getSimpleName}").asLeft
    }

  private def interpolate(fromScope: List[ASTScope], interp: Interpolation, ast: PetaformAST, envVars: EnvVars): Either[ScopedError, PetaformAST] =
    for {
      raws <- interp.sources.traverse {
        case Interpolation.Source.EnvVar(varName)      => getEnvVar(fromScope, varName, envVars).map(PetaformAST.Str(_))
        case Interpolation.Source.Config(__configPath) => getFromAst2(fromScope, Nil, __configPath.toList, ast)
      }
      afterAdds <- raws.tail.foldLeft(raws.head.asRight[ScopedError]) { case (acc, ast) => acc.flatMap(add(fromScope, _, ast)) }
      afterFunctions <- applyFunctions(fromScope, afterAdds, interp.functions)
    } yield afterFunctions

  private def interpolate(pair: Dependency.Pair, ast: PetaformAST, envVars: EnvVars): Either[ScopedError, PetaformAST] =
    pair.interpolationType match {
      case Dependency.InterpolationType.Str(interpString) =>
        interpolateString(pair, interpString, ast, envVars).map(PetaformAST.Str(_))
      case Dependency.InterpolationType.FlatInterpolation(interp) =>
        interpolate(pair.path, interp, ast, envVars)
      case Dependency.InterpolationType.Lines(lines) =>
        lines.traverse(interpolateString(pair, _, ast, envVars)).map(PetaformAST.EofStr(_))
    }

  private def replaceValue(
      fromScope: List[ASTScope],
      rLookupScope: List[ASTScope],
      lookupScope: List[ASTScope],
      calculatedValue: PetaformAST,
      ast: PetaformAST,
  ): Either[ScopedError, PetaformAST] =
    lookupScope match {
      case head :: tail =>
        val newRScope = head :: rLookupScope
        (head, ast) match {
          case (ASTScope.Key(key), PetaformAST.Obj(elems)) =>
            elems.indexWhere(_._1 == key) match {
              case -1 => ScopedError(fromScope, s"Missing key for interpolation: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
              case idx =>
                replaceValue(fromScope, newRScope, tail, calculatedValue, elems(idx)._2).map { replaced =>
                  PetaformAST.Obj(elems.updated(idx, (key, replaced)))
                }
            }
          case (ASTScope.Idx(idx), PetaformAST.Arr(elems)) =>
            if (idx >= elems.size) ScopedError(fromScope, s"Idx out of bounds: ${ASTScope.format("CFG", newRScope.reverse)}").asLeft
            else
              replaceValue(fromScope, newRScope, tail, calculatedValue, elems(idx)).map { replaced =>
                PetaformAST.Arr(elems.updated(idx, replaced))
              }
          case (_: ASTScope.Key, _) =>
            ScopedError(newRScope.reverse, s"Expected object, but got ${ast.getClass.getSimpleName}").asLeft
          case (_: ASTScope.Idx, _) =>
            ScopedError(newRScope.reverse, s"Expected array, but got ${ast.getClass.getSimpleName}").asLeft
        }
      case Nil =>
        ast match {
          case PetaformAST.Undef => calculatedValue.asRight
          case _                 => ScopedError(fromScope, s"(this should never happen) Expected to replace Undef, but got ${ast.getClass.getSimpleName}").asLeft
        }
    }

  @tailrec
  private def interpolateAll(
      ordering: List[Dependency.Pair],
      ast: PetaformAST,
      envVars: EnvVars,
      config: Option[PetaformAST],
  ): Either[ScopedError, PetaformAST] =
    ordering match {
      case head :: tail =>
        interpolate(head, config.getOrElse(ast), envVars).flatMap(replaceValue(head.path, Nil, head.path, _, ast)) match {
          case Right(ast)  => interpolateAll(tail, ast, envVars, config)
          case Left(error) => error.asLeft
        }
      case Nil =>
        ast.asRight
    }

}
