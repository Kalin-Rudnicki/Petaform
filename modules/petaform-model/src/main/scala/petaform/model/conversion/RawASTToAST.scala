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
      ordering <- TopologicalSort(ast)
      init <- Stage1(ScopePath.empty, ast, envVars)
      result <- Stage2(ordering, init, envVars, config)
    } yield result

  // =====|  |=====

  private final case class Dependency(
      pair: Dependency.Pair,
      dependsOn: Set[ScopePath],
  )
  private object Dependency {

    final case class Pair(
        path: ScopePath,
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

  import Shared.*

  private object Shared {

    def getEnvVar(scope: ScopePath, varName: String, envVars: EnvVars): Either[ScopedError, String] =
      envVars.get(varName) match {
        case Some(varValue) => varValue.asRight
        case None           => ScopedError(scope, s"Missing env var '$varName'").asLeft
      }

    def concat(scope: ScopePath, left: PetaformAST, right: PetaformAST): Either[ScopedError, PetaformAST] =
      (left, right) match {
        // empty
        case (PetaformAST.Empty, PetaformAST.Empty) => PetaformAST.Empty.asRight
        // obj
        case (PetaformAST.Empty, ast: PetaformAST.Obj)       => ast.asRight
        case (ast: PetaformAST.Obj, PetaformAST.Empty)       => ast.asRight
        case (left: PetaformAST.Obj, right: PetaformAST.Obj) => PetaformAST.Obj(left.elems ::: right.elems).asRight // TODO (KR) : merge
        // arr
        case (PetaformAST.Empty, ast: PetaformAST.Arr)       => ast.asRight
        case (ast: PetaformAST.Arr, PetaformAST.Empty)       => ast.asRight
        case (left: PetaformAST.Arr, right: PetaformAST.Arr) => PetaformAST.Arr(left.elems ::: right.elems).asRight
        // string
        case (PetaformAST.EofStr(left), PetaformAST.EofStr(right))         => PetaformAST.EofStr(left ::: right).asRight
        case (PetaformAST.StringLike(left), PetaformAST.StringLike(right)) => PetaformAST.Str(left + right).asRight
        // error
        case _ => ScopedError(scope, s"Can not add ${left.getClass.getSimpleName} + ${right.getClass.getSimpleName}").asLeft
      }

    private val functions: Map[String, Function[?]] =
      List[Function[?]](
        Function.make[PetaformAST]("format") { ast => PetaformAST.Str(ast.format).asRight },
        Function.make[PetaformAST.StringLike]("upper") { _.mapString(_.toUpperCase).asRight },
        Function.make[PetaformAST.StringLike]("lower") { _.mapString(_.toLowerCase).asRight },
        Function.make[PetaformAST.StringLike]("unesc") { _.mapString(_.unesc).asRight }, // TODO (KR) : this might be a little weird if used with EofString?
        Function.make[PetaformAST]("json") { ASTToJson(_).map(PetaformAST.Raw(_)) },
      ).map(f => f.name -> f).toMap

    def applyFunction(scope: ScopePath, name: String, args: List[PetaformAST]): Either[ScopedError, PetaformAST] =
      functions.get(name) match {
        case Some(func) => func(scope, args)
        case None       => ScopedError(scope, s"Unknown function: $name").asLeft
      }

    def getStr(scope: ScopePath, ast: PetaformAST): Either[ScopedError, PetaformAST.StringLike] =
      ast match {
        case ast: PetaformAST.StringLike => ast.asRight
        case _                           => ScopedError(scope, s"Expected StringLike, but got ${ast.getClass.getSimpleName}").asLeft
      }

  }

  private object TopologicalSort {

    def apply(ast: RawPetaformAST): Either[ScopedError, List[Dependency.Pair]] =
      sort(getDependencies(ScopePath.empty, ast))

    private def configPathsFromInterpolation(interp: Interpolation): List[ScopePath] =
      interp match {
        case Interpolation.Source.Config(path)      => ScopePath.inOrder(path.toList) :: Nil
        case Interpolation.Concat(left, right)      => configPathsFromInterpolation(left) ::: configPathsFromInterpolation(right)
        case Interpolation.GenericFunction(_, args) => args.flatMap(configPathsFromInterpolation)
        case Interpolation.Scoped(child)            => configPathsFromInterpolation(child)
        case Interpolation.Source.EnvVar(_)         => Nil
        case Interpolation.Raw(value)               => Nil
        case Interpolation.Str(str)                 => Nil
      }

    private def getDependencies(scopePath: ScopePath, ast: RawPetaformAST): List[Dependency] =
      ast match {
        case RawPetaformAST.Raw(_) => Nil
        case RawPetaformAST.Str(str) =>
          val deps = str.pairs.map(_._1).flatMap(configPathsFromInterpolation).toSet
          if (deps.isEmpty) Nil
          else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.Str(str)), deps) :: Nil
        case RawPetaformAST.FlatInterpolation(interpolation) =>
          val deps = configPathsFromInterpolation(interpolation).toSet
          if (deps.isEmpty) Nil
          else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.FlatInterpolation(interpolation)), deps) :: Nil
        case RawPetaformAST.Undef => Nil
        case RawPetaformAST.Null  => Nil
        case RawPetaformAST.Empty => Nil
        case RawPetaformAST.Obj(elems) =>
          elems.flatMap {
            case (_, RawPetaformAST.Obj.Value.Required)             => Nil
            case (key, RawPetaformAST.Obj.Value.Provided(_, value)) => getDependencies(scopePath :+ ASTScope.Key(key), value)
          }
        case RawPetaformAST.Arr(elems) =>
          elems.zipWithIndex.flatMap { case (elem, idx) =>
            getDependencies(scopePath :+ ASTScope.Idx(idx), elem)
          }
        case RawPetaformAST.EofStr(lines) =>
          val deps = lines.flatMap(_.pairs.map(_._1)).flatMap(configPathsFromInterpolation).toSet
          if (deps.isEmpty) Nil
          else Dependency(Dependency.Pair(scopePath, Dependency.InterpolationType.Lines(lines)), deps) :: Nil
      }

    private def sort(dependencies: List[Dependency]): Either[ScopedError, List[Dependency.Pair]] = {
      @tailrec
      def loop(
          toSort: Map[ScopePath, Set[ScopePath]],
          rSorted: List[ScopePath],
      ): Either[ScopedError, List[Dependency.Pair]] =
        if (toSort.isEmpty) {
          val depMap = dependencies.map(dep => dep.pair.path -> dep).toMap
          rSorted.reverse.map(depMap(_).pair).asRight
        } else {
          val keysWithoutValues: Set[ScopePath] = toSort.collect { case (k, v) if v.isEmpty => k }.toSet
          val valuesWithoutKey: Set[ScopePath] = toSort.values.flatten.toSet.filterNot(toSort.contains)
          val valuesToConsume: Set[ScopePath] = keysWithoutValues | valuesWithoutKey

          if (valuesToConsume.isEmpty) {
            val unusedString =
              toSort.toList.map { case (k, v) =>
                s"\n  - ${ASTScope.format("CFG", k)} -> ${v.toList.map(ASTScope.format("CFG", _)).mkString(" / ")}"
              }.mkString

            ScopedError(ScopePath.empty, s"Cyclic interpolation dependencies:$unusedString").asLeft
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

  }

  object Stage1 {

    private[RawASTToAST] def apply(scope: ScopePath, ast: RawPetaformAST, envVars: EnvVars): Either[ScopedError, PetaformAST] =
      ast match {
        case RawPetaformAST.Raw(value) => PetaformAST.Raw(value).asRight
        case RawPetaformAST.Str(str) =>
          attemptConvertInterpolatedString(str, scope, envVars).map {
            case Some(value) => PetaformAST.Str(value)
            case None        => PetaformAST.Undef
          }
        case RawPetaformAST.FlatInterpolation(interpolation) =>
          attemptConvertInterpolation(interpolation, scope, envVars).map(_.getOrElse(PetaformAST.Undef))
        case RawPetaformAST.Undef => PetaformAST.Undef.asRight
        case RawPetaformAST.Null  => PetaformAST.Null.asRight
        case RawPetaformAST.Empty => PetaformAST.Empty.asRight
        case RawPetaformAST.Obj(elems) =>
          elems
            .traverse {
              case (key, RawPetaformAST.Obj.Value.Required) =>
                ScopedError(scope :+ ASTScope.Key(key), "Value was not provided for @required key").asLeft
              case (key, RawPetaformAST.Obj.Value.Provided(_, value)) =>
                Stage1(scope :+ ASTScope.Key(key), value, envVars).map((key, _))
            }
            .map(PetaformAST.Obj(_))
        case RawPetaformAST.Arr(elems) =>
          elems.zipWithIndex
            .traverse { case (value, idx) =>
              Stage1(scope :+ ASTScope.Idx(idx), value, envVars)
            }
            .map(PetaformAST.Arr(_))
        case RawPetaformAST.EofStr(lines) =>
          lines.traverse(attemptConvertInterpolatedString(_, scope, envVars)).map {
            _.sequence match {
              case Some(lines) => PetaformAST.EofStr(lines)
              case None        => PetaformAST.Undef
            }
          }
      }

    def attemptConvertInterpolatedString(
        str: InterpolatedString,
        scope: ScopePath,
        envVars: EnvVars,
    ): Either[ScopedError, Option[String]] =
      str.pairs
        .traverse { case (interp, suffix) => attemptConvertInterpolation(interp, scope, envVars).map((_, suffix)) }
        .map { _.traverse { case (a, b) => a.map((_, b)) } }
        .flatMap { // TODO (KR) : some other functions to do this better?
          case Some(pairs) =>
            pairs
              .traverse { case (ast, suffix) => getStr(scope, ast).map(s => List(s.stringValue, suffix)) }
              .map { lists => (str.prefix :: lists.flatten).mkString.some }
          case None => None.asRight
        }

    def attemptConvertInterpolation(
        interpolation: Interpolation,
        scope: ScopePath,
        envVars: EnvVars,
    ): Either[ScopedError, Option[PetaformAST]] =
      interpolation match {
        case Interpolation.Source.EnvVar(varName) => getEnvVar(scope, varName, envVars).map(PetaformAST.Str(_).some)
        case Interpolation.Source.Config(_)       => None.asRight
        case Interpolation.Raw(value)             => PetaformAST.Raw(value).some.asRight
        case Interpolation.Str(str)               => attemptConvertInterpolatedString(str, scope, envVars).map(_.map(PetaformAST.Str(_)))
        case Interpolation.Scoped(child)          => attemptConvertInterpolation(child, scope, envVars)
        case Interpolation.Concat(left, right) =>
          for {
            leftConverted <- attemptConvertInterpolation(left, scope, envVars)
            rightConverted <- attemptConvertInterpolation(right, scope, envVars)
            both = (leftConverted, rightConverted) match {
              case (Some(left), Some(right)) => (left, right).some
              case _                         => None
            }
            res <- both.traverse(concat(scope, _, _))
          } yield res
        case Interpolation.GenericFunction(key, args) =>
          args
            .traverse { attemptConvertInterpolation(_, scope, envVars) }
            .map { _.sequence }
            .flatMap {
              case Some(args) => applyFunction(scope, key, args).map(_.some)
              case None       => None.asRight
            }
      }

  }

  private object Stage2 {

    @tailrec
    def apply(
        ordering: List[Dependency.Pair],
        ast: PetaformAST,
        envVars: EnvVars,
        config: Option[PetaformAST],
    ): Either[ScopedError, PetaformAST] =
      ordering match {
        case head :: tail =>
          resolveDependency(head, config.getOrElse(ast), envVars).flatMap(replaceValue(head.path, ScopePath.empty, head.path, _, ast)) match {
            case Right(ast)  => Stage2(tail, ast, envVars, config)
            case Left(error) => error.asLeft
          }
        case Nil =>
          ast.asRight
      }

    @tailrec
    private def getFromAst(scope: ScopePath, seenScope: ScopePath, lookupScope: ScopePath, referenceAST: PetaformAST): Either[ScopedError, PetaformAST] =
      lookupScope match {
        case ScopePath(head, tail) =>
          val newSeenScope = seenScope :+ head
          (head, referenceAST) match {
            case (ASTScope.Key(key), PetaformAST.Obj(elems)) =>
              elems.find(_._1 == key) match {
                case Some((_, value)) => getFromAst(scope, newSeenScope, tail, value)
                case None             => ScopedError(scope, s"Missing key for interpolation: ${ASTScope.format("CFG", newSeenScope)}").asLeft
              }
            case (ASTScope.Idx(idx), PetaformAST.Arr(elems)) =>
              if (idx >= elems.size) ScopedError(scope, s"Idx out of bounds: ${ASTScope.format("CFG", newSeenScope)}").asLeft
              else getFromAst(scope, newSeenScope, tail, elems(idx))
            case (_: ASTScope.Key, _) =>
              ScopedError(newSeenScope, s"Expected object, but got ${referenceAST.getClass.getSimpleName}").asLeft
            case (_: ASTScope.Idx, _) =>
              ScopedError(newSeenScope, s"Expected array, but got ${referenceAST.getClass.getSimpleName}").asLeft
          }
        case _ => referenceAST.asRight
      }

    private def convertInterpolatedString(
        str: InterpolatedString,
        scope: ScopePath,
        envVars: EnvVars,
        referenceAST: PetaformAST,
    ): Either[ScopedError, String] =
      str.pairs
        .traverse { case (interp, suffix) => convertInterpolation(interp, scope, envVars, referenceAST).map((_, suffix)) }
        .flatMap {
          _.traverse { case (ast, suffix) => getStr(scope, ast).map(s => List(s.stringValue, suffix)) }
            .map { lists => (str.prefix :: lists.flatten).mkString }
        }

    private def replaceValue(
        scope: ScopePath,
        seenScope: ScopePath,
        lookupScope: ScopePath,
        calculatedValue: PetaformAST,
        referenceAST: PetaformAST,
    ): Either[ScopedError, PetaformAST] =
      lookupScope match {
        case ScopePath(head, tail) =>
          val newSeenScope = seenScope :+ head
          (head, referenceAST) match {
            case (ASTScope.Key(key), PetaformAST.Obj(elems)) =>
              elems.indexWhere(_._1 == key) match {
                case -1 => ScopedError(scope, s"Missing key for interpolation: ${ASTScope.format("CFG", newSeenScope)}").asLeft
                case idx =>
                  replaceValue(scope, newSeenScope, tail, calculatedValue, elems(idx)._2).map { replaced =>
                    PetaformAST.Obj(elems.updated(idx, (key, replaced)))
                  }
              }
            case (ASTScope.Idx(idx), PetaformAST.Arr(elems)) =>
              if (idx >= elems.size) ScopedError(scope, s"Idx out of bounds: ${ASTScope.format("CFG", newSeenScope)}").asLeft
              else
                replaceValue(scope, newSeenScope, tail, calculatedValue, elems(idx)).map { replaced =>
                  PetaformAST.Arr(elems.updated(idx, replaced))
                }
            case (_: ASTScope.Key, _) =>
              ScopedError(newSeenScope, s"Expected object, but got ${referenceAST.getClass.getSimpleName}").asLeft
            case (_: ASTScope.Idx, _) =>
              ScopedError(newSeenScope, s"Expected array, but got ${referenceAST.getClass.getSimpleName}").asLeft
          }
        case _ =>
          referenceAST match {
            case PetaformAST.Undef => calculatedValue.asRight
            case _                 => ScopedError(scope, s"(this should never happen) Expected to replace Undef, but got ${referenceAST.getClass.getSimpleName}").asLeft
          }
      }

    private def convertInterpolation(
        interpolation: Interpolation,
        scope: ScopePath,
        envVars: EnvVars,
        referenceAST: PetaformAST,
    ): Either[ScopedError, PetaformAST] =
      interpolation match {
        case Interpolation.Source.EnvVar(varName) => getEnvVar(scope, varName, envVars).map(PetaformAST.Str(_))
        case Interpolation.Source.Config(cfgPath) => getFromAst(scope, ScopePath.empty, ScopePath.inOrder(cfgPath.toList), referenceAST)
        case Interpolation.Raw(value)             => PetaformAST.Raw(value).asRight
        case Interpolation.Str(str)               => convertInterpolatedString(str, scope, envVars, referenceAST).map(PetaformAST.Str(_))
        case Interpolation.Scoped(child)          => convertInterpolation(child, scope, envVars, referenceAST)
        case Interpolation.Concat(left, right) =>
          for {
            leftConverted <- convertInterpolation(left, scope, envVars, referenceAST)
            rightConverted <- convertInterpolation(left, scope, envVars, referenceAST)
            res <- concat(scope, leftConverted, rightConverted)
          } yield res
        case Interpolation.GenericFunction(key, args) =>
          args
            .traverse { convertInterpolation(_, scope, envVars, referenceAST) }
            .flatMap { applyFunction(scope, key, _) }
      }

    private def resolveDependency(pair: Dependency.Pair, referenceAST: PetaformAST, envVars: EnvVars): Either[ScopedError, PetaformAST] =
      pair.interpolationType match {
        case Dependency.InterpolationType.Str(str) =>
          convertInterpolatedString(str, pair.path, envVars, referenceAST).map(PetaformAST.Str(_))
        case Dependency.InterpolationType.FlatInterpolation(interpolation) =>
          convertInterpolation(interpolation, pair.path, envVars, referenceAST)
        case Dependency.InterpolationType.Lines(lines) =>
          lines.traverse(convertInterpolatedString(_, pair.path, envVars, referenceAST)).map(PetaformAST.EofStr(_))
      }

  }

}
