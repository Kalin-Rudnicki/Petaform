package petaform.model

import cats.data.NonEmptyList

sealed trait Interpolation {

  private final def showInternal: String = this match {
    case Interpolation.Source.EnvVar(varName)      => s"ENV.$varName"
    case Interpolation.Source.Config(__configPath) => s"CFG${__configPath.toList.map(_.pathString).mkString}"
    case Interpolation.Raw(value)                  => value
    case Interpolation.Str(str)                    => str.show
    case Interpolation.Concat(left, right)         => s"${left.showInternal} ++ ${right.showInternal}"
    case Interpolation.GenericFunction(key, args)  => s"$key(${args.map(_.showInternal).mkString(", ")})"
    case Interpolation.Scoped(child)               => s"(${child.showInternal})"
  }

  final def show: String = s"$${$showInternal}"

}
object Interpolation {

  sealed trait Source extends Interpolation
  object Source {
    final case class EnvVar(varName: String) extends Interpolation.Source
    final case class Config(__configPath: NonEmptyList[ASTScope]) extends Interpolation.Source
  }

  final case class Raw(value: String) extends Interpolation

  final case class Str(str: InterpolatedString) extends Interpolation

  final case class Concat(left: Interpolation, right: Interpolation) extends Interpolation

  final case class GenericFunction(key: String, args: List[Interpolation]) extends Interpolation

  final case class Scoped(child: Interpolation) extends Interpolation

}
