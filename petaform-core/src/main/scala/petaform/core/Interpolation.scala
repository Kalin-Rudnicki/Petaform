package petaform.core

import cats.data.NonEmptyList
import scala.language.dynamics

sealed trait Interpolation {

  final def show: String =
    this match {
      case Interpolation.EnvVar(varName)      => s"$${ENV.$varName}"
      case Interpolation.Config(__configPath) => s"$${CFG${__configPath.toList.map(_.pathString).mkString}}"
    }

}
object Interpolation {

  final case class EnvVar(varName: String) extends Interpolation
  final case class Config(__configPath: NonEmptyList[ASTScope]) extends Interpolation with Dynamic {
    def selectDynamic(fieldName: String): Config = Config(__configPath :+ ASTScope.Key(fieldName))
    def apply(idx: Int): Config = Config(__configPath :+ ASTScope.Idx(idx))
  }

}

// =====| Builders |=====

object env extends Dynamic {
  def selectDynamic(fieldName: String): Interpolation.EnvVar = Interpolation.EnvVar(fieldName)
}

object cfg extends Dynamic {
  def selectDynamic(fieldName: String): Interpolation.Config = Interpolation.Config(NonEmptyList.one(ASTScope.Key(fieldName)))
}
