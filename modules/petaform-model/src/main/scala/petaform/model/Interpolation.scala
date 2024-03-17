package petaform.model

import cats.data.NonEmptyList
import scala.language.dynamics

final case class Interpolation(
    source: Interpolation.Source,
    functions: List[String],
) {

  def show: String = s"$${${source.show}${functions.map(f => s" | $f").mkString}}"

}
object Interpolation {

  sealed trait Source {

    final def show: String =
      this match {
        case Interpolation.Source.EnvVar(varName)      => s"ENV.$varName"
        case Interpolation.Source.Config(__configPath) => s"CFG${__configPath.toList.map(_.pathString).mkString}"
      }

  }
  object Source {
    final case class EnvVar(varName: String) extends Interpolation.Source
    final case class Config(__configPath: NonEmptyList[ASTScope]) extends Interpolation.Source
  }

}
