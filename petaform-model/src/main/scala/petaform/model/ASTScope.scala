package petaform.model

sealed trait ASTScope {

  final def pathString: String =
    this match {
      case ASTScope.Key(key) => s".$key"
      case ASTScope.Idx(idx) => s"[$idx]"
    }

}
object ASTScope {

  final case class Key(key: String) extends ASTScope
  final case class Idx(idx: Int) extends ASTScope

  def format(prefix: String, path: List[ASTScope]): String =
    s"$prefix${path.map(_.pathString).mkString}"

}
