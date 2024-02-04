package petaform.core

final case class ScopedError(path: List[ASTScope], error: String) {
  override def toString: String =
    s"( _root_${path.map(_.pathString).mkString} ) $error"
}
