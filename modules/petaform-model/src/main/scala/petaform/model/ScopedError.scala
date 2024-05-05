package petaform.model

final case class ScopedError(path: ScopePath, error: String) {
  override def toString: String =
    s"( _root_${path.pathInOrder.map(_.pathString).mkString} ) $error"
}
