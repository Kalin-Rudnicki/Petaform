package petaform.model

import cats.syntax.option.*

final class ScopePath private (reversed: Boolean, path: List[ASTScope]) {

  def pathInOrder: List[ASTScope] = if (reversed) path.reverse else path
  def pathReversed: List[ASTScope] = if (reversed) path else path.reverse

  def +:(scope: ASTScope): ScopePath = new ScopePath(false, scope :: pathInOrder)
  def :+(scope: ASTScope): ScopePath = new ScopePath(true, scope :: pathReversed)

  override def equals(obj: Any): Boolean = obj.asInstanceOf[Matchable] match
    case that: ScopePath => this.pathInOrder == that.pathInOrder
    case _               => false

  override def hashCode(): Int = pathInOrder.hashCode()

}
object ScopePath {

  val empty = new ScopePath(true, Nil)
  def inOrder(path: List[ASTScope]) = new ScopePath(false, path)
  def reversed(path: List[ASTScope]) = new ScopePath(true, path)

  def unapply(path: ScopePath): Option[(ASTScope, ScopePath)] =
    path.pathInOrder match
      case head :: tail => (head, ScopePath.inOrder(tail)).some
      case Nil          => None

}
