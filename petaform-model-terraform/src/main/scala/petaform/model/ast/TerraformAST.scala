package petaform.model.ast

sealed trait TerraformAST
object TerraformAST {

  final case class BlockSet(key: String, elems: List[TerraformAST]) extends TerraformAST
  final case class KeyValue(key: String, value: Value) extends TerraformAST

  sealed trait Value
  object Value {
    final case class Raw(value: String) extends Value
    final case class Str(str: String) extends Value
    final case class Arr(elems: List[Value]) extends Value
    final case class Map(elems: List[(String, Value)]) extends Value
  }

}
