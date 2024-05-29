package petaform.model.typeclass

import harness.core.*
import harness.deriving.*
import petaform.model.ast.*

trait ASTEncoder[A] { self =>

  def encode(a: A): PetaformAST

  def omitKey(a: A): Boolean = false

  final def contramap[B](f: B => A): ASTEncoder[B] =
    b => self.encode(f(b))

}
object ASTEncoder extends ASTEncoderLowPriority {

  def apply[A](implicit enc: ASTEncoder[A]): ASTEncoder[A] = enc

  implicit val ast: ASTEncoder[PetaformAST] = identity(_)
  implicit val objAst: ASTEncoder[PetaformAST.Obj] = identity(_)

  implicit def map[K: StringEncoder, V: ASTEncoder]: ASTEncoder[Map[K, V]] =
    m => PetaformAST.Obj(m.toList.map { case (k, v) => StringEncoder[K].encode(k) -> ASTEncoder[V].encode(v) })

  implicit def list[V: ASTEncoder]: ASTEncoder[List[V]] =
    l => PetaformAST.Arr(l.map(ASTEncoder[V].encode))

  implicit def option[A: ASTEncoder]: ASTEncoder[Option[A]] =
    new ASTEncoder[Option[A]] {
      override def encode(a: Option[A]): PetaformAST =
        a match {
          case Some(a) => ASTEncoder[A].encode(a)
          case None    => PetaformAST.Null
        }
      override def omitKey(a: Option[A]): Boolean = a.isEmpty
    }

  // TODO (KR) : number, boolean, ect -> raw
  implicit def fromStringEncoder[A: StringEncoder]: ASTEncoder[A] =
    a => PetaformAST.Str(StringEncoder[A].encode(a))

  // TODO (KR) : instances

}

trait ASTEncoderLowPriority extends ASTEncoderLowPriority2 {

  implicit def fromCodec[A: ASTCodec]: ASTEncoder[A] = ASTCodec[A].encoder

}

trait ASTEncoderLowPriority2 extends K0.Derivable[ASTEncoder] {

  override implicit inline def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[ASTEncoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.ProductInstances.of[A, ASTEncoder]

    Derived { a =>
      PetaformAST.Obj(instances.withInstance(a).map.withLabels(labels) { [t] => (l: String, i: ASTEncoder[t], t: t) => Option.when(!i.omitKey(t)) { l -> i.encode(t) } }.flatten)
    }
  }

  override implicit inline def genSum[A](implicit m: K0.SumGeneric[A]): Derived[ASTEncoder[A]] = {
    val labels = Labelling.of[A]
    val instances = K0.SumInstances.of[A, ASTEncoder]

    Derived { a =>
      instances.withInstance(a).use.withLabels(labels) { [t <: A] => (l: String, i: ASTEncoder[t], t: t) => PetaformAST.Obj(l -> i.encode(t)) }
    }
  }

}

implicit class PetaformEncoderOps[A](a: A) {
  def toPetaformAST(implicit enc: ASTEncoder[A]): PetaformAST = enc.encode(a)
}
