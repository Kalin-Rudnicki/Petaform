package petaform.model.typeclass

import harness.core.*
import petaform.model.ast.*
import shapeless3.deriving.*

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

trait ASTEncoderLowPriority2 {

  given astProductEncoderGen[A](using inst: => K0.ProductInstances[ASTEncoder, A], labels: => Labelling[A]): ASTEncoder[A] =
    a =>
      PetaformAST.Obj(
        labels.elemLabels.toList
          .zip(
            inst.foldRight(a)(List.empty[Option[PetaformAST]]) {
              [t] => (enc: ASTEncoder[t], tt: t, list: List[Option[PetaformAST]]) => Option.when(!enc.omitKey(tt))(enc.encode(tt)) :: list
            },
          )
          .collect { case (key, Some(ast)) => key -> ast },
      )

  given astCoproductEncoderGen[A](using inst: => K0.CoproductInstances[ASTEncoder, A], labels: => Labelling[A]): ASTEncoder[A] = { a =>
    val idx: Int = inst.mirror.ordinal(a.asInstanceOf)

    PetaformAST.Obj(
      (
        labels.elemLabels(idx),
        inst.inject(idx) { [t] => (enc: ASTEncoder[t]) => enc.encode(a.asInstanceOf) },
      ) :: Nil,
    )
  }

  inline def derived[A](using gen: K0.Generic[A]): ASTEncoder[A] =
    gen.derive(astProductEncoderGen[A], astCoproductEncoderGen[A])

}
