package petaform.model.conversion

import cats.syntax.either.*
import harness.zio.TypeOps
import petaform.model.*
import petaform.model.ast.PetaformAST
import petaform.model.typeclass.ASTDecoder
import zio.Tag

final case class Function[A](name: String, decoder: Function.Decoder[A], f: A => Either[ScopedError, PetaformAST]) {

  def apply(scope: ScopePath, args: List[PetaformAST]): Either[ScopedError, PetaformAST] =
    decoder.decode(name, scope, args).flatMap(f)

}
object Function {

  def make[A](name: String)(f: A => Either[ScopedError, PetaformAST])(implicit decoder: Decoder[A]): Function[A] =
    Function(name, decoder, f)

  // =====|  |=====

  trait SingleDecoder[A] {
    val typeName: String
    def decode(scope: ScopePath, arg: PetaformAST): Either[ScopedError, A]
  }
  object SingleDecoder {

    implicit def make[A](implicit tag: Tag[A], decoder: ASTDecoder[A]): SingleDecoder[A] =
      new SingleDecoder[A] {
        override val typeName: String = tag.typeName.prefixObject
        override def decode(scope: ScopePath, arg: PetaformAST): Either[ScopedError, A] = decoder.decode(arg)
      }

  }

  trait Decoder[A] {
    def decode(functionName: String, scope: ScopePath, args: List[PetaformAST]): Either[ScopedError, A]
  }
  object Decoder {

    private def make[A](decoders: SingleDecoder[?]*)(pf: (String, ScopePath) => PartialFunction[List[PetaformAST], Either[ScopedError, A]]): Decoder[A] =
      new Decoder[A] {
        override def decode(functionName: String, scope: ScopePath, args: List[PetaformAST]): Either[ScopedError, A] =
          pf(functionName, scope)
            .lift(args)
            .toRight { ScopedError(scope, s"Function $functionName expects ${decoders.length} arguments (${decoders.map(_.typeName).mkString(", ")}), but got ${args.length}") }
            .flatten
      }

    private def decode[A](scope: ScopePath, functionName: String, argIdx: Int, arg: PetaformAST, decoder: SingleDecoder[A]): Either[ScopedError, A] =
      decoder
        .decode(scope, arg)
        .leftMap { case ScopedError(scope, error) => ScopedError(scope, s"[function: $functionName, arg-idx: $argIdx, type: ${decoder.typeName}] ") }

    implicit val make0: Decoder[Unit] =
      make() { (_, _) => { case List() => ().asRight } }

    implicit def make1[A0](implicit d0: SingleDecoder[A0]): Decoder[A0] =
      make(d0) { (functionName, scope) =>
        { case List(a0) => decode(scope, functionName, 0, a0, d0) }
      }

    implicit def make2[A0, A1](implicit d0: SingleDecoder[A0], d1: SingleDecoder[A1]): Decoder[(A0, A1)] =
      make(d0, d1) { (functionName, scope) =>
        { case List(a0, a1) =>
          for {
            v0 <- decode(scope, functionName, 0, a0, d0)
            v1 <- decode(scope, functionName, 1, a1, d1)
          } yield (v0, v1)
        }
      }

    implicit def make3[A0, A1, A2](implicit d0: SingleDecoder[A0], d1: SingleDecoder[A1], d2: SingleDecoder[A2]): Decoder[(A0, A1, A2)] =
      make(d0, d1, d2) { (functionName, scope) =>
        { case List(a0, a1, a2) =>
          for {
            v0 <- decode(scope, functionName, 0, a0, d0)
            v1 <- decode(scope, functionName, 1, a1, d1)
            v2 <- decode(scope, functionName, 2, a2, d2)
          } yield (v0, v1, v2)
        }
      }

    implicit def make4[A0, A1, A2, A3](implicit d0: SingleDecoder[A0], d1: SingleDecoder[A1], d2: SingleDecoder[A2], d3: SingleDecoder[A3]): Decoder[(A0, A1, A2, A3)] =
      make(d0, d1, d2, d3) { (functionName, scope) =>
        { case List(a0, a1, a2, a3) =>
          for {
            v0 <- decode(scope, functionName, 0, a0, d0)
            v1 <- decode(scope, functionName, 1, a1, d1)
            v2 <- decode(scope, functionName, 2, a2, d2)
            v3 <- decode(scope, functionName, 3, a3, d3)
          } yield (v0, v1, v2, v3)
        }
      }

    implicit def make5[A0, A1, A2, A3, A4](implicit d0: SingleDecoder[A0], d1: SingleDecoder[A1], d2: SingleDecoder[A2], d3: SingleDecoder[A3], d4: SingleDecoder[A4]): Decoder[(A0, A1, A2, A3, A4)] =
      make(d0, d1, d2, d3, d4) { (functionName, scope) =>
        { case List(a0, a1, a2, a3, a4) =>
          for {
            v0 <- decode(scope, functionName, 0, a0, d0)
            v1 <- decode(scope, functionName, 1, a1, d1)
            v2 <- decode(scope, functionName, 2, a2, d2)
            v3 <- decode(scope, functionName, 3, a3, d3)
            v4 <- decode(scope, functionName, 4, a4, d4)
          } yield (v0, v1, v2, v3, v4)
        }
      }

  }

}
