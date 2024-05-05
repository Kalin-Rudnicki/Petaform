package petaform.model.conversion

import cats.syntax.either.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import scala.reflect.ClassTag

object ConversionUtil {

  def rawASTToMap(rawAST: RawPetaformAST, scope: ScopePath): Either[ScopedError, Map[String, RawPetaformAST]] =
    safeCastRawAST[RawPetaformAST.Obj](rawAST, scope).flatMap { obj =>
      obj.elems
        .traverse {
          case (key, RawPetaformAST.Obj.Value.Provided(_, value)) => (key, value).asRight
          case (key, RawPetaformAST.Obj.Value.Required)           => ScopedError(scope :+ ASTScope.Key(key), "Can not convert key with `@required` value into map").asLeft
        }
        .map(_.toMap)
    }

  def rawASTToProvidedMap(rawAST: RawPetaformAST, scope: ScopePath): Either[ScopedError, Map[String, RawPetaformAST.Obj.Value.Provided]] =
    safeCastRawAST[RawPetaformAST.Obj](rawAST, scope).flatMap { obj =>
      obj.elems
        .traverse {
          case (key, value: RawPetaformAST.Obj.Value.Provided) => (key, value).asRight
          case (key, RawPetaformAST.Obj.Value.Required)        => ScopedError(scope :+ ASTScope.Key(key), "Can not convert key with `@required` value into map").asLeft
        }
        .map(_.toMap)
    }

  def safeCastRawAST[T <: RawPetaformAST](
      rawAST: RawPetaformAST,
      scope: ScopePath,
  )(implicit ct: ClassTag[T]): Either[ScopedError, T] =
    rawAST match {
      case ct(t) => t.asRight
      case _     => ScopedError(scope, s"Expected ${ct.runtimeClass.getSimpleName}, got ${rawAST.getClass.getSimpleName}").asLeft
    }

  def safeCastRawAST2[T1 <: RawPetaformAST, T2 <: RawPetaformAST](
      rawAST: RawPetaformAST,
      scope: ScopePath,
  )(implicit ct1: ClassTag[T1], ct2: ClassTag[T2]): Either[ScopedError, T1 | T2] =
    rawAST match {
      case ct1(t1) => t1.asRight
      case ct2(t2) => t2.asRight
      case _       => ScopedError(scope, s"Expected (${ct1.runtimeClass.getSimpleName} | ${ct2.runtimeClass.getSimpleName}), got ${rawAST.getClass.getSimpleName}").asLeft
    }

  def getProvidedValue(obj: RawPetaformAST.Obj, key: String, scope: ScopePath): Either[ScopedError, RawPetaformAST.Obj.Value.Provided] = {
    val newScope = scope :+ ASTScope.Key(key)
    obj.elems.toMap.get(key) match {
      case Some(provided: RawPetaformAST.Obj.Value.Provided) => provided.asRight
      case Some(RawPetaformAST.Obj.Value.Required)           => ScopedError(newScope, "key must be provided").asLeft
      case None                                              => ScopedError(newScope, "missing key").asLeft
    }
  }

}
