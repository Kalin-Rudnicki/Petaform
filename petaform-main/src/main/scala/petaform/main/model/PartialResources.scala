package petaform.main.model

import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*

final class PartialResources private (private val resourcesMap: Map[String, Map[String, RawPetaformAST]]) {

  def getRawVariantAST(resourceName: String, variantName: String): Either[ScopedError, RawPetaformAST] =
    for {
      resourceMap <- resourcesMap.get(resourceName).toRight(ScopedError(List(ASTScope.Key(resourceName)), "Missing resource"))
      variantAST <- resourceMap.get(variantName).toRight(ScopedError(List(ASTScope.Key(resourceName), ASTScope.Key(variantName)), "Missing variant"))
    } yield variantAST

}
object PartialResources {

  def fromRawResourcesAST(rawResourcesAST: RawPetaformAST): Either[ScopedError, PartialResources] =
    for {
      map1 <- ConversionUtil.rawASTToMap(rawResourcesAST, Nil)
      list2 <- map1.toList.traverse { case (k, v) => ConversionUtil.rawASTToMap(v, ASTScope.Key(k) :: Nil).map((k, _)) }
    } yield new PartialResources(list2.toMap)

}
