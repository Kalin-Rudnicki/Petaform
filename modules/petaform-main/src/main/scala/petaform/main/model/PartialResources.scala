package petaform.main.model

import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*

final class PartialResources private (private val resourcesMap: Map[String, Map[String, RawPetaformAST]]) {

  def getRawVariantAST(resourceName: String, variantName: String): Either[ScopedError, RawPetaformAST] =
    for {
      resourceMap <- resourcesMap.get(resourceName).toRight(ScopedError(ScopePath.inOrder(List(ASTScope.Key(resourceName))), "Missing resource"))
      variantAST <- resourceMap.get(variantName).toRight(ScopedError(ScopePath.inOrder(List(ASTScope.Key(resourceName), ASTScope.Key(variantName))), "Missing variant"))
    } yield variantAST

}
object PartialResources {

  def fromRawResourcesAST(rawResourcesAST: RawPetaformAST): Either[ScopedError, PartialResources] =
    for {
      map1 <- ConversionUtil.rawASTToMap(rawResourcesAST, ScopePath.empty)
      list2 <- map1.toList.traverse { case (k, v) => ConversionUtil.rawASTToMap(v, ScopePath.reversed(ASTScope.Key(k) :: Nil)).map((k, _)) }
    } yield new PartialResources(list2.toMap)

}
