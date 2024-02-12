package petaform.main.model

import cats.syntax.either.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*

final case class PartiallyLoadedEnvironment private (
    envName: String,
    configPaths: List[String],
    rawPetaformAST: RawPetaformAST.Obj,
)
object PartiallyLoadedEnvironment {

  def mapFromRawEnvsAST(rawEnvsAST: RawPetaformAST, envVars: EnvVars): Either[ScopedError, Map[String, PartiallyLoadedEnvironment]] =
    for {
      envMap <- ConversionUtil.rawASTToMap(rawEnvsAST, Nil)
      envList <- envMap.toList.traverse { case (k, v) => parseEnvironment(k, v, envVars) }
    } yield envList.toMap

  // =====|  |=====

  private def parseEnvironment(envName: String, envAST: RawPetaformAST, envVars: EnvVars): Either[ScopedError, (String, PartiallyLoadedEnvironment)] = {
    val envRScope = ASTScope.Key(envName) :: Nil
    val configsRScope = ASTScope.Key("configs") :: envRScope

    for {
      envASTObj <- ConversionUtil.safeCastRawAST[RawPetaformAST.Obj](envAST, envRScope)
      configsValue <- ConversionUtil.getProvidedValue(envASTObj, "configs", envRScope)
      configsArray <- ConversionUtil.safeCastRawAST[RawPetaformAST.Arr](configsValue.value, configsRScope)
      configs <- configsArray.elems.zipWithIndex.traverse { case (elem, idx) =>
        val elemRScope = ASTScope.Idx(idx) :: configsRScope
        ConversionUtil.safeCastRawAST2[RawPetaformAST.Raw, RawPetaformAST.Str](elem, elemRScope).flatMap {
          case RawPetaformAST.Raw(str) => str.asRight
          case RawPetaformAST.Str(str) =>
            RawASTToAST
              .convertStringWithoutConfig(str, elemRScope, envVars)
              .flatMap { _.toRight(ScopedError(elemRScope.reverse, "config path can not contain CFG interpolation")) }
        }
      }
      rootAst = RawPetaformAST.Obj(
        (
          envName,
          RawPetaformAST.Obj.Value.Provided(true, envAST),
        ) :: Nil,
      )
    } yield (envName, PartiallyLoadedEnvironment(envName, configs, rootAst))
  }

}
