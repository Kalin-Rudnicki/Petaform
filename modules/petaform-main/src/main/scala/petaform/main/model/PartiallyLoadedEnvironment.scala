package petaform.main.model

import cats.syntax.either.*
import cats.syntax.traverse.*
import petaform.model.*
import petaform.model.ast.*
import petaform.model.conversion.*

final case class PartiallyLoadedEnvironment private (
    envName: String,
    configPaths: Map[String, List[String]],
    rawPetaformAST: RawPetaformAST.Obj,
)
object PartiallyLoadedEnvironment {

  def mapFromRawEnvsAST(rawEnvsAST: RawPetaformAST, envVars: EnvVars): Either[ScopedError, Map[String, PartiallyLoadedEnvironment]] =
    for {
      envMap <- ConversionUtil.rawASTToMap(rawEnvsAST, ScopePath.empty)
      envList <- envMap.toList.traverse { case (k, v) => parseEnvironment(k, v, envVars) }
    } yield envList.toMap

  // =====|  |=====

  private def parseEnvironment(envName: String, envAST: RawPetaformAST, envVars: EnvVars): Either[ScopedError, (String, PartiallyLoadedEnvironment)] = {
    val envScope = ScopePath.reversed(ASTScope.Key(envName) :: Nil)
    val configsScope = envScope :+ ASTScope.Key("configs")

    for {
      envASTObj <- ConversionUtil.safeCastRawAST[RawPetaformAST.Obj](envAST, envScope)
      configsValue <- ConversionUtil.getProvidedValue(envASTObj, "configs", envScope)
      configsMap <- ConversionUtil.safeCastRawAST[RawPetaformAST.Obj](configsValue.value, configsScope)
      configsPairs <- configsMap.elems.traverse {
        case (key, RawPetaformAST.Obj.Value.Provided(_, ast)) =>
          ConversionUtil.safeCastRawAST[RawPetaformAST.Arr](ast, configsScope :+ ASTScope.Key(key)).map((key, _))
        case (key, RawPetaformAST.Obj.Value.Required) =>
          ScopedError(configsScope :+ ASTScope.Key(key), "Must be provided").asLeft
      }
      configs <- configsPairs.traverse { case (key, configsArray) =>
        val keyScope = configsScope :+ ASTScope.Key(key)
        configsArray.elems.zipWithIndex
          .traverse { case (elem, idx) =>
            val elemScope = keyScope :+ ASTScope.Idx(idx)
            ConversionUtil.safeCastRawAST2[RawPetaformAST.Raw, RawPetaformAST.Str](elem, elemScope).flatMap {
              case RawPetaformAST.Raw(str) =>
                str.asRight
              case RawPetaformAST.Str(str) =>
                RawASTToAST.Stage1
                  .attemptConvertInterpolatedString(str, elemScope, envVars)
                  .flatMap {
                    _.toRight(ScopedError(elemScope, "config path can not contain CFG interpolation"))
                  }
            }
          }
          .map((key, _))
      }
      rootAst = RawPetaformAST.Obj(
        (
          envName,
          RawPetaformAST.Obj.Value.Provided(true, envAST),
        ) :: Nil,
      )
    } yield (envName, PartiallyLoadedEnvironment(envName, configs.toMap, rootAst))
  }

}
