package petaform.main.model

import java.util.UUID
import zio.json.*
import zio.json.ast.Json

final case class TfState(
    version: Int,
    @jsonField("terraform_version") terraformVersion: String,
    lineage: UUID,
    resources: List[TfState.Resource],
    // TODO (KR) :
    // outputs: ?,
    // @jsonField("check_results") checkResults: ?,

)
object TfState {

  final case class Resource(
      mode: String,
      `type`: String,
      name: String,
      provider: String,
      instances: List[Instance],
  )
  object Resource {
    implicit val jsonCodec: JsonCodec[Resource] = DeriveJsonCodec.gen
  }

  final case class Instance(
      @jsonField("schema_version") schemaVersion: Int,
      attributes: Map[String, Json],
  )
  object Instance {
    implicit val jsonCodec: JsonCodec[Instance] = DeriveJsonCodec.gen
  }

  implicit val jsonCodec: JsonCodec[TfState] = DeriveJsonCodec.gen

}
