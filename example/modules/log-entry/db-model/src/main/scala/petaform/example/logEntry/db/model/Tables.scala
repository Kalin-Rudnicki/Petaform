package petaform.example.logEntry.db.model

import harness.sql.*
import java.time.Instant
import petaform.example.logEntry.domain.model as Domain

final case class Entry[F[_]](
    id: F[Entry.Id],
    message: F[String],
    version: F[String],
    timestamp: F[Instant],
) extends Table.WithId[F, Entry.Id]
object Entry extends Table.Companion.WithId[Domain.EntryId, Entry] {

  override implicit lazy val tableSchema: TableSchema[Entry] =
    TableSchema.derived[Entry]("entry") {
      new Entry.Cols(
        id = Entry.Id.pkCol,
        message = Col.string("message"),
        version = Col.string("version"),
        timestamp = Col.instant("timestamp"),
      )
    }

}
