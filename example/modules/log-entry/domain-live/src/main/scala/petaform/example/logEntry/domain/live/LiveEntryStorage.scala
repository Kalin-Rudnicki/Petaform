package petaform.example.logEntry.domain.live

import harness.sql.*
import harness.zio.*
import petaform.example.logEntry.db.model as DB
import petaform.example.logEntry.domain.EntryStorage
import petaform.example.logEntry.domain.model as Domain
import zio.*

final case class LiveEntryStorage(con: JDBCConnection) extends EntryStorage {
  import LiveEntryStorage.*

  override def insertEntry(entry: Domain.Entry): RIO[Logger & Telemetry, Unit] =
    con.use { Q.insert(entry.toDb).single }

  override def getEntryById(id: Domain.EntryId): RIO[Logger & Telemetry, Domain.Entry] =
    con.use { Q.selectById(id).single.map(_.toDomain) }

  override def getEntries: RIO[Logger & Telemetry, Chunk[Domain.Entry]] =
    con.use { Q.selectAll().chunk.map(_.map(_.toDomain)) }

}
object LiveEntryStorage {

  val layer: URLayer[JDBCConnection, EntryStorage] =
    ZLayer.fromFunction { LiveEntryStorage.apply }

  extension (self: Domain.Entry) {
    private def toDb: DB.Entry.Identity =
      new DB.Entry.Identity(
        id = self.id,
        message = self.message,
        version = self.version,
        timestamp = self.timestamp,
      )
  }
  extension (self: DB.Entry.Identity) {
    private def toDomain: Domain.Entry =
      Domain.Entry(
        id = self.id,
        message = self.message,
        version = self.version,
        timestamp = self.timestamp,
      )
  }

  private object Q extends TableQueries[Domain.EntryId, DB.Entry]

}
