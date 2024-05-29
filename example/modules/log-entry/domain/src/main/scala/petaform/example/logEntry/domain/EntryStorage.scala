package petaform.example.logEntry.domain

import harness.zio.*
import petaform.example.logEntry.domain.model.*
import zio.*

trait EntryStorage {
  def insertEntry(entry: Entry): RIO[Logger & Telemetry, Unit]
  def getEntryById(id: EntryId): RIO[Logger & Telemetry, Entry]
  def getEntries: RIO[Logger & Telemetry, Chunk[Entry]]
}
