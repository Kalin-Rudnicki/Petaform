package petaform.example.logEntry.domain.model

import java.time.Instant

final case class Entry(
    id: EntryId,
    message: String,
    version: String,
    timestamp: Instant,
)
