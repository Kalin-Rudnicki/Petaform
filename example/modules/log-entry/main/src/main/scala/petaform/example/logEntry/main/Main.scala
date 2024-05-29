package petaform.example.logEntry.main

import harness.core.*
import harness.sql.*
import harness.sql.autoSchema.*
import harness.zio.*
import petaform.example.logEntry.db.model.LegacyTables
import petaform.example.logEntry.domain.*
import petaform.example.logEntry.domain.live.*
import petaform.example.logEntry.domain.model.*
import zio.*
import zio.json.*

object Main {

  type Env = EntryStorage & AppConfig

  private val envLayer: RLayer[HarnessEnv & Scope, Env] =
    ZLayer.makeSome[HarnessEnv & Scope, Env](
      HConfig.readLayer[DbConfig]("db"),
      HConfig.readLayer[AppConfig]("app-config"),
      JDBCConnectionPool.configLayerWithMigrations {
        PlannedMigrations(
          InMemoryMigration.auto(Version.parseUnsafe("0.0.1"), Tables.fromCompanions(LegacyTables.entry.V1)),
        )
      },
      JDBCConnection.poolLayer,
      LiveEntryStorage.layer,
    )

  final case class AppConfig(
      version: String,
      message: String,
      showLastNEntries: Option[Int],
  )
  object AppConfig {
    implicit val jsonCodec: JsonCodec[AppConfig] = DeriveJsonCodec.gen
  }

  val executable: Executable =
    Executable
      .withLayer { envLayer }
      .withThrowableEffect {
        for {
          now <- Clock.instant
          _ <- Logger.log.important(s"Starting LogEntry Main - $now")
          entryStorage <- ZIO.service[EntryStorage]
          cfg <- ZIO.service[AppConfig]
          id <- EntryId.genZio
          row = Entry(id, cfg.message, cfg.version, now)
          _ <- entryStorage.insertEntry(row)
          _ <- ZIO.foreachDiscard(cfg.showLastNEntries) { n =>
            for {
              _ <- ZIO.fail(new RuntimeException(s"$n <= 0")).when(n <= 0)
              history <- entryStorage.getEntries
              filtered = history.takeRight(n)
              _ <- ZIO.foreachDiscard(filtered) { e => Logger.log.info(s"[${e.timestamp}] ${e.version} : ${e.message}") }
            } yield ()
          }
        } yield ()
      }

}
