package petaform.example.main

import harness.zio.*

object Main extends ExecutableApp {

  override val executable: Executable =
    Executable.fromSubCommands(
      "log-entry" -> petaform.example.logEntry.main.Main.executable,
    )

}
