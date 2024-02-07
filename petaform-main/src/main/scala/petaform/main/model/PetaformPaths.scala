package petaform.main.model

import harness.zio.*

final case class PetaformPaths private (
    petaformPath: Path,
    internalPath: Path,
    internalEnvironmentsPath: Path,
    environmentsPath: Path,
    resourcesPath: Path,
    configPath: Path,
)
object PetaformPaths {

  def fromPetaformPathString(pathString: String): HRIO[FileSystem, PetaformPaths] =
    for {
      // TODO (KR) : ensure exists
      petaformPath <- Path(pathString)
      internalPath <- petaformPath.child(".internal")
      internalEnvironmentsPath <- internalPath.child("environments")
      environmentsPath <- petaformPath.child("environments.conf")
      resourcesPath <- petaformPath.child("resources.conf")
      configPath <- petaformPath.child("config")

      _ <- internalPath.mkdirs.unlessZIO(internalPath.exists)
      _ <- internalEnvironmentsPath.mkdirs.unlessZIO(internalEnvironmentsPath.exists)
    } yield PetaformPaths(petaformPath, internalPath, internalEnvironmentsPath, environmentsPath, resourcesPath, configPath)

}
