//

// =====| Shared Settings |=====

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

val Scala_3 = "3.3.0"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "petaform"

ThisBuild / watchBeforeCommand := Watch.clearScreen

lazy val testAndCompile = "test->test;compile->compile"

lazy val miscSettings =
  Seq(
    scalaVersion := Scala_3,
    scalacOptions ++= Seq("-source:future", "-Ycheck-all-patmat", "-Wunused:all", "-Werror", "-language:implicitConversions", "-deprecation", "-feature"),
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
  )

lazy val publishSettings =
  Seq(
    organization := MyOrg,
    description := "Miscellaneous libraries/utilities for Scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
  )

lazy val testSettings =
  Seq(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

lazy val HarnessVersion = "3.1.14"

// =====| Projects |=====

lazy val `petaform-root` =
  project
    .in(file("."))
    .aggregate(
      `petaform-core`,
    )

lazy val `petaform-core` =
  project
    .in(file("petaform-core"))
    .settings(
      name := "petaform-core",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % HarnessVersion,
        MyOrg %% "harness-zio-test" % HarnessVersion % Test,
        MyOrg %% "slyce-parse" % "2.0.8",
        MyOrg %% "slyce-parse-exe" % "2.0.8" % Test,
        "org.typelevel" %% "shapeless3-deriving" % "3.4.1-3-4f382ff-SNAPSHOT",
      ),
      Compile / fork := true,
      Test / fork := true,
    )
