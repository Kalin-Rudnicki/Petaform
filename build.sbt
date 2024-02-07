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
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `petaform-model`,
      `petaform-model-repr`,
      `petaform-model-terraform`,
      `petaform-parsing`,
      `petaform-main`,
    )

lazy val `petaform-model` =
  project
    .in(file("petaform-model"))
    .settings(
      name := "petaform-model",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-core" % HarnessVersion,
        "org.typelevel" %% "shapeless3-deriving" % "3.4.1-3-4f382ff-SNAPSHOT",
      ),
    )

lazy val `petaform-model-repr` =
  project
    .in(file("petaform-model-repr"))
    .settings(
      name := "petaform-model-repr",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`petaform-model`)

lazy val `petaform-model-terraform` =
  project
    .in(file("petaform-model-terraform"))
    .settings(
      name := "petaform-model-terraform",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`petaform-model-repr`)

lazy val `petaform-parsing` =
  project
    .in(file("petaform-parsing"))
    .settings(
      name := "petaform-parsing",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % HarnessVersion,
        MyOrg %% "slyce-parse" % "2.0.8",
      ),
    )
    .dependsOn(`petaform-model`)

lazy val `petaform-main` =
  project
    .in(file("petaform-main"))
    .settings(
      name := "petaform-main",
      publishSettings,
      miscSettings,
      testSettings,
      assemblyJarName := s"${name.value}-${version.value}.jar",
      Compile / fork := true,
      Test / fork := true,
    )
    .dependsOn(`petaform-model-terraform`, `petaform-parsing`)
