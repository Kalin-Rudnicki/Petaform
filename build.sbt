//

// =====| Shared Settings |=====

enablePlugins(GitVersioning)
git.gitTagToVersionNumber := { tag =>
  if (tag.matches("^\\d+\\..*$")) Some(tag)
  else None
}

val Scala_3 = "3.3.0"

val MyOrg = "io.github.kalin-rudnicki"

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
    description := "Utility to sit on top of terraform.",
  )

lazy val testSettings =
  Seq(
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

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
        MyOrg %% "harness-zio" % Versions.harness,
        MyOrg %% "harness-deriving" % Versions.harness,
        MyOrg %% "harness-zio-test" % Versions.harness % Test,
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
    .dependsOn(`petaform-model` % testAndCompile)

lazy val `petaform-model-terraform` =
  project
    .in(file("petaform-model-terraform"))
    .settings(
      name := "petaform-model-terraform",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`petaform-model-repr` % testAndCompile)

lazy val `petaform-parsing` =
  project
    .in(file("petaform-parsing"))
    .settings(
      name := "petaform-parsing",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "slyce-parse" % Versions.slyce,
      ),
    )
    .dependsOn(`petaform-model` % testAndCompile)

lazy val `petaform-main` =
  project
    .in(file("petaform-main"))
    .settings(
      name := "petaform-main",
      publishSettings,
      miscSettings,
      testSettings,
      version := "0.0.10",
      assemblyJarName := s"../../../jars/${name.value}-${version.value}.jar",
      // Compile / fork := true,
      Test / fork := true,
    )
    .dependsOn(
      `petaform-model-terraform` % testAndCompile,
      `petaform-parsing` % testAndCompile,
    )
