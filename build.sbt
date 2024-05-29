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
      // petaform
      `petaform-model`,
      `petaform-model-repr`,
      `petaform-model-terraform`,
      `petaform-parsing`,
      `petaform-main`,
      // example
      `petaform-example--main`,
    )

lazy val `petaform-model` =
  project
    .in(file("modules/petaform-model"))
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
    .in(file("modules/petaform-model-repr"))
    .settings(
      name := "petaform-model-repr",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`petaform-model` % testAndCompile)

lazy val `petaform-model-terraform` =
  project
    .in(file("modules/petaform-model-terraform"))
    .settings(
      name := "petaform-model-terraform",
      publishSettings,
      miscSettings,
      testSettings,
    )
    .dependsOn(`petaform-model-repr` % testAndCompile)

lazy val `petaform-parsing` =
  project
    .in(file("modules/petaform-parsing"))
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
    .in(file("modules/petaform-main"))
    .settings(
      name := "petaform-main",
      publishSettings,
      miscSettings,
      testSettings,
      // version := "0.1.1",
      assemblyJarName := s"../../../../jars/${name.value}-${version.value}.jar",
      // Compile / fork := true,
      Test / fork := true,
    )
    .dependsOn(
      `petaform-model-terraform` % testAndCompile,
      `petaform-parsing` % testAndCompile,
    )

// =====| Example |=====

// --- Log Entry ---

lazy val `petaform-example--log-entry` =
  project
    .in(file("example/modules/log-entry"))
    .aggregate(
      `petaform-example--log-entry--domain-model`,
      `petaform-example--log-entry--db-model`,
      `petaform-example--log-entry--domain`,
      `petaform-example--log-entry--domain-live`,
      `petaform-example--log-entry--main`,
    )
    .settings(
      publish / skip := true,
    )

lazy val `petaform-example--log-entry--domain-model` =
  project
    .in(file("example/modules/log-entry/domain-model"))
    .settings(
      name := "petaform-example--log-entry--domain-model",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
        MyOrg %% "harness-pk" % Versions.harness,
      ),
      Test / fork := true,
      publish / skip := true,
    )

lazy val `petaform-example--log-entry--db-model` =
  project
    .in(file("example/modules/log-entry/db-model"))
    .settings(
      name := "petaform-example--log-entry--db-model",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-sql" % Versions.harness,
      ),
      Test / fork := true,
      publish / skip := true,
    )
    .dependsOn(
      `petaform-example--log-entry--domain-model`,
    )

lazy val `petaform-example--log-entry--domain` =
  project
    .in(file("example/modules/log-entry/domain"))
    .settings(
      name := "petaform-example--log-entry--domain",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
      Test / fork := true,
      publish / skip := true,
    )
    .dependsOn(
      `petaform-example--log-entry--domain-model`,
    )

lazy val `petaform-example--log-entry--domain-live` =
  project
    .in(file("example/modules/log-entry/domain-live"))
    .settings(
      name := "petaform-example--log-entry--domain-live",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
      Test / fork := true,
      publish / skip := true,
    )
    .dependsOn(
      `petaform-example--log-entry--domain`,
      `petaform-example--log-entry--db-model`,
    )

lazy val `petaform-example--log-entry--main` =
  project
    .in(file("example/modules/log-entry/main"))
    .settings(
      name := "petaform-example--log-entry--main",
      publishSettings,
      miscSettings,
      testSettings,
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
      Test / fork := true,
      publish / skip := true,
    )
    .dependsOn(
      `petaform-example--log-entry--domain-live`,
    )

// --- Main ---

lazy val `petaform-example--main` =
  project
    .in(file("example/modules/main"))
    .settings(
      name := "petaform-example--main",
      publishSettings,
      miscSettings,
      testSettings,
      assemblyJarName := {
        val versionEnvVar = "APP_VERSION"
        val appVersion = scala.sys.env.getOrElse(versionEnvVar, throw new RuntimeException(s"Assembly requires '$versionEnvVar' env var"))
        s"../../../../jars/${name.value}--$appVersion.jar"
      },
      libraryDependencies ++= Seq(
        MyOrg %% "harness-zio" % Versions.harness,
      ),
      // Compile / fork := true,
      Test / fork := true,
      publish / skip := true,
    )
    .dependsOn(
      `petaform-example--log-entry--main`,
    )
