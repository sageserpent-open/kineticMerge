import sbtrelease.ReleaseStateTransformations.*

import scala.language.postfixOps

lazy val javaVersion = "17"

ThisBuild / scalaVersion := "3.9.0"

ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)

ThisBuild / scalacOptions ++= List(
  s"-java-output-version:$javaVersion",
  "-deprecation"
)

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val applicationExecutableName =
  settingKey[String]("Executable name produced by Coursier packaging")

lazy val versionResource =
  settingKey[File]("Location of generated version resource file.")

lazy val commonSettings = Seq(
  organization     := "com.sageserpent",
  organizationName := "sageserpent",
  licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
  pomIncludeRepository := { _ => false },
  publishMavenStyle    := true
)

lazy val commonLibraryDependencies = Seq(
  libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
  libraryDependencies += "ch.qos.logback"    % "logback-core"    % "1.6.5",
  libraryDependencies += "ch.qos.logback"    % "logback-classic" % "1.6.5",
  libraryDependencies += "org.typelevel"    %% "cats-core"       % "2.13.0",
  libraryDependencies += "com.sageserpent" %% "americium-utilities" % "2.2.2",
  libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.10",
  libraryDependencies += "org.typelevel" %% "alleycats-core" % "2.13.0",
  libraryDependencies += "org.typelevel" %% "cats-effect"    % "3.7.1",
  libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.4.0",
  libraryDependencies ++= Seq(
    "dev.optics" %% "monocle-core"  % "3.3.0",
    "dev.optics" %% "monocle-macro" % "3.3.0"
  ),
  libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.5.0",
  libraryDependencies += "com.lihaoyi"             %% "os-lib"  % "0.11.8",
  libraryDependencies += "com.lihaoyi"             %% "fansi"   % "0.5.1",
  libraryDependencies += "com.lihaoyi"             %% "pprint"  % "0.9.6",
  libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.5",
  libraryDependencies += "com.google.guava" % "guava" % "33.7.2-jre",
  libraryDependencies += "com.github.ben-manes.caffeine" % "caffeine" % "3.3.0",
  libraryDependencies += "me.tongfei"         % "progressbar"   % "0.10.2",
  libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.21.0",
  libraryDependencies +=
    "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
  libraryDependencies += "org.typelevel" %% "kittens" % "3.5.0",
  libraryDependencies += "io.github.dotty-cps-async" %% "dotty-cps-async" % "1.4.0",

  libraryDependencies += "de.sciss"        %% "fingertree" % "1.5.5" % Test,
  libraryDependencies += "com.sageserpent" %% "americium"  % "2.2.2" % Test,
  libraryDependencies += "com.sageserpent" %% "americium-junit5" % "2.2.2" % Test,
  libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.17.1" % Test,
  libraryDependencies += "org.apache.commons" % "commons-text" % "1.15.0" % Test,
  libraryDependencies += "com.github.sbt.junit" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test
)

lazy val cliApplicationSettings = commonSettings ++ commonLibraryDependencies ++ Seq(
  publish / skip   := true,
  publishLocal / skip := false,
  libraryDependencies += "com.github.scopt" %% "scopt" % "4.2.0",
  packageExecutable := {
    val libPublished: Unit = (kineticMerge / Compile / publishLocal).value
    val mainPublished: Unit = (Compile / publishLocal).value

    val packagingVersion = (ThisBuild / version).value

    println(s"Packaging executable with version: $packagingVersion")

    val applicationName = applicationExecutableName.value

    val localArtifactCoordinates =
      s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:$packagingVersion"

    val executablePath = s"${target.value}${Path.sep}$applicationName"

    coursier.cli.Coursier.main(
      s"bootstrap --verbose --bat=true -M com.sageserpent.kineticmerge.Main --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath"
        .split("\\s+")
    )

    applicationName
  },
  Test / test / logLevel    := Level.Error,
  Test / fork               := true,
  Test / testForkedParallel := true,
  Test / javaOptions ++= Seq("-Xmx8G")
)

lazy val kineticMerge = (project in file("kinetic-merge"))
  .settings(
    commonSettings,
    commonLibraryDependencies,
    name        := "kinetic-merge",
    description := "Merge branches in the presence of code motion within and between files.",
    versionResource := {
      val additionalResourcesDirectory = (Compile / resourceManaged).value

      additionalResourcesDirectory.toPath.resolve("version.txt").toFile
    },
    Compile / resourceGenerators += Def.task {
      val location = versionResource.value

      val packagingVersion = (ThisBuild / version).value

      println(
        s"Generating version resource: $location for version: $packagingVersion"
      )

      IO.write(location, packagingVersion)

      Seq(location)
    }.taskValue,
    Test / test / logLevel    := Level.Error,
    Test / fork               := true,
    Test / testForkedParallel := true,
    Test / javaOptions ++= Seq("-Xmx8G")
  )

lazy val gitCliApplication = (project in file("git-cli-application"))
  .dependsOn(kineticMerge, kineticMerge % "test->test")
  .settings(
    cliApplicationSettings,
    name                      := "git-cli-application",
    applicationExecutableName := "kinetic-merge",
    description               := "Git CLI application for Kinetic Merge."
  )

lazy val toolCliApplication = (project in file("tool-cli-application"))
  .dependsOn(kineticMerge, kineticMerge % "test->test")
  .settings(
    cliApplicationSettings,
    name                      := "tool-cli-application",
    applicationExecutableName := "kinetic-merge-tool",
    description               := "Merge tool CLI application for Kinetic Merge."
  )

lazy val root = (project in file("."))
  .aggregate(kineticMerge, gitCliApplication, toolCliApplication)
  .settings(
    publish / skip := true,
    releaseCrossBuild := false, // No cross-building here - just Scala 3.
    releaseProcess    := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // *DO NOT* run `publishSigned`, `sonatypeBundleRelease` and
      // `pushChanges` - the equivalent is done on GitHub by
      // `gha-scala-library-release-workflow`.
      setNextVersion,
      commitNextVersion
    )
  )
