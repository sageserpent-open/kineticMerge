import sbtrelease.ReleaseStateTransformations.*

import scala.language.postfixOps

lazy val javaVersion = "17"

ThisBuild / scalaVersion := "3.3.6"

ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)

ThisBuild / scalacOptions ++= List(
  s"-java-output-version:$javaVersion",
  "-source:future"
)

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val versionResource =
  settingKey[File]("Location of generated version resource file.")

lazy val root = (project in file("."))
  .settings(
    pomIncludeRepository := { _ => false },
    publishMavenStyle    := true,
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
    organization     := "com.sageserpent",
    organizationName := "sageserpent",
    description := "Merge branches in the presence of code motion within and between files.",
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
    ),
    name            := "kinetic-merge",
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
    packageExecutable := {
      val packagingVersion = (ThisBuild / version).value

      println(s"Packaging executable with version: $packagingVersion")

      val localArtifactCoordinates =
        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:$packagingVersion"

      val executablePath = s"${target.value}${Path.sep}${name.value}"

      coursier.cli.Coursier.main(
        s"bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath"
          .split("\\s+")
      )

      name.value
    },
    packageExecutable := (packageExecutable dependsOn publishLocal).value,
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    libraryDependencies += "ch.qos.logback"    % "logback-core"    % "1.5.18",
    libraryDependencies += "ch.qos.logback"    % "logback-classic" % "1.5.18",
    libraryDependencies += "org.typelevel"    %% "cats-core"       % "2.9.0",
    libraryDependencies += "com.github.scopt" %% "scopt"           % "4.1.0",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.10",
    libraryDependencies += "org.typelevel" %% "cats-core"      % "2.13.0",
    libraryDependencies += "org.typelevel" %% "alleycats-core" % "2.13.0",
    libraryDependencies += "org.typelevel" %% "cats-effect"    % "3.6.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.4.0",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.3.0",
      "dev.optics" %% "monocle-macro" % "3.3.0"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "com.lihaoyi"             %% "os-lib"  % "0.11.5",
    libraryDependencies += "com.lihaoyi"             %% "fansi"   % "0.5.1",
    libraryDependencies += "com.lihaoyi"             %% "pprint"  % "0.9.4",
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.5",
    libraryDependencies += "com.google.guava" % "guava"      % "33.5.0-jre",
    libraryDependencies += "de.sciss"        %% "fingertree" % "1.5.5",
    libraryDependencies += "com.github.ben-manes.caffeine" % "caffeine" % "3.2.2",
    libraryDependencies += "me.tongfei" % "progressbar" % "0.10.1",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    libraryDependencies += "com.sageserpent" %% "americium" % "1.22.0" % Test,
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.17.0" % Test,
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.14.0" % Test,
    libraryDependencies += "com.github.sbt.junit" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    Test / test / logLevel    := Level.Error,
    Test / fork               := true,
    Test / testForkedParallel := true
  )
