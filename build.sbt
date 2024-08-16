import sbtrelease.ReleaseStateTransformations.*
import xerial.sbt.Sonatype.*

import java.io.OutputStream
import scala.language.postfixOps
import scala.sys.process.*

lazy val javaVersion = "14"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val versionResource =
  settingKey[File]("Location of generated version resource file.")

lazy val root = (project in file("."))
  .settings(
    publishTo              := sonatypePublishToBundle.value,
    pomIncludeRepository   := { _ => false },
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    publishMavenStyle      := true,
    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
    organization     := "com.sageserpent",
    organizationName := "sageserpent",
    description := "Merge branches in the presence of code motion within and between files.",
    sonatypeProjectHosting := Some(
      GitHubHosting(
        user = "sageserpent-open",
        repository = "kineticMerge",
        email = "gjmurphy1@icloud.com"
      )
    ),
    releaseCrossBuild := false, // No cross-building here - just Scala 3.
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommand("packageExecutable"),
      releaseStepCommand(
        "publishSigned"
      ), // ... finally the publishing step using SBT's own mechanism.
      releaseStepCommand("sonatypeBundleRelease"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    scalacOptions ++= List("-source:future"),
    name := "kinetic-merge",
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

      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !

      name.value
    },
    packageExecutable := (packageExecutable dependsOn publishLocal).value,
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
    libraryDependencies += "ch.qos.logback"    % "logback-core"    % "1.4.14",
    libraryDependencies += "ch.qos.logback"    % "logback-classic" % "1.4.14",
    libraryDependencies += "org.typelevel"    %% "cats-core"       % "2.9.0",
    libraryDependencies += "com.github.scopt" %% "scopt"           % "4.1.0",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.6",
    libraryDependencies += "org.typelevel" %% "cats-core"      % "2.10.0",
    libraryDependencies += "org.typelevel" %% "alleycats-core" % "2.10.0",
    libraryDependencies += "org.typelevel" %% "cats-effect"    % "3.5.2",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "com.lihaoyi"             %% "os-lib"  % "0.9.1",
    libraryDependencies += "com.lihaoyi"             %% "fansi"   % "0.4.0",
    libraryDependencies += "com.lihaoyi"             %% "pprint"  % "0.8.1",
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.4",
    libraryDependencies += "com.google.guava" % "guava"      % "32.1.2-jre",
    libraryDependencies += "de.sciss"        %% "fingertree" % "1.5.5",
    libraryDependencies += "com.github.ben-manes.caffeine" % "caffeine" % "3.1.8",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.11.0",
    libraryDependencies += "me.tongfei"         % "progressbar"  % "0.10.1",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    libraryDependencies += "com.sageserpent" %% "americium" % "1.19.1" % Test,
    libraryDependencies += "eu.timepit"      %% "refined"   % "0.11.2",
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test,
    libraryDependencies += "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    Test / testOptions += Tests.Argument(jupiterTestFramework, "-q"),
    Test / fork               := true,
    Test / testForkedParallel := true,
    Test / outputStrategy := Some(
      OutputStrategy.CustomOutput(OutputStream.nullOutputStream)
    )
  )
