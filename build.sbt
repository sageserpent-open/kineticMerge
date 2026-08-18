import sbtrelease.ReleaseStateTransformations.*

import scala.language.postfixOps

lazy val javaVersion = "17"

ThisBuild / scalaVersion := "3.3.8"

Compile / javacOptions ++= Seq("--release", javaVersion)

Compile / scalacOptions ++= List(
  "-java-output-version", javaVersion,
  "-source:future"
)

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

lazy val versionResource =
  settingKey[File]("Location of generated version resource file.")

lazy val projectHoldingCoursierDependency = project.settings(
  scalaVersion                             := "2.13.18",
  libraryDependencies += "io.get-coursier" %% "coursier-cli" % "2.1.24",
  Compile / run / mainClass                := Some("coursier.cli.Coursier")
)

lazy val root = (project in file("."))
  .settings(
    pomIncludeRepository := { _ => false },
    publishMavenStyle    := true,
    licenses += ("MIT", uri("https://opensource.org/licenses/MIT")),
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
    packageExecutable := Def.uncached {
      Def
        .taskDyn({
          val packagingVersion = (ThisBuild / version).value

          println(s"Packaging executable with version: $packagingVersion")

          val localArtifactCoordinates =
            s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:$packagingVersion"

          val executablePath = s"${target.value}${Path.sep}${name.value}"

          val necessaryLeadingWhitespace = " "

          (projectHoldingCoursierDependency / Compile / run)
            .toTask(
              necessaryLeadingWhitespace + s"bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath"
            )
            .map(_ => name.value)
        })
        .value
    },
    packageExecutable := packageExecutable.dependsOn(publishLocal).value,
    libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6",
    libraryDependencies += "ch.qos.logback"    % "logback-core"    % "1.6.1",
    libraryDependencies += "ch.qos.logback"    % "logback-classic" % "1.6.1",
    libraryDependencies += "org.typelevel"    %% "cats-core"       % "2.13.0",
    libraryDependencies += "com.github.scopt" %% "scopt"           % "4.1.0",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.10",
    libraryDependencies += "org.typelevel" %% "cats-core"      % "2.13.0",
    libraryDependencies += "org.typelevel" %% "alleycats-core" % "2.13.0",
    libraryDependencies += "org.typelevel" %% "cats-effect"    % "3.7.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.4.0",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.3.0",
      "dev.optics" %% "monocle-macro" % "3.3.0"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    libraryDependencies += "com.lihaoyi"             %% "os-lib"  % "0.11.8",
    libraryDependencies += "com.lihaoyi"             %% "fansi"   % "0.5.1",
    libraryDependencies += "com.lihaoyi"             %% "pprint"  % "0.9.6",
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.5",
    libraryDependencies += "com.google.guava" % "guava"      % "33.6.0-jre",
    libraryDependencies += "de.sciss"        %% "fingertree" % "1.5.5" % Test,
    libraryDependencies += "com.github.ben-manes.caffeine" % "caffeine" % "3.2.4",
    libraryDependencies += "me.tongfei"         % "progressbar"   % "0.10.2",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.20.0",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0",
    libraryDependencies += "com.sageserpent" %% "americium" % "2.2.2" % Test,
    libraryDependencies += "com.sageserpent" %% "americium-junit5" % "2.2.2" % Test,
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.17.1" % Test,
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.15.0" % Test,
    libraryDependencies += "com.github.sbt.junit" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    libraryDependencies += "org.typelevel" %% "kittens" % "3.5.0",
    Test / logLevel                        := Level.Error,
    Global / logLevel                      := Level.Error,
    Test / testOptions += Tests.Argument(jupiterTestFramework, "-q"),
    Test / logBuffered                     := false,
    Test / fork                            := true,
    Test / testForkedParallel              := true,
    Test / javaOptions ++= Seq("-Xmx3g", "-XX:+UseG1GC"),
    Global / concurrentRestrictions := Seq(
      Tags.limit(Tags.ForkedTestGroup, math.max(1, java.lang.Runtime.getRuntime.availableProcessors())),
      Tags.limitAll(math.max(1, java.lang.Runtime.getRuntime.availableProcessors()))
    )
  )
