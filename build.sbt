import scala.sys.process.*
import scala.language.postfixOps
import sbtrelease.ReleaseStateTransformations.*
import xerial.sbt.Sonatype.*

enablePlugins(ShadingPlugin)

lazy val javaVersion = "14"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)

lazy val packageExecutable =
  taskKey[String]("Package an executable with Coursier")

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
      releaseStepCommandAndRemaining(
        "publishSigned"
      ), // ... finally the publishing step using SBT's own mechanism.
      releaseStepCommand("sonatypeBundleRelease"),
      releaseStepCommand("packageExecutable"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    ),
    scalacOptions ++= List("-source:future"),
    name := "kinetic-merge",
    packageExecutable := {
      val _ = publishLocal.value

      val localArtifactCoordinates =
        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"

      val executablePath = s"${target.value}${Path.sep}${name.value}"

      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !

      name.value
    },
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
    libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.6",
    libraryDependencies += "org.typelevel" %% "cats-core"   % "2.10.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2",
    libraryDependencies ++= Seq(
      "dev.optics" %% "monocle-core"  % "3.2.0",
      "dev.optics" %% "monocle-macro" % "3.2.0"
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
    libraryDependencies += "com.lihaoyi"             %% "fansi"   % "0.4.0",
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.4",
    libraryDependencies += "com.sageserpent" %% "americium" % "1.16.0" % Test,
    libraryDependencies += "com.lihaoyi"     %% "pprint"    % "0.8.1"  % Test,
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test,
    libraryDependencies += "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    // NASTY HACK: instead of an explicit inter-project dependency via
    // `.dependsOn`, use this to stop both Coursier from pulling in the JAR from
    // `rabinFingerprint` *and* also the generated POM from referencing said
    // dependency.
    Compile / internalDependencyClasspath ++= (rabinFingerprint / Compile / exportedProducts).value ++ (rabinFingerprint / Compile / externalDependencyClasspath).value,
    Test / fork               := true,
    Test / testForkedParallel := true,
    Test / javaOptions ++= Seq("-Xms10G", "-Xmx10G"),
    shadingVerbose := true,
    shadedJars += (rabinFingerprint / Compile / packageBin).value,
    shadingRules ++= Seq(
      ShadingRule.moveUnder("org.rabinfingerprint", "shaded")
    ),
    validNamespaces ++= Set("com", "org", "shaded"),
    validEntries += "usage.txt",
    packageBin := shadedPackageBin.value
  )

lazy val rabinFingerprint = (project in file("rabinfingerprint")).settings(
  publishMavenStyle                        := false,
  crossPaths                               := false,
  packageBin / publishArtifact             := false,
  packageSrc / publishArtifact             := false,
  packageDoc / publishArtifact             := false,
  libraryDependencies += "com.google.guava" % "guava" % "32.1.2-jre",
  libraryDependencies += "junit"            % "junit" % "4.13.2" % Test
)
