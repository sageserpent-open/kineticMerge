import sbtrelease.ReleaseStateTransformations.*
import xerial.sbt.Sonatype.*

import scala.language.postfixOps
import scala.sys.process.*
import scala.xml.transform.{RewriteRule, RuleTransformer}
import scala.xml.{Elem, Node}

enablePlugins(ShadingPlugin)

lazy val javaVersion = "14"

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.0"

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
    pomPostProcess := { node =>
      val rejectedDependencyArtifact =
        (rabinFingerprint / Compile / packageBin / artifact).value.name

      object removeRejectedDependencyArtifact extends RewriteRule {
        override def transform(subtreeNode: Node): Seq[Node] =
          subtreeNode match {
            case element: Elem if element.label == "dependency" =>
              if (
                element.child
                  .filter(_.label == "artifactId")
                  .exists(rejectedDependencyArtifact == _.text)
              ) Seq.empty
              else Seq(subtreeNode)
            case _ => Seq(subtreeNode)
          }
      }

      val transformer = new RuleTransformer(removeRejectedDependencyArtifact)

      transformer.transform(node).head
    },
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
    libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0",
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
    libraryDependencies += "com.softwaremill.common" %% "tagging" % "2.3.4",
    libraryDependencies += "com.google.guava" % "guava"      % "32.1.2-jre",
    libraryDependencies += "de.sciss"        %% "fingertree" % "1.5.5",
    libraryDependencies += "com.sageserpent" %% "americium"  % "1.17.0",
    libraryDependencies += "com.lihaoyi"     %% "pprint"     % "0.8.1" % Test,
    libraryDependencies += "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test,
    libraryDependencies += "net.aichler" % "jupiter-interface" % JupiterKeys.jupiterVersion.value % Test,
    Test / fork               := true,
    Test / testForkedParallel := true,
    Test / javaOptions ++= Seq("-Xms10G", "-Xmx10G"),
    shadingVerbose := true,
    shadedJars += (rabinFingerprint / Compile / packageBin).value,
    shadingRules ++= Seq(
      ShadingRule.moveUnder("org.rabinfingerprint", "shaded")
    ),
    validNamespaces ++= Set("com", "org", "shaded"),
    validEntries ++= Set("version.txt", "usage.txt"),
    packageBin := shadedPackageBin.value
  )
  // Just an optional dependency - otherwise Coursier will pick this up. It's
  // enough to allow IntelliJ to both import from SBT and run tests, given that
  // the Rabin fingerprint code is shaded into Kinetic Merge.
  .dependsOn(rabinFingerprint % Optional)

lazy val rabinFingerprint = (project in file("rabinfingerprint")).settings(
  publishMavenStyle                        := false,
  crossPaths                               := false,
  packageBin / publishArtifact             := false,
  packageSrc / publishArtifact             := false,
  packageDoc / publishArtifact             := false,
  libraryDependencies += "com.google.guava" % "guava" % "32.1.2-jre",
  libraryDependencies += "junit"            % "junit" % "4.13.2" % Test
)
