package com.sageserpent.kineticmerge.core

import com.google.common.hash.Hashing
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.Token.tokens
import com.sageserpent.kineticmerge.core.merge.{FullyMerged, MergedWithConflicts}
import org.junit.jupiter.api.TestFactory
import pprint.*

class CodeMotionAnalysisExtensionTest extends ProseExamples:
  @TestFactory
  def proseCanBeMerged(): DynamicTests =
    val thresholds = Trials.api.doubles(0.01, 1)

    thresholds.withLimit(10).dynamicTests { threshold =>
      type FakePath = String

      val prosePath: FakePath    = "prose"
      val sbtBuildPath: FakePath = "sbtBuild"

      val baseSources =
        MappedContentSources(
          contentsByPath = Map(
            prosePath    -> tokens(wordsworth).get,
            sbtBuildPath -> tokens(baseSbtBuild).get
          ),
          label = "base"
        )
      val leftSources =
        MappedContentSources(
          contentsByPath = Map(
            prosePath    -> tokens(jobsworth).get,
            sbtBuildPath -> tokens(leftSbtBuild).get
          ),
          label = "left"
        )
      val rightSources =
        MappedContentSources(
          contentsByPath = Map(
            prosePath    -> tokens(emsworth).get,
            sbtBuildPath -> tokens(rightSbtBuild).get
          ),
          label = "right"
        )

      val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
        base = baseSources,
        left = leftSources,
        right = rightSources
      )(minimumSizeFractionForMotionDetection = threshold)(
        elementEquality = Token.equality,
        elementOrder = Token.comparison,
        elementFunnel = Token.funnel,
        hashFunction = Hashing.murmur3_32_fixed()
      ): @unchecked

      def merge(path: FakePath): Unit =
        codeMotionAnalysis
          .mergeAt(path)(equality = Token.equality) match
          case Right(MergedWithConflicts(leftElements, rightElements)) =>
            println(s"**** Conflicted merge at path: $path ****")
            pprintln(leftElements.map(_.text).mkString)
            println("*********************************")
            pprintln(rightElements.map(_.text).mkString)
          case Right(FullyMerged(elements)) =>
            println(s"**** Full merge at path: $path ****")
            pprintln(elements.map(_.text).mkString)
      end merge

      merge(prosePath)
      merge(sbtBuildPath)
    }
  end proseCanBeMerged
end CodeMotionAnalysisExtensionTest

trait ProseExamples:
  protected val wordsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |Continuous as the stars that shine
      |And twinkle on the milky way,
      |They stretched in never-ending line
      |Along the margin of a bay:
      |Ten thousand saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And dances with the daffodils.
      |""".stripMargin

  protected val jobsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of golden daffodils;
      |Beside the lake, beneath the trees,
      |Fluttering and dancing in the breeze.
      |
      |I thought, 'Was this part of the job role?'.
      |'Should I be expected to deal with flowers?'
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but thought only of
      |raising this in the next Zoom meeting.
      |
      |For oft, when on my Aeron I slouch
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sends an email to human resources.
      |""".stripMargin

  protected val emsworth =
    """
      |I wandered lonely as a cloud
      |That floats on high o'er vales and hills,
      |When all at once I saw a crowd,
      |A host, of small fishing boats;
      |Astride the sea, beneath the quay,
      |Rocking and swaying in the breeze.
      |
      |Why this allusion?
      |I Havant a clue!
      |Along the margin of a bay:
      |Ten thousand (well, maybe not quite) saw I at a glance,
      |Tossing their heads in sprightly dance.
      |
      |The waves beside them danced; but they
      |Out-did the sparkling waves in glee:
      |A poet could not but be gay,
      |In such a jocund company:
      |I gazed—and gazed—but little thought
      |What wealth the show to me had brought:
      |
      |For oft, when on my couch I lie
      |In vacant or in pensive mood,
      |They flash upon that inward eye
      |Which is the bliss of solitude;
      |And then my heart with pleasure fills,
      |And sashays with the fishing boats.
      |""".stripMargin

  protected val baseSbtBuild =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommandAndRemaining(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      releaseStepCommand("packageExecutable"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    packageExecutable := {
      |      val _ = publishLocal.value; (rabinFingerprint / publishLocal).value
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin

  protected val leftSbtBuild =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |import scala.xml.transform.{RuleTransformer, RewriteRule}
      |import scala.xml.{Node, Elem}
      |
      |enablePlugins(ShadingPlugin)
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val versionResource =
      |  settingKey[File]("Location of generated version resource file.")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    pomPostProcess := { node =>
      |      val rejectedDependencyArtifact =
      |        (rabinFingerprint / Compile / packageBin / artifact).value.name
      |
      |      object removeRejectedDependencyArtifact extends RewriteRule {
      |        override def transform(subtreeNode: Node): Seq[Node] =
      |          subtreeNode match {
      |            case element: Elem if element.label == "dependency" =>
      |              if (
      |                element.child
      |                  .filter(_.label == "artifactId")
      |                  .exists(rejectedDependencyArtifact == _.text)
      |              ) Seq.empty
      |              else Seq(subtreeNode)
      |            case _ => Seq(subtreeNode)
      |          }
      |      }
      |
      |      val transformer = new RuleTransformer(removeRejectedDependencyArtifact)
      |
      |      transformer.transform(node).head
      |    },
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommand("packageExecutable"),
      |      releaseStepCommand(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    versionResource := {
      |      val additionalResourcesDirectory = (Compile / resourceManaged).value
      |
      |      additionalResourcesDirectory.toPath.resolve("version.txt").toFile
      |    },
      |    Compile / resourceGenerators += Def.task {
      |      val location = versionResource.value
      |
      |      val packagingVersion = (ThisBuild / version).value
      |
      |      println(
      |        s"Generating version resource: $location for version: $packagingVersion"
      |      )
      |
      |      IO.write(location, packagingVersion)
      |
      |      Seq(location)
      |    }.taskValue,
      |    packageExecutable := {
      |      val packagingVersion = (ThisBuild / version).value
      |
      |      println(s"Packaging executable with version: $packagingVersion")
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:$packagingVersion"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin

  protected val rightSbtBuild =
    """
      |import scala.sys.process.*
      |import scala.language.postfixOps
      |import sbtrelease.ReleaseStateTransformations.*
      |import xerial.sbt.Sonatype.*
      |
      |lazy val javaVersion = "14"
      |
      |ThisBuild / version := "0.1.0-SNAPSHOT"
      |
      |ThisBuild / scalaVersion := "3.3.0"
      |
      |ThisBuild / javacOptions ++= Seq("-source", javaVersion, "-target", javaVersion)
      |
      |lazy val packageExecutable =
      |  taskKey[String]("Package an executable with Coursier")
      |
      |lazy val root = (project in file("."))
      |  .settings(
      |    publishTo              := sonatypePublishToBundle.value,
      |    pomIncludeRepository   := { _ => false },
      |    sonatypeCredentialHost := "s01.oss.sonatype.org",
      |    publishMavenStyle      := true,
      |    licenses += ("MIT", url("https://opensource.org/licenses/MIT")),
      |    organization     := "com.sageserpent",
      |    organizationName := "sageserpent",
      |    description := "Merge branches in the presence of code motion within and between files.",
      |    sonatypeProjectHosting := Some(
      |      GitHubHosting(
      |        user = "sageserpent-open",
      |        repository = "kineticMerge",
      |        email = "gjmurphy1@icloud.com"
      |      )
      |    ),
      |    releaseCrossBuild := false, // No cross-building here - just Scala 3.
      |    releaseProcess := Seq[ReleaseStep](
      |      checkSnapshotDependencies,
      |      inquireVersions,
      |      runClean,
      |      runTest,
      |      setReleaseVersion,
      |      commitReleaseVersion,
      |      tagRelease,
      |      releaseStepCommandAndRemaining(
      |        "publishSigned"
      |      ), // ... finally the publishing step using SBT's own mechanism.
      |      releaseStepCommand("sonatypeBundleRelease"),
      |      releaseStepCommand("packageExecutable"),
      |      setNextVersion,
      |      commitNextVersion,
      |      pushChanges
      |    ),
      |    scalacOptions ++= List("-source:future"),
      |    name := "kinetic-merge",
      |    packageExecutable := {
      |      val _ = publishLocal.value
      |
      |      val localArtifactCoordinates =
      |        s"${organization.value}:${name.value}_${scalaBinaryVersion.value}:${version.value}"
      |
      |      val executablePath = s"${target.value}${Path.sep}${name.value}"
      |
      |      s"cs bootstrap --verbose --bat=true --scala-version ${scalaBinaryVersion.value} -f $localArtifactCoordinates -o $executablePath" !
      |
      |      name.value
      |    })
      |""".stripMargin
end ProseExamples