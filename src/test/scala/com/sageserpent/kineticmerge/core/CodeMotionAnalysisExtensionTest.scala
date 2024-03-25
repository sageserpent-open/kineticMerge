package com.sageserpent.kineticmerge.core

import com.google.common.hash.Hashing
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtension.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisExtensionTest.FakePath
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.Token.tokens
import org.junit.jupiter.api.Assertions.fail
import org.junit.jupiter.api.{Test, TestFactory}
import pprint.*

import scala.util.Right

object CodeMotionAnalysisExtensionTest:
  type FakePath = String
end CodeMotionAnalysisExtensionTest

class CodeMotionAnalysisExtensionTest extends ProseExamples:
  @Test
  def issue23BugReproduction(): Unit =
    val minimumMatchSize                 = 4
    val thresholdSizeFractionForMatching = 0.1

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val tokenRegex = raw"(SAFE|INTRUDER|FIZZY|BANG|.)+?".r.anchored

    def stuntDoubleTokens(content: String): Vector[Token] = tokenRegex
      .findAllMatchIn(content)
      .map(_.group(1))
      .map(Token.Significant.apply)
      .toVector

    val baseSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionBase)),
      label = "base"
    )
    val leftSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionLeft)),
      label = "left"
    )
    val rightSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> stuntDoubleTokens(issue23BugReproductionRight)),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(
      minimumMatchSize = minimumMatchSize,
      thresholdSizeFractionForMatching = thresholdSizeFractionForMatching
    )(
      elementEquality = Token.equality,
      elementOrder = Token.comparison,
      elementFunnel = Token.funnel,
      hashFunction = Hashing.murmur3_32_fixed()
    ): @unchecked

    val expected = stuntDoubleTokens(issue23BugReproductionExpectedMerge)

    codeMotionAnalysis.mergeAt(placeholderPath)(equality = Token.equality) match
      case FullyMerged(result) =>
        assert(result.corresponds(expected)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(s"*** Left result...\n")
        println(leftResult)
        println()
        println(s"*** Right result...\n")
        println(rightResult)

        fail("Should have seen a clean merge.")
    end match

  end issue23BugReproduction

  @Test
  def codeMotion(): Unit =
    val minimumMatchSize                 = 4
    val thresholdSizeFractionForMatching = 0

    val placeholderPath: FakePath = "*** STUNT DOUBLE ***"

    val baseSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleBase).get),
      label = "base"
    )
    val leftSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleLeft).get),
      label = "left"
    )
    val rightSources = MappedContentSources(
      contentsByPath =
        Map(placeholderPath -> tokens(codeMotionExampleRight).get),
      label = "right"
    )

    val Right(codeMotionAnalysis) = CodeMotionAnalysis.of(
      base = baseSources,
      left = leftSources,
      right = rightSources
    )(
      minimumMatchSize = minimumMatchSize,
      thresholdSizeFractionForMatching = thresholdSizeFractionForMatching
    )(
      elementEquality = Token.equality,
      elementOrder = Token.comparison,
      elementFunnel = Token.funnel,
      hashFunction = Hashing.murmur3_32_fixed()
    ): @unchecked

    val expected = tokens(codeMotionExampleExpectedMerge).get

    codeMotionAnalysis.mergeAt(placeholderPath)(equality = Token.equality) match
      case FullyMerged(result) =>
        assert(result.corresponds(expected)(Token.equality))
      case MergedWithConflicts(leftResult, rightResult) =>
        println(s"*** Left result...\n")
        println(leftResult)
        println()
        println(s"*** Right result...\n")
        println(rightResult)

        fail("Should have seen a clean merge.")
    end match

  end codeMotion

  @TestFactory
  def merging(): DynamicTests =
    val minimumMatchSizes = Trials.api.integers(2, 10)

    minimumMatchSizes.withLimit(30).dynamicTests { minimumMatchSize =>
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
      )(
        minimumMatchSize = minimumMatchSize,
        thresholdSizeFractionForMatching = 0
      )(
        elementEquality = Token.equality,
        elementOrder = Token.comparison,
        elementFunnel = Token.funnel,
        hashFunction = Hashing.murmur3_32_fixed()
      ): @unchecked

      def merge(path: FakePath): Unit =
        codeMotionAnalysis
          .mergeAt(path)(equality = Token.equality) match
          case MergedWithConflicts(leftElements, rightElements) =>
            println(s"**** Conflicted merge at path: $path ****")
            pprintln(leftElements.map(_.text).mkString)
            println("*********************************")
            pprintln(rightElements.map(_.text).mkString)
          case FullyMerged(elements) =>
            println(s"**** Full merge at path: $path ****")
            pprintln(elements.map(_.text).mkString)
      end merge

      merge(prosePath)
      merge(sbtBuildPath)
    }
  end merging
end CodeMotionAnalysisExtensionTest

trait ProseExamples:
  protected val issue23BugReproductionBase: String =
    """
      |chipsSAFEketchupSAFEnoodlesFIZZYsandwichSAFEpudding
      |""".stripMargin

  protected val issue23BugReproductionLeft: String =
    """
      |chipsSAFEketchupSAFEnoodlesBANGsandwichSAFEpudding
      |""".stripMargin

  protected val issue23BugReproductionRight: String =
    """
      |chipsINTRUDERketchupINTRUDERnoodlesFIZZYsandwichINTRUDERpudding
      |""".stripMargin

  protected val issue23BugReproductionExpectedMerge: String =
    """
      |chipsINTRUDERketchupINTRUDERnoodlesBANGsandwichINTRUDERpudding
      |""".stripMargin

  protected val wordsworth: String =
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

  protected val jobsworth: String =
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

  protected val emsworth: String =
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

  protected val baseSbtBuild: String =
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

  protected val leftSbtBuild: String =
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

  protected val rightSbtBuild: String =
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

  protected val codeMotionExampleBase: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      | // Using Kinetic Merge will improve your software engineering practices...
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    override val showLocation: Boolean = true
      |    override val showTypes: Boolean    = /* TODO - remove this comment, it's here to force propagation of the edit on the right. */ true
      |  end assert
      |end ExpectyFlavouredAssert
      |""".stripMargin

  protected val codeMotionExampleLeft: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      | // Using Kinetic Merge will help you improvise in your software engineering experience...
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    // Swapped the next two lines around...
      |    override val showTypes: Boolean    = /* TODO - remove this comment, it's here to force propagation of the edit on the right. */ true
      |    override val showLocation: Boolean = true
      |
      |  end assert
      |end ExpectyFlavouredAssert
      |""".stripMargin

  protected val codeMotionExampleRight: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    override val showLocation: Boolean = true
      |    override val showTypes: Boolean    = false // This edit should propagate.
      |  end assert
      |end ExpectyFlavouredAssert
      |  // Using Kinetic Merge will improve your software engineering practices...
      |""".stripMargin

  protected val codeMotionExampleExpectedMerge: String =
    """
      |package com.sageserpent.kineticmerge.core
      |
      |import com.eed3si9n.expecty.Expecty
      |
      |object ExpectyFlavouredAssert:
      |  val assert: Expecty = new Expecty:
      |    // Swapped the next two lines around...
      |    override val showTypes: Boolean    = false // This edit should propagate.
      |    override val showLocation: Boolean = true
      |
      |  end assert
      |end ExpectyFlavouredAssert
      | // Using Kinetic Merge will help you improvise in your software engineering experience...
      |""".stripMargin
end ProseExamples
