package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.TrialsScaffolding.Tuple4Trials
import com.sageserpent.americium.java.{CasesLimitStrategy, ConfiguredTrialsTest, TrialsScaffolding, TrialsTest}
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.FakeSources
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, Inspectors}
import org.scalatestplus.junit5.AssertionsForJUnit

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{FiniteDuration, SECONDS}

@TestInstance(Lifecycle.PER_CLASS)
class CodeMotionAnalysisTest
    extends AssertionsForJUnit
    with Matchers
    with Inspectors:
  val minimumSizeFractionTrials: Trials[Double] =
    trialsApi.doubles(0.1, 1)

  val sourcesTrials: Trials[FakeSources] =
    for
      textSize <- trialsApi.integers(0, 10000)
      textsByPath <- trialsApi
        .integers(1, 1000)
        .maps(
          trialsApi
            .characters(lowerBound = 'a', upperBound = 'z')
            .lotsOfSize(textSize)
        )
        .filter(_.nonEmpty)
    yield FakeSources(textsByPath)

  val configuredTrials: Tuple4Trials.SupplyToSyntaxTuple4[
    FakeSources,
    FakeSources,
    FakeSources,
    Double
  ] =
    (sourcesTrials.javaTrials and sourcesTrials.javaTrials and sourcesTrials.javaTrials and minimumSizeFractionTrials.javaTrials)
      .withLimit(100)

  @ConfiguredTrialsTest("configuredTrials")
  def sourcesCanBeReconstructedFromTheAnalysis(
      base: FakeSources,
      left: FakeSources,
      right: FakeSources,
      minimumSizeFraction: Double
  ): Unit =
    extension (results: Map[Int, File])
      private infix def matches(sources: FakeSources): Assertion =
        results.keys should be(sources.filesByPath.keys)

        forAll(results) { case (path, result) =>
          result.contents should be(sources.filesByPath(path).contents)
        }
      end matches
    end extension

    val Right(analysis: CodeMotionAnalysis[FakeSources#Path]) =
      CodeMotionAnalysis.of(base, left, right)(
        minimumSizeFraction
      ): @unchecked

    analysis.base matches base
    analysis.left matches left
    analysis.right matches right
  end sourcesCanBeReconstructedFromTheAnalysis

  // TODO - test *exact* matching of sections across *three* sources.
  // TODO - test *exact* matching of sections across *two* sources augmented
  // with a nominal match to bring in the 'missing' section.
  // Matches should be maximal in extent across three sources. This is subtle,
  // as a match may be extensible across just two sources, but the extension
  // won't work for the third sources.
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  case class FakeSources(textsByPath: Map[Int, String]) extends Sources[Int]:
    case class SectionImplementation(
        path: Path,
        override val startOffset: Int,
        override val size: Int
    ) extends Section:
      override def contents: String =
        textsByPath(path).substring(startOffset, onePastEndOffset)
    end SectionImplementation

    override def filesByPath: Map[Path, File] =
      textsByPath.map { case (path, text) =>
        path -> File(
          Vector(
            SectionImplementation(
              path = path,
              startOffset = 0,
              size = text.length
            )
          )
        )
      }

  end FakeSources

end CodeMotionAnalysisTest
