package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.junit5.ConfiguredTrialsTest
import com.sageserpent.americium.java.{CasesLimitStrategy, TrialsScaffolding as JavaTrialsScaffolding}
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi, TrialsScaffolding}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.FakeSources
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import org.junit.jupiter.api.*
import org.junit.jupiter.api.DynamicTest.dynamicTest
import org.junit.jupiter.api.TestInstance.Lifecycle
import org.opentest4j.TestAbortedException

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{FiniteDuration, SECONDS}
import scala.jdk.CollectionConverters.*

class CodeMotionAnalysisTest:
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

  @TestFactory
  def sourcesCanBeReconstructedFromTheAnalysis: DynamicTests =
    extension (results: Map[Int, File])
      private infix def matches(sources: FakeSources): Unit =
        assert(results.keys == sources.filesByPath.keys)

        results.foreach { case (path, result) =>
          assert(result.contents == sources.filesByPath(path).contents)
        }
      end matches
    end extension

    (sourcesTrials and sourcesTrials and sourcesTrials and minimumSizeFractionTrials)
      .withLimit(100)
      .dynamicTests(
        (
            base: FakeSources,
            left: FakeSources,
            right: FakeSources,
            minimumSizeFraction: Double
        ) =>
          val Right(analysis: CodeMotionAnalysis[FakeSources#Path]) =
            CodeMotionAnalysis.of(base, left, right)(
              minimumSizeFraction
            ): @unchecked

          analysis.base matches base
          analysis.left matches left
          analysis.right matches right
      )
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

    case class SectionImplementation(
        path: Path,
        override val startOffset: Int,
        override val size: Int
    ) extends Section:
      override def contents: String =
        textsByPath(path).substring(startOffset, onePastEndOffset)
    end SectionImplementation

  end FakeSources

end CodeMotionAnalysisTest
