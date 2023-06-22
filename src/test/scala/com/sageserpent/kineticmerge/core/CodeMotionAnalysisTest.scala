package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{FakeSources, minimumSizeFractionTrials, sourcesTrials}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class CodeMotionAnalysisTest extends AnyFlatSpec with Matchers with Inspectors:
  "sources" should "be reconstructed from the analysis" in:
    class verify(results: Map[Int, File]):
      def against(sources: FakeSources): Unit =
        results.keys should be(sources.filesByPath.keys)

        forAll(results) { case (path, result) =>
          result.contents should be(sources.filesByPath(path).contents)
        }
      end against
    end verify

    (sourcesTrials and sourcesTrials and sourcesTrials and minimumSizeFractionTrials)
      .withLimit(100)
      .supplyTo:
        case tuple @ (
              base: FakeSources,
              left: FakeSources,
              right: FakeSources,
              minimumSizeFraction: Double
            ) =>
          println(tuple)

          val Right(analysis: CodeMotionAnalysis[FakeSources#Path]) =
            CodeMotionAnalysis.of(base, left, right)(
              minimumSizeFraction
            ): @unchecked

          verify(analysis.base).against(base)
          verify(analysis.left).against(left)
          verify(analysis.right).against(right)

  // TODO - test *exact* matching of sections across *three* sources.
  // TODO - test *exact* matching of sections across *two* sources augmented
  // with a nominal match to bring in the 'missing' section.
  // Matches should be maximal in extent across three sources. This is subtle,
  // as a match may be extensible across just two sources, but the extension
  // won't work for the third sources.
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
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
