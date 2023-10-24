package com.sageserpent.kineticmerge.core

import com.google.common.hash.{Hashing, PrimitiveSink}
import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.junit5.ConfiguredTrialsTest
import com.sageserpent.americium.java.{CasesLimitStrategy, TrialsScaffolding as JavaTrialsScaffolding}
import com.sageserpent.americium.junit5.*
import com.sageserpent.americium.{Trials, TrialsApi, TrialsScaffolding}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{FakeSources, funnel}
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
            .integers(lowerBound = 1, upperBound = 20)
            .lotsOfSize[Vector[Int]](textSize)
        )
        .filter(_.nonEmpty)
    yield FakeSources(textsByPath)

  @TestFactory
  def sourcesCanBeReconstructedFromTheAnalysis: DynamicTests =
    extension (results: Map[FakeSources#Path, File[FakeSources#Element]])
      private infix def matches(sources: FakeSources): Unit =
        assert(results.keys == sources.filesByPath.keys)

        results.foreach { case (path, result) =>
          assert(result.content == sources.filesByPath(path).content)
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
          val Right(
            analysis: CodeMotionAnalysis[FakeSources#Path, FakeSources#Element]
          ) =
            CodeMotionAnalysis.of(base, left, right)(
              minimumSizeFraction
            )(
              equality = _ == _,
              hashFunction = Hashing.murmur3_32_fixed(),
              funnel = funnel
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
  type Path    = Int
  type Element = Int

  private def funnel(element: Int, primitiveSink: PrimitiveSink): Unit =
    primitiveSink.putInt(element)
  end funnel

  case class FakeSources(textsByPath: Map[Path, Vector[Element]])
      extends Sources[Path, Element]:
    override def filesByPath: Map[Path, File[Element]] =
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
    ) extends Section[Element]:
      override def content: IndexedSeq[Element] =
        textsByPath(path).slice(startOffset, onePastEndOffset)
    end SectionImplementation

  end FakeSources

end CodeMotionAnalysisTest
