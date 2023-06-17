package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.java.CasesLimitStrategy
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{FakeSources, sourcesTrials}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.concurrent.duration.{FiniteDuration, SECONDS}

class CodeMotionAnalysisTest extends AnyFlatSpec with Matchers with Inspectors:
  "sources" should "be reconstructed from the analysis" in:
    sourcesTrials
      .withStrategy(_ =>
        CasesLimitStrategy.timed(FiniteDuration.apply(10, SECONDS))
      )
      .supplyTo { sources =>
        val files = sources.filesByPath

        files.keys should be(sources.filesByPath.keys)

        forAll(files) { case (path, file) =>
          file.contents should be(sources.textsByPath(path))
        }
      }

    // TODO - test *exact* matching of sections across *three* sources.
    // TODO - test *exact* matching of sections across *two* sources augmented
    // with a nominal match to bring in the 'missing' section.
    // Matches should be maximal in extent across three sources. This is subtle,
    // as a match may be extensible across just two sources, but the extension
    // won't work for the third sources.
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
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
        override val width: Int
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
              width = text.length
            )
          )
        )
      }

  end FakeSources

end CodeMotionAnalysisTest
