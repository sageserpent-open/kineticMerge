package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials.api as trialsApi
import com.sageserpent.americium.{Trials, TrialsApi}
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.{FakeSources, sourcesTrials}
import org.scalatest.Inspectors
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CodeMotionAnalysisTest extends AnyFlatSpec with Matchers with Inspectors:
  "sources" should "be reconstructed from the analysis" in:
    sourcesTrials.withLimit(10).supplyTo { sources =>
      val files = sources.filesByPath

      files.keys should be(sources.filesByPath.keys)

      forAll(files) { case (path, file) =>
        file.contents should be(sources.textsByPath(path))
      }
    }
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  val sourcesTrials: Trials[FakeSources] = trialsApi
    .integers(lowerBound = 1, upperBound = 20)
    .maps(trialsApi.characters(lowerBound = 'a', upperBound = 'z').several)
    .map(m => FakeSources(m.asInstanceOf[Map[Int, String]]))

  case class FakeSources(textsByPath: Map[Int, String]) extends Sources[Int]:
    override type Path = Int

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
