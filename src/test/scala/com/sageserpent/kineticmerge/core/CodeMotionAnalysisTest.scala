package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.FakeSources
import org.scalatest.flatspec.AnyFlatSpec

class CodeMotionAnalysisTest extends AnyFlatSpec:
  "sources" should "be reconstructed from the analysis" in:
    val baseSources = FakeSources
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
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
