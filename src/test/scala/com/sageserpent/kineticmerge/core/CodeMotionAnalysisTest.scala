package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.FakeSources
import org.scalatest.flatspec.AnyFlatSpec

class CodeMotionAnalysisTest extends AnyFlatSpec:
  "sources" should "be reconstructed from the analysis" in:
    val baseSources = FakeSources
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  case class FakeSources(textsByPath: Map[Int, String]) extends Sources[Int]:
    override def filesByPath: Map[Path, File] =
      textsByPath.map { case (path, text) =>
        path -> File(
          Vector(
            new Section:
              override def startOffset: Int = 0

              override def width: Int = text.length

              override def contents: String =
                textsByPath(path).substring(startOffset, onePastEndOffset)
          )
        )
      }

  end FakeSources

end CodeMotionAnalysisTest
