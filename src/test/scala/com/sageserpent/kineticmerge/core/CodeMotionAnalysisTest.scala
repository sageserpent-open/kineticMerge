package com.sageserpent.kineticmerge.core

import cats.collections.HashSet as CatsHashSet
import cats.derived.*
import com.sageserpent.kineticmerge.core.CodeMotionAnalysisTest.FakeSources
import org.scalatest.flatspec.AnyFlatSpec

class CodeMotionAnalysisTest extends AnyFlatSpec:
  "sources" should "be reconstructed from the analysis" in:
    val baseSources = FakeSources
end CodeMotionAnalysisTest

object CodeMotionAnalysisTest:
  case class FakeSources(textsByPath: Map[Int, String]) extends Sources[Int]:
    override def files: Set[File] =
      CatsHashSet
        .fromIterableOnce(textsByPath.map { case (path, text) =>
          File(
            path,
            Vector(
              new Section:
                override def startOffset: Int = 0

                override def width: Int = text.length

                override def contents: String =
                  textsByPath(path).substring(startOffset, onePastEndOffset)
            )
          )
        })
        .toSet
  end FakeSources

end CodeMotionAnalysisTest
