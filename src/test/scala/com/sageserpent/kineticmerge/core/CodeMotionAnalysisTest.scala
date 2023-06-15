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
  case class FakeSources(textsByPath: Map[Int, String]) extends Sources:
    override type Path = Int

    case class SectionImplementation(
        override val path: Path,
        override val startOffset: Int,
        override val width: Int
    ) extends this.Section:
      override def contents: String =
        textsByPath(path).substring(startOffset, onePastEndOffset)
    end SectionImplementation

    override type SectionType = SectionImplementation

    override def files: Set[File] =
      CatsHashSet
        .fromIterableOnce(textsByPath.map { case (path, text) =>
          File(
            path,
            Vector(
              SectionImplementation(
                path = path,
                startOffset = 0,
                width = text.length
              )
            )
          )
        })
        .toSet
  end FakeSources

end CodeMotionAnalysisTest
