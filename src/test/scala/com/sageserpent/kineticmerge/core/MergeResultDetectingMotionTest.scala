package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeResultDetectingMotionTest.Element
import org.junit.jupiter.api.Test

object MergeResultDetectingMotionTest:
  type Element = Int
end MergeResultDetectingMotionTest

class MergeResultDetectingMotionTest:
  @Test
  def baseAndLeftMatchWhenMergeDoesNotAlignTheTwoMatchingSections: Unit =
    val baseElement: Element = 1
    val leftElement: Element = 2

    val baseAndLeft = Match.BaseAndLeft(baseElement, leftElement)

    val matchesByElement =
      Map(baseElement -> baseAndLeft, leftElement -> baseAndLeft)

    def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
      .get(element)
      .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

    val mergeAlgebra = MergeResultDetectingMotion.mergeAlgebra(matchesFor)

    val mergeResult =
      mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

    assert(
      FullyMerged(elements = IndexedSeq.empty) == mergeResult.coreMergeResult
    )

    assert(None == mergeResult.changesPropagatedThroughMotion(leftElement))
  end baseAndLeftMatchWhenMergeDoesNotAlignTheTwoMatchingSections
end MergeResultDetectingMotionTest
