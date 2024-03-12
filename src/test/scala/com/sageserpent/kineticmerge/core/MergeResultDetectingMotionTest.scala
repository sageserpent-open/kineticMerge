package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeResultDetectingMotionTest.Element
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{spy, verify}
import org.mockito.junit.jupiter.MockitoExtension

object MergeResultDetectingMotionTest:
  type Element = Int
end MergeResultDetectingMotionTest

@ExtendWith(Array(classOf[MockitoExtension]))
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

    val coreMergeAlgebra = spy(MergeResult.mergeAlgebra[Element])

    val mergeAlgebra =
      MergeResultDetectingMotion.mergeAlgebra(matchesFor, coreMergeAlgebra)

    val mergeResult =
      mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

    verify(coreMergeAlgebra).leftDeletion(
      any(),
      ArgumentMatchers.eq(baseElement)
    )

    assert(None == mergeResult.changesPropagatedThroughMotion(leftElement))
  end baseAndLeftMatchWhenMergeDoesNotAlignTheTwoMatchingSections
end MergeResultDetectingMotionTest
