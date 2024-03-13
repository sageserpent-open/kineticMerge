package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.{DynamicTests, given}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeResultDetectingMotionTest.{Element, mockCoreMergeAlgebra}
import org.junit.jupiter.api.TestFactory
import org.junit.jupiter.api.extension.ExtendWith
import org.mockito.ArgumentMatchers
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{RETURNS_SELF, mock, verify}
import org.mockito.junit.jupiter.MockitoExtension

object MergeResultDetectingMotionTest:
  type Element = Int

  // NOTE: yes, this is imperative mocking in a pure-functional context, but try
  // writing an auditing merge algebra and getting it to plug into
  // `MergeResultDetectingMotion`. Then cut over the test suite and decide
  // whether it was worth it...
  def mockCoreMergeAlgebra() =
    mock(classOf[merge.MergeAlgebra[MergeResult, Element]], RETURNS_SELF)
end MergeResultDetectingMotionTest

@ExtendWith(Array(classOf[MockitoExtension]))
class MergeResultDetectingMotionTest:
  @TestFactory
  def pairwiseMatchWhenMergeDoesNotAlignTheTwoMatchingSections: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element    = 1
      val ourSideElement: Element = 2

      val baseAndOurSidePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseElement,
              rightElement = ourSideElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseElement,
              leftElement = ourSideElement
            )

      val matchesByElement =
        Map(
          baseElement    -> baseAndOurSidePairwiseMatch,
          ourSideElement -> baseAndOurSidePairwiseMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val coreMergeAlgebra = mockCoreMergeAlgebra()

      val mergeAlgebra =
        MergeResultDetectingMotion.mergeAlgebra(matchesFor, coreMergeAlgebra)

      val mergeResult =
        mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

      verify(coreMergeAlgebra).coincidentDeletion(
        any(),
        ArgumentMatchers.eq(baseElement)
      )

      assert(None == mergeResult.changesPropagatedThroughMotion(ourSideElement))
    }
  end pairwiseMatchWhenMergeDoesNotAlignTheTwoMatchingSections
end MergeResultDetectingMotionTest
