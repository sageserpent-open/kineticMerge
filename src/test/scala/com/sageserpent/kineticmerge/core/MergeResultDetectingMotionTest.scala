package com.sageserpent.kineticmerge.core

import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.{DynamicTests, given}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MergeResultDetectingMotionTest.Operation.*
import com.sageserpent.kineticmerge.core.MergeResultDetectingMotionTest.{Element, auditingCoreMergeAlgebra}
import org.junit.jupiter.api.TestFactory

object MergeResultDetectingMotionTest:
  type Element  = Int
  type Audit[X] = Vector[Operation[X]]

  enum Operation[X]:
    case Preservation(preserved: X)
    case LeftInsertion(inserted: X)
    case RightInsertion(inserted: X)
    case CoincidentInsertion(inserted: X)
    case LeftDeletion(deleted: X)
    case RightDeletion(deleted: X)
    case CoincidentDeletion(deleted: X)
    case LeftEdit(edited: X, edits: IndexedSeq[X])
    case RightEdit(edited: X, edits: IndexedSeq[X])
    case CoincidentEdit(edited: X, edits: IndexedSeq[X])
    case Conflict(
        edited: IndexedSeq[X],
        leftEdits: IndexedSeq[X],
        rightEdits: IndexedSeq[X]
    )

  end Operation

  object auditingCoreMergeAlgebra extends merge.MergeAlgebra[Audit, Element]:
    override def empty: Audit[Element] = Vector.empty
    override def preservation(
        result: Audit[Element],
        preservedElement: Element
    ): Audit[Element] = result :+ Preservation(preservedElement)
    override def leftInsertion(
        result: Audit[Element],
        insertedElement: Element
    ): Audit[Element] = result :+ LeftInsertion(insertedElement)
    override def rightInsertion(
        result: Audit[Element],
        insertedElement: Element
    ): Audit[Element] = result :+ RightInsertion(insertedElement)
    override def coincidentInsertion(
        result: Audit[Element],
        insertedElement: Element
    ): Audit[Element] = result :+ CoincidentInsertion(insertedElement)
    override def leftDeletion(
        result: Audit[Element],
        deletedElement: Element
    ): Audit[Element] = result :+ LeftDeletion(deletedElement)
    override def rightDeletion(
        result: Audit[Element],
        deletedElement: Element
    ): Audit[Element] = result :+ RightDeletion(deletedElement)
    override def coincidentDeletion(
        result: Audit[Element],
        deletedElement: Element
    ): Audit[Element] = result :+ CoincidentDeletion(deletedElement)
    override def leftEdit(
        result: Audit[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Audit[Element] = result :+ LeftEdit(editedElement, editElements)
    override def rightEdit(
        result: Audit[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Audit[Element] = result :+ RightEdit(editedElement, editElements)
    override def coincidentEdit(
        result: Audit[Element],
        editedElement: Element,
        editElements: IndexedSeq[Element]
    ): Audit[Element] = result :+ CoincidentEdit(editedElement, editElements)
    override def conflict(
        result: Audit[Element],
        editedElements: IndexedSeq[Element],
        leftEditElements: IndexedSeq[Element],
        rightEditElements: IndexedSeq[Element]
    ): Audit[Element] =
      result :+ Conflict(editedElements, leftEditElements, rightEditElements)
  end auditingCoreMergeAlgebra

end MergeResultDetectingMotionTest

class MergeResultDetectingMotionTest:
  @TestFactory
  def coincidentDeletionWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2

      val baseAndOurSidePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseElement,
              rightElement = ourSideMovedElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseElement,
              leftElement = ourSideMovedElement
            )

      val matchesByElement =
        Map(
          baseElement         -> baseAndOurSidePairwiseMatch,
          ourSideMovedElement -> baseAndOurSidePairwiseMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val mergeAlgebra =
        MergeResultDetectingMotion
          .mergeAlgebra(matchesFor, auditingCoreMergeAlgebra)

      val mergeResult =
        mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

      assert(
        Vector(CoincidentDeletion(baseElement)) == mergeResult.coreMergeResult
      )

      assert(
        None == mergeResult.changesPropagatedThroughMotion(ourSideMovedElement)
      )
    }
  end coincidentDeletionWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val theirSideElement: Element    = 3

      val baseAndOurSidePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseElement,
              rightElement = ourSideMovedElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseElement,
              leftElement = ourSideMovedElement
            )

      val matchesByElement =
        Map(
          baseElement         -> baseAndOurSidePairwiseMatch,
          ourSideMovedElement -> baseAndOurSidePairwiseMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val mergeAlgebra =
        MergeResultDetectingMotion
          .mergeAlgebra(matchesFor, auditingCoreMergeAlgebra)

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq(theirSideElement),
            rightEditElements = IndexedSeq.empty
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(theirSideElement)
          )

      assert(
        Vector(CoincidentDeletion(baseElement)) == mergeResult.coreMergeResult
      )

      assert(
        Some(theirSideElement) == mergeResult
          .changesPropagatedThroughMotion(ourSideMovedElement)
      )
    }
  end ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

end MergeResultDetectingMotionTest
