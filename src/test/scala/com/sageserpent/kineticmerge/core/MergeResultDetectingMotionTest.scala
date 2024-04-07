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
        Set(None) == mergeResult.changesPropagatedThroughMotion
          .get(ourSideMovedElement)
      )
    }
  end coincidentDeletionWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element          = 1
      val ourSideMovedElement: Element  = 2
      val theirSideEditElement: Element = 3

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
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq.empty
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      assert(
        Vector(CoincidentDeletion(baseElement)) == mergeResult.coreMergeResult
      )

      assert(
        Set(
          Some(theirSideEditElement)
        ) == mergeResult.changesPropagatedThroughMotion.get(ourSideMovedElement)
      )
    }
  end ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val ourSideEditElement: Element  = 3

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
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq.empty
          )

      // NOTE: when a merge algebra is driven by `merge.of`, the following
      // sequences of operations would not be permitted. It's OK in this
      // situation though - the front end algebra should be driven correctly,
      // and the core algebra is expected to have its operations translated to
      // take motion into account. We could translate to a right- or left-edit,
      // but this feels wrong - there never was an element on the opposite side
      // of the edit, that was moved out of the way instead.
      if mirrorImage then
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            RightInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            LeftInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Set(None) == mergeResult.changesPropagatedThroughMotion
          .get(ourSideMovedElement)
      )
    }
  end ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element          = 1
      val ourSideMovedElement: Element  = 2
      val ourSideEditElement: Element   = 3
      val theirSideEditElement: Element = 4

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
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      // NOTE: when a merge algebra is driven by `merge.of`, the following
      // sequences of operations would not be permitted. It's OK in this
      // situation though - the front end algebra should be driven correctly,
      // and the core algebra is expected to have its operations translated to
      // take motion into account. We could translate to a right- or left-edit,
      // but this feels wrong - there never was an element on the opposite side
      // of the edit, that was moved out of the way instead.
      if mirrorImage then
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            RightInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            LeftInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Set(
          Some(theirSideEditElement)
        ) == mergeResult.changesPropagatedThroughMotion.get(ourSideMovedElement)
      )
    }
  end ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentEditWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element           = 1
      val ourSideMovedElement: Element   = 2
      val coincidentEditElement: Element = 3

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
        mergeAlgebra.coincidentEdit(
          mergeAlgebra.empty,
          baseElement,
          IndexedSeq(coincidentEditElement)
        )

      // The reasoning here is that a coincident edit, regardless of whether it
      // is part of a match or not, should stand as it is and not have one side
      // considered as an edit of the moved element.
      assert(
        Vector(
          CoincidentEdit(baseElement, IndexedSeq(coincidentEditElement))
        ) == mergeResult.coreMergeResult
      )

      assert(
        Set(None) == mergeResult.changesPropagatedThroughMotion
          .get(ourSideMovedElement)
      )
    }
  end coincidentEditWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourSideDeletionWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val theirSideElement: Element    = 3

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideElement
          )

      val matchesByElement =
        Map(
          baseElement         -> allSidesMatch,
          ourSideMovedElement -> allSidesMatch,
          theirSideElement    -> allSidesMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val mergeAlgebra =
        MergeResultDetectingMotion
          .mergeAlgebra(matchesFor, auditingCoreMergeAlgebra)

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.rightDeletion(mergeAlgebra.empty, baseElement)
        else mergeAlgebra.leftDeletion(mergeAlgebra.empty, baseElement)

      if mirrorImage then
        assert(
          Vector(RightDeletion(baseElement)) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(LeftDeletion(baseElement)) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesPropagatedThroughMotion
          .containsKey(ourSideMovedElement)
      )
    }
  end ourSideDeletionWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourSideEditWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val ourSideEditElement: Element  = 3
      val theirSideElement: Element    = 4

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideElement
          )

      val matchesByElement =
        Map(
          baseElement         -> allSidesMatch,
          ourSideMovedElement -> allSidesMatch,
          theirSideElement    -> allSidesMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val mergeAlgebra =
        MergeResultDetectingMotion
          .mergeAlgebra(matchesFor, auditingCoreMergeAlgebra)

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.rightEdit(
            mergeAlgebra.empty,
            baseElement,
            IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.leftEdit(
            mergeAlgebra.empty,
            baseElement,
            IndexedSeq(ourSideEditElement)
          )

      if mirrorImage then
        assert(
          Vector(
            RightEdit(baseElement, IndexedSeq(ourSideEditElement))
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            LeftEdit(baseElement, IndexedSeq(ourSideEditElement))
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesPropagatedThroughMotion
          .containsKey(ourSideMovedElement)
      )
    }
  end ourSideEditWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentDeletionWhereBaseHasMovedAwayOnBothSides: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element           = 1
      val ourSideMovedElement: Element   = 2
      val theirSideMovedElement: Element = 3

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          baseElement           -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
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
        Set(None) == mergeResult.changesPropagatedThroughMotion
          .get(ourSideMovedElement)
      )
      assert(
        Set(None) == mergeResult.changesPropagatedThroughMotion
          .get(theirSideMovedElement)
      )
    }
  end coincidentDeletionWhereBaseHasMovedAwayOnBothSides

  @TestFactory
  def ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnBothSides
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element           = 1
      val ourSideMovedElement: Element   = 2
      val theirSideMovedElement: Element = 3
      val ourSideEditElement: Element    = 4

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          baseElement           -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
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
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq.empty
          )

      // NOTE: when a merge algebra is driven by `merge.of`, the following
      // sequences of operations would not be permitted. It's OK in this
      // situation though - the front end algebra should be driven correctly,
      // and the core algebra is expected to have its operations translated to
      // take motion into account. We could translate to a right- or left-edit,
      // but this feels wrong - there never was an element on the opposite side
      // of the edit, that was moved out of the way instead.
      if mirrorImage then
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            RightInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseElement),
            LeftInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesPropagatedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        Set(
          Some(ourSideEditElement)
        ) == mergeResult.changesPropagatedThroughMotion
          .get(theirSideMovedElement)
      )
    }
  end ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnBothSides

  @TestFactory
  def ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnBothSides
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element           = 1
      val ourSideMovedElement: Element   = 2
      val theirSideMovedElement: Element = 3
      val ourSideEditElement: Element    = 4
      val theirSideEditElement: Element  = 5

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          baseElement           -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
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
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      if mirrorImage then
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseElement),
              IndexedSeq(theirSideEditElement),
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseElement),
              IndexedSeq(ourSideEditElement),
              IndexedSeq(theirSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Set(
          Some(theirSideEditElement)
        ) == mergeResult.changesPropagatedThroughMotion.get(ourSideMovedElement)
      )
      assert(
        Set(
          Some(ourSideEditElement)
        ) == mergeResult.changesPropagatedThroughMotion
          .get(theirSideMovedElement)
      )
    }
  end ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnBothSides

  @TestFactory
  def coincidentInsertionOfMovesOnBothSides: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element           = 1
      val ourSideMovedElement: Element   = 2
      val theirSideMovedElement: Element = 3

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          baseElement           -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
        )

      def matchesFor(element: Element): Set[Match[Element]] = matchesByElement
        .get(element)
        .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

      val mergeAlgebra =
        MergeResultDetectingMotion
          .mergeAlgebra(matchesFor, auditingCoreMergeAlgebra)

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.coincidentInsertion(
            mergeAlgebra.empty,
            theirSideMovedElement
          )
        else
          mergeAlgebra.coincidentInsertion(
            mergeAlgebra.empty,
            ourSideMovedElement
          )

      if mirrorImage then
        assert(
          Vector(
            CoincidentInsertion(theirSideMovedElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentInsertion(ourSideMovedElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesPropagatedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesPropagatedThroughMotion
          .containsKey(theirSideMovedElement)
      )
    }
  end coincidentInsertionOfMovesOnBothSides

end MergeResultDetectingMotionTest
