package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.americium.Trials
import com.sageserpent.americium.junit5.{DynamicTests, given}
import com.sageserpent.kineticmerge.core.ExpectyFlavouredAssert.assert
import com.sageserpent.kineticmerge.core.MatchesContextTest.*
import com.sageserpent.kineticmerge.core.MatchesContextTest.Operation.*
import com.sageserpent.kineticmerge.core.MatchesContextTest.ResolutionOutcome.SomethingElseChosen
import com.sageserpent.kineticmerge.core.ResolutionContracts.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import org.junit.jupiter.api.{Test, TestFactory}

// NOTE: the majority of the tests use `guardedCoinFlippingResolution` as a stub; this emphasizes
// the independence of the tests' expected outcomes from the behaviour of the resolution.
// NOTE: the merge algebra furnished by the `MatchesContext` only tracks *incoming* moves, so any
// outgoing moves aren't included in test expectations; they only participate in the test setup. This
// is in contrast to noting edit and deletion migrations, where the migration has to picked up at the
// move source.
// TODO: perhaps migrations should be associated with the move *source* rather than the destination?
class MatchesContextTest:
  @TestFactory
  def ourMoveDestinationInsertion: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                     = 1
      val ourSideInsertedIncomingMoveDestinationElement: Element = 2

      val incomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = incomingMoveSourceElement,
              rightElement = ourSideInsertedIncomingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = incomingMoveSourceElement,
              leftElement = ourSideInsertedIncomingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          incomingMoveSourceElement -> incomingMovePairwiseMatch,
          ourSideInsertedIncomingMoveDestinationElement -> incomingMovePairwiseMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion
          .mergeAlgebra(
            coreMergeAlgebra = auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra
            .rightInsertion(
              mergeAlgebra.empty,
              insertedElement = ourSideInsertedIncomingMoveDestinationElement
            )
        else
          mergeAlgebra.leftInsertion(
            mergeAlgebra.empty,
            insertedElement = ourSideInsertedIncomingMoveDestinationElement
          )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(incomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set(ourSideInsertedIncomingMoveDestinationElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set(ourSideInsertedIncomingMoveDestinationElement),
            right = Set.empty,
            coincident = Set.empty
          )
        )
      end if
    }
  end ourMoveDestinationInsertion

  @TestFactory
  def ourMoveDestinationEdit: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                 = 1
      val baseElement: Element                               = 2
      val theirSideElement: Element                          = 3
      val ourSideEditIncomingMoveDestinationElement: Element = 4

      val incomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = incomingMoveSourceElement,
              rightElement = ourSideEditIncomingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = incomingMoveSourceElement,
              leftElement = ourSideEditIncomingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          incomingMoveSourceElement -> incomingMovePairwiseMatch,
          ourSideEditIncomingMoveDestinationElement -> incomingMovePairwiseMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra
            .rightEdit(
              mergeAlgebra.empty,
              editedBaseElement = baseElement,
              editedLeftElement = theirSideElement,
              editElements =
                IndexedSeq(ourSideEditIncomingMoveDestinationElement)
            )
        else
          mergeAlgebra.leftEdit(
            mergeAlgebra.empty,
            editedBaseElement = baseElement,
            editedRightElement = theirSideElement,
            editElements = IndexedSeq(ourSideEditIncomingMoveDestinationElement)
          )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(incomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set(ourSideEditIncomingMoveDestinationElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set(ourSideEditIncomingMoveDestinationElement),
            right = Set.empty,
            coincident = Set.empty
          )
        )
      end if
    }
  end ourMoveDestinationEdit

  @TestFactory
  def ourMoveDestinationEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                 = 1
      val baseOutgoingMoveSourceElement: Element             = 2
      val ourSideOutgoingMoveDestinationElement: Element     = 3
      val ourSideEditIncomingMoveDestinationElement: Element = 4

      val baseAndOurSideIncomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = incomingMoveSourceElement,
              rightElement = ourSideEditIncomingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = incomingMoveSourceElement,
              leftElement = ourSideEditIncomingMoveDestinationElement
            )

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          incomingMoveSourceElement -> baseAndOurSideIncomingMovePairwiseMatch,
          ourSideEditIncomingMoveDestinationElement -> baseAndOurSideIncomingMovePairwiseMatch,
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      val matchesContext = MatchesContext(
        matchesFor(matchesByElement)
      )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        matchesContext.MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements =
              IndexedSeq(ourSideEditIncomingMoveDestinationElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements =
              IndexedSeq(ourSideEditIncomingMoveDestinationElement),
            rightEditElements = IndexedSeq.empty
          )

      import matchesContext.*

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
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            RightInsertion(ourSideEditIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            LeftInsertion(ourSideEditIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditIncomingMoveDestinationElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSideIncomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set(ourSideEditIncomingMoveDestinationElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set(ourSideEditIncomingMoveDestinationElement),
            right = Set.empty,
            coincident = Set.empty
          )
        )
      end if
    }
  end ourMoveDestinationEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourMoveDestinationEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                 = 1
      val baseOutgoingMoveSourceElement: Element             = 2
      val ourSideOutgoingMoveDestinationElement: Element     = 3
      val ourSideEditIncomingMoveDestinationElement: Element = 4
      val theirSideEditElement: Element                      = 5

      val baseAndOurSideIncomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = incomingMoveSourceElement,
              rightElement = ourSideEditIncomingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = incomingMoveSourceElement,
              leftElement = ourSideEditIncomingMoveDestinationElement
            )

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          incomingMoveSourceElement -> baseAndOurSideIncomingMovePairwiseMatch,
          ourSideEditIncomingMoveDestinationElement -> baseAndOurSideIncomingMovePairwiseMatch
        )

      val matchesContext = MatchesContext(
        matchesFor(matchesByElement)
      )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        matchesContext.MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements =
              IndexedSeq(ourSideEditIncomingMoveDestinationElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements =
              IndexedSeq(ourSideEditIncomingMoveDestinationElement),
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      import matchesContext.*

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
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            RightInsertion(ourSideEditIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            LeftInsertion(ourSideEditIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditIncomingMoveDestinationElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(
          IndexedSeq(theirSideEditElement)
        ) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSideIncomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set(ourSideEditIncomingMoveDestinationElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set(ourSideEditIncomingMoveDestinationElement),
            right = Set.empty,
            coincident = Set.empty
          )
        )
      end if
    }
  end ourMoveDestinationEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentDeletionWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        mergeAlgebra.coincidentDeletion(
          mergeAlgebra.empty,
          deletedElement = baseOutgoingMoveSourceElement
        )

      assert(
        Vector(
          CoincidentDeletion(baseOutgoingMoveSourceElement)
        ) == mergeResult.coreMergeResult
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )
    }
  end coincidentDeletionWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val theirSideEditElement: Element                  = 3

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq.empty
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      assert(
        Vector(
          CoincidentDeletion(baseOutgoingMoveSourceElement)
        ) == mergeResult.coreMergeResult
      )

      assert(
        Set(
          IndexedSeq(theirSideEditElement)
        ) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )
    }
  end ourDeletionVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val ourSideEditElement: Element                    = 3

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      val matchesContext = MatchesContext(
        matchesFor(matchesByElement)
      )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        matchesContext.MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq.empty
          )

      import matchesContext.*

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
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            RightInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            LeftInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )
    }
  end ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val ourSideEditElement: Element                    = 3
      val theirSideEditElement: Element                  = 4

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      val matchesContext = MatchesContext(
        matchesFor(matchesByElement)
      )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        matchesContext.MergeResultDetectingMotion
          .mergeAlgebra(
            auditingCoreMergeAlgebra,
            guardedCoinFlippingResolution
          )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      import matchesContext.*

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
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            RightInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentDeletion(baseOutgoingMoveSourceElement),
            LeftInsertion(ourSideEditElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(
          IndexedSeq(theirSideEditElement)
        ) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )
    }
  end ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentEditWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val ourSideCoincidentEditElement: Element          = 3
      val theirSideCoincidentEditElement: Element        = 4

      val baseAndOurSideOutgoingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseOutgoingMoveSourceElement,
              rightElement = ourSideOutgoingMoveDestinationElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseOutgoingMoveSourceElement,
              leftElement = ourSideOutgoingMoveDestinationElement
            )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMovePairwiseMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            editedElement = baseOutgoingMoveSourceElement,
            editElements = IndexedSeq(
              theirSideCoincidentEditElement -> ourSideCoincidentEditElement
            )
          )
        else
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            editedElement = baseOutgoingMoveSourceElement,
            editElements = IndexedSeq(
              ourSideCoincidentEditElement -> theirSideCoincidentEditElement
            )
          )

      // The reasoning here is that a coincident edit, regardless of whether it
      // is part of a match or not, should stand as it is and not have one side
      // considered as an edit of the moved element.
      if mirrorImage then
        assert(
          Vector(
            CoincidentEdit(
              baseOutgoingMoveSourceElement,
              IndexedSeq(
                theirSideCoincidentEditElement -> ourSideCoincidentEditElement
              )
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentEdit(
              baseOutgoingMoveSourceElement,
              IndexedSeq(
                ourSideCoincidentEditElement -> theirSideCoincidentEditElement
              )
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideOutgoingMoveDestinationElement)
      )
    }
  end coincidentEditWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourSideDeletionWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    (Trials.api.booleans and Trials.api
      .integers(1, 10)
      .options
      .map(
        _.fold(ifEmpty = ResolutionOutcome.LeftChosen)(unrefined =>
          ResolutionOutcome.SomethingElseChosen(
            refineV[Positive].unsafeFrom(unrefined)
          )
        )
      )).withLimit(10).dynamicTests { (mirrorImage, resolutionOutcome) =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val theirSideElement: Element                      = 3

      val spuriousTheirSideElement: Element = 4

      val baseAndOurSideOutgoingMoveAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = theirSideElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = theirSideElement
          )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMoveAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMoveAllSidesMatch,
          theirSideElement -> baseAndOurSideOutgoingMoveAllSidesMatch
        )

      val spuriousAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = spuriousTheirSideElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = spuriousTheirSideElement
          )

      val spuriousMatchesByElement =
        Map(
          baseOutgoingMoveSourceElement         -> spuriousAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> spuriousAllSidesMatch,
          spuriousTheirSideElement              -> spuriousAllSidesMatch
        )

      given Eq[Element] = (lhs: Element, rhs: Element) =>
        matchesByElement.equivalent(lhs, rhs) || spuriousMatchesByElement
          .equivalent(lhs, rhs)

      val resolution = guardedStubResolution(resolutionOutcome, mirrorImage)

      val mergeAlgebra =
        MatchesContext((element: Element) =>
          matchesFor(matchesByElement)(element) union matchesFor(
            spuriousMatchesByElement
          )(element)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          resolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra
            .rightDeletion(
              mergeAlgebra.empty,
              deletedBaseElement = baseOutgoingMoveSourceElement,
              deletedLeftElement = theirSideElement
            )
        else
          mergeAlgebra
            .leftDeletion(
              mergeAlgebra.empty,
              deletedBaseElement = baseOutgoingMoveSourceElement,
              deletedRightElement = theirSideElement
            )

      if mirrorImage then
        assert(
          Vector(
            RightDeletion(baseOutgoingMoveSourceElement, theirSideElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            LeftDeletion(baseOutgoingMoveSourceElement, theirSideElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      resolutionOutcome match
        case ResolutionOutcome.LeftChosen =>
          assert(
            !mergeResult.changesMigratedThroughMotion
              .containsKey(ourSideOutgoingMoveDestinationElement)
          )
        case ResolutionOutcome.SomethingElseChosen(offset) =>
          if mirrorImage then
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseOutgoingMoveSourceElement),
                    theirSideElement,
                    ourSideOutgoingMoveDestinationElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideOutgoingMoveDestinationElement)
            )
          else
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseOutgoingMoveSourceElement),
                    ourSideOutgoingMoveDestinationElement,
                    theirSideElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideOutgoingMoveDestinationElement)
            )
          end if
      end match
    }
  end ourSideDeletionWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def ourSideEditWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    (Trials.api.booleans and Trials.api
      .integers(1, 10)
      .options
      .map(
        _.fold(ifEmpty = ResolutionOutcome.LeftChosen)(unrefined =>
          ResolutionOutcome.SomethingElseChosen(
            refineV[Positive].unsafeFrom(unrefined)
          )
        )
      )).withLimit(10).dynamicTests { (mirrorImage, resolutionOutcome) =>
      val baseOutgoingMoveSourceElement: Element         = 1
      val ourSideOutgoingMoveDestinationElement: Element = 2
      val ourSideEditElement: Element                    = 3
      val theirSideElement: Element                      = 4

      val spuriousTheirSideElement: Element = 5

      val baseAndOurSideOutgoingMoveAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = theirSideElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = theirSideElement
          )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndOurSideOutgoingMoveAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndOurSideOutgoingMoveAllSidesMatch,
          theirSideElement -> baseAndOurSideOutgoingMoveAllSidesMatch
        )

      val spuriousAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = spuriousTheirSideElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = spuriousTheirSideElement
          )

      val spuriousMatchesByElement =
        Map(
          baseOutgoingMoveSourceElement         -> spuriousAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> spuriousAllSidesMatch,
          spuriousTheirSideElement              -> spuriousAllSidesMatch
        )

      given Eq[Element] = (lhs: Element, rhs: Element) =>
        matchesByElement.equivalent(lhs, rhs) || spuriousMatchesByElement
          .equivalent(lhs, rhs)

      val resolution = guardedStubResolution(resolutionOutcome, mirrorImage)

      val mergeAlgebra =
        MatchesContext((element: Element) =>
          matchesFor(matchesByElement)(element) union matchesFor(
            spuriousMatchesByElement
          )(element)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          resolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.rightEdit(
            mergeAlgebra.empty,
            editedBaseElement = baseOutgoingMoveSourceElement,
            editedLeftElement = theirSideElement,
            editElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.leftEdit(
            mergeAlgebra.empty,
            editedBaseElement = baseOutgoingMoveSourceElement,
            editedRightElement = theirSideElement,
            editElements = IndexedSeq(ourSideEditElement)
          )

      if mirrorImage then
        assert(
          Vector(
            RightEdit(
              baseOutgoingMoveSourceElement,
              theirSideElement,
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            LeftEdit(
              baseOutgoingMoveSourceElement,
              theirSideElement,
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      resolutionOutcome match
        case ResolutionOutcome.LeftChosen =>
          assert(
            !mergeResult.changesMigratedThroughMotion
              .containsKey(ourSideOutgoingMoveDestinationElement)
          )
        case ResolutionOutcome.SomethingElseChosen(offset) =>
          if mirrorImage then
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseOutgoingMoveSourceElement),
                    theirSideElement,
                    ourSideOutgoingMoveDestinationElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideOutgoingMoveDestinationElement)
            )
          else
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseOutgoingMoveSourceElement),
                    ourSideOutgoingMoveDestinationElement,
                    theirSideElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideOutgoingMoveDestinationElement)
            )
          end if
      end match
    }
  end ourSideEditWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentDeletionWhereBaseHasMovedAwayOnBothSides: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element           = 1
      val ourSideOutgoingMoveDestinationElement: Element   = 2
      val theirSideOutgoingMoveDestinationElement: Element = 3

      val baseAndBothSidesOutgoingMoveAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = theirSideOutgoingMoveDestinationElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = theirSideOutgoingMoveDestinationElement
          )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          theirSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        mergeAlgebra.coincidentDeletion(
          mergeAlgebra.empty,
          deletedElement = baseOutgoingMoveSourceElement
        )

      assert(
        Vector(
          CoincidentDeletion(baseOutgoingMoveSourceElement)
        ) == mergeResult.coreMergeResult
      )

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideOutgoingMoveDestinationElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideOutgoingMoveDestinationElement)
      )
    }
  end coincidentDeletionWhereBaseHasMovedAwayOnBothSides

  @TestFactory
  def ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnBothSides
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element           = 1
      val ourSideOutgoingMoveDestinationElement: Element   = 2
      val theirSideOutgoingMoveDestinationElement: Element = 3
      val ourSideEditElement: Element                      = 4

      val baseAndBothSidesOutgoingMoveAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = theirSideOutgoingMoveDestinationElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = theirSideOutgoingMoveDestinationElement
          )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          theirSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq.empty,
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq.empty
          )

      if mirrorImage then
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseOutgoingMoveSourceElement),
              IndexedSeq.empty,
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseOutgoingMoveSourceElement),
              IndexedSeq(ourSideEditElement),
              IndexedSeq.empty
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideOutgoingMoveDestinationElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideOutgoingMoveDestinationElement)
      )
    }
  end ourEditVersusTheirDeletionConflictWhereBaseHasMovedAwayOnBothSides

  @TestFactory
  def ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnBothSides
      : DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseOutgoingMoveSourceElement: Element           = 1
      val ourSideOutgoingMoveDestinationElement: Element   = 2
      val theirSideOutgoingMoveDestinationElement: Element = 3
      val ourSideEditElement: Element                      = 4
      val theirSideEditElement: Element                    = 5

      val baseAndBothSidesOutgoingMoveAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = theirSideOutgoingMoveDestinationElement,
            rightElement = ourSideOutgoingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = baseOutgoingMoveSourceElement,
            leftElement = ourSideOutgoingMoveDestinationElement,
            rightElement = theirSideOutgoingMoveDestinationElement
          )

      val matchesByElement =
        Map(
          baseOutgoingMoveSourceElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          ourSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch,
          theirSideOutgoingMoveDestinationElement -> baseAndBothSidesOutgoingMoveAllSidesMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(theirSideEditElement),
            rightEditElements = IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.conflict(
            mergeAlgebra.empty,
            editedElements = IndexedSeq(baseOutgoingMoveSourceElement),
            leftEditElements = IndexedSeq(ourSideEditElement),
            rightEditElements = IndexedSeq(theirSideEditElement)
          )

      if mirrorImage then
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseOutgoingMoveSourceElement),
              IndexedSeq(theirSideEditElement),
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseOutgoingMoveSourceElement),
              IndexedSeq(ourSideEditElement),
              IndexedSeq(theirSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideOutgoingMoveDestinationElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideOutgoingMoveDestinationElement)
      )
    }
  end ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnBothSides

  @Test
  def leftInsertion: Unit =
    val ourSideInsertedElement: Element = 1

    val matchesByElement = Map.empty[Element, Match[Element]]

    val matchesContext = MatchesContext(matchesFor(matchesByElement))

    given Eq[Element] = matchesByElement.equivalent

    val mergeAlgebra =
      matchesContext.MergeResultDetectingMotion.mergeAlgebra(
        auditingCoreMergeAlgebra,
        guardedCoinFlippingResolution
      )

    val mergeResult =
      mergeAlgebra.leftInsertion(
        mergeAlgebra.empty,
        insertedElement = ourSideInsertedElement
      )

    import matchesContext.*

    assert(
      Seq(
        matchesContext.Insertion(Side.Left, ourSideInsertedElement)
      ) == mergeResult.insertions
    )
  end leftInsertion

  @Test
  def rightInsertion: Unit =
    val theirSideInsertedElement: Element = 1

    val matchesByElement = Map.empty[Element, Match[Element]]

    val matchesContext = MatchesContext(matchesFor(matchesByElement))

    given Eq[Element] = matchesByElement.equivalent

    val mergeAlgebra =
      matchesContext.MergeResultDetectingMotion.mergeAlgebra(
        auditingCoreMergeAlgebra,
        guardedCoinFlippingResolution
      )

    val mergeResult =
      mergeAlgebra.rightInsertion(
        mergeAlgebra.empty,
        insertedElement = theirSideInsertedElement
      )

    import matchesContext.*

    assert(
      Seq(
        matchesContext.Insertion(Side.Right, theirSideInsertedElement)
      ) == mergeResult.insertions
    )
  end rightInsertion

  @TestFactory
  def coincidentMoveDestinationInsertion: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                       = 1
      val ourSideInsertedIncomingMoveDestinationElement: Element   = 2
      val theirSideInsertedIncomingMoveDestinationElement: Element = 3

      val incomingMovesAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = incomingMoveSourceElement,
            leftElement = theirSideInsertedIncomingMoveDestinationElement,
            rightElement = ourSideInsertedIncomingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = incomingMoveSourceElement,
            leftElement = ourSideInsertedIncomingMoveDestinationElement,
            rightElement = theirSideInsertedIncomingMoveDestinationElement
          )

      val matchesByElement =
        Map(
          incomingMoveSourceElement -> incomingMovesAllSidesMatch,
          ourSideInsertedIncomingMoveDestinationElement -> incomingMovesAllSidesMatch,
          theirSideInsertedIncomingMoveDestinationElement -> incomingMovesAllSidesMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.coincidentInsertion(
            mergeAlgebra.empty,
            theirSideInsertedIncomingMoveDestinationElement,
            ourSideInsertedIncomingMoveDestinationElement
          )
        else
          mergeAlgebra.coincidentInsertion(
            mergeAlgebra.empty,
            ourSideInsertedIncomingMoveDestinationElement,
            theirSideInsertedIncomingMoveDestinationElement
          )

      if mirrorImage then
        assert(
          Vector(
            CoincidentInsertion(theirSideInsertedIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentInsertion(ourSideInsertedIncomingMoveDestinationElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideInsertedIncomingMoveDestinationElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideInsertedIncomingMoveDestinationElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(incomingMovesAllSidesMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(
              theirSideInsertedIncomingMoveDestinationElement -> ourSideInsertedIncomingMoveDestinationElement
            )
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(
              ourSideInsertedIncomingMoveDestinationElement -> theirSideInsertedIncomingMoveDestinationElement
            )
          )
        )
      end if
    }
  end coincidentMoveDestinationInsertion

  @TestFactory
  def coincidentMoveDestinationEdit: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val incomingMoveSourceElement: Element                   = 1
      val baseElement: Element                                 = 2
      val ourSideEditIncomingMoveDestinationElement: Element   = 3
      val theirSideEditIncomingMoveDestinationElement: Element = 4

      val incomingMovesAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = incomingMoveSourceElement,
            leftElement = theirSideEditIncomingMoveDestinationElement,
            rightElement = ourSideEditIncomingMoveDestinationElement
          )
        else
          Match.AllSides(
            baseElement = incomingMoveSourceElement,
            leftElement = ourSideEditIncomingMoveDestinationElement,
            rightElement = theirSideEditIncomingMoveDestinationElement
          )

      val matchesByElement =
        Map(
          incomingMoveSourceElement -> incomingMovesAllSidesMatch,
          ourSideEditIncomingMoveDestinationElement -> incomingMovesAllSidesMatch,
          theirSideEditIncomingMoveDestinationElement -> incomingMovesAllSidesMatch
        )

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        if mirrorImage then
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            editedElement = baseElement,
            editElements = IndexedSeq(
              theirSideEditIncomingMoveDestinationElement -> ourSideEditIncomingMoveDestinationElement
            )
          )
        else
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            editedElement = baseElement,
            editElements = IndexedSeq(
              ourSideEditIncomingMoveDestinationElement -> theirSideEditIncomingMoveDestinationElement
            )
          )

      if mirrorImage then
        assert(
          Vector(
            CoincidentEdit(
              baseElement,
              IndexedSeq(
                theirSideEditIncomingMoveDestinationElement -> ourSideEditIncomingMoveDestinationElement
              )
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentEdit(
              baseElement,
              IndexedSeq(
                ourSideEditIncomingMoveDestinationElement -> theirSideEditIncomingMoveDestinationElement
              )
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideEditIncomingMoveDestinationElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideEditIncomingMoveDestinationElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(incomingMovesAllSidesMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(
              theirSideEditIncomingMoveDestinationElement -> ourSideEditIncomingMoveDestinationElement
            )
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(incomingMoveSourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(
              ourSideEditIncomingMoveDestinationElement -> theirSideEditIncomingMoveDestinationElement
            )
          )
        )
      end if
    }
  end coincidentMoveDestinationEdit

end MatchesContextTest

object MatchesContextTest:
  type Element  = Int
  type Audit[X] = Vector[Operation[X]]

  def matchesFor(
      matchesByElement: Map[Element, Match[Element]]
  )(element: Element): Set[Match[Element]] = matchesByElement
    .get(element)
    .fold(ifEmpty = Set.empty[Match[Element]])(Set(_))

  def guardedStubResolution(
      resolutionOutcome: ResolutionOutcome,
      mirrorImage: Boolean
  )(using
      Eq[Element]
  ): Resolution[Element] = new StubResolution(resolutionOutcome, mirrorImage)
    with ResolutionContracts[Element] {}

  trait StubResolution(
      resolutionOutcome: ResolutionOutcome,
      mirrorImage: Boolean
  ) extends Resolution[Element]:
    override def apply(
        base: Option[Element],
        left: Element,
        right: Element
    ): Element =
      val mirroredLeft = if mirrorImage then right else left

      resolutionOutcome match
        case ResolutionOutcome.LeftChosen => mirroredLeft
        case SomethingElseChosen(offset)  => mirroredLeft + offset.value
      end match
    end apply
  end StubResolution

  enum Operation[X]:
    case Preservation(
        preservedBaseElement: X,
        preservedElementOnLeft: X,
        preservedElementOnRight: X
    )
    case LeftInsertion(inserted: X)
    case RightInsertion(inserted: X)
    case CoincidentInsertion(inserted: X)
    case LeftDeletion(deletedBaseElement: X, deletedRightElement: X)
    case RightDeletion(deletedBaseElement: X, deletedLeftElement: X)
    case CoincidentDeletion(deleted: X)
    case LeftEdit(
        editedBaseElement: X,
        editedRightElement: X,
        edits: IndexedSeq[X]
    )
    case RightEdit(
        editedBaseElement: X,
        editedLeftElement: X,
        edits: IndexedSeq[X]
    )
    case CoincidentEdit(edited: X, edits: IndexedSeq[(X, X)])
    case Conflict(
        edited: IndexedSeq[X],
        leftEdits: IndexedSeq[X],
        rightEdits: IndexedSeq[X]
    )

  end Operation

  enum ResolutionOutcome:
    case LeftChosen
    case SomethingElseChosen(offset: Int Refined Positive)
  end ResolutionOutcome

  object auditingCoreMergeAlgebra extends merge.MergeAlgebra[Audit, Element]:
    override def empty: Audit[Element] = Vector.empty
    override def preservation(
        result: Audit[Element],
        preservedBaseElement: Element,
        preservedElementOnLeft: Element,
        preservedElementOnRight: Element
    ): Audit[Element] = result :+ Preservation(
      preservedBaseElement,
      preservedElementOnLeft,
      preservedElementOnRight
    )
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
        insertedElementOnLeft: Element,
        insertedElementOnRight: Element
    ): Audit[Element] = result :+ CoincidentInsertion(insertedElementOnLeft)
    override def leftDeletion(
        result: Audit[Element],
        deletedBaseElement: Element,
        deletedRightElement: Element
    ): Audit[Element] =
      result :+ LeftDeletion(deletedBaseElement, deletedRightElement)
    override def rightDeletion(
        result: Audit[Element],
        deletedBaseElement: Element,
        deletedLeftElement: Element
    ): Audit[Element] =
      result :+ RightDeletion(deletedBaseElement, deletedLeftElement)
    override def coincidentDeletion(
        result: Audit[Element],
        deletedElement: Element
    ): Audit[Element] = result :+ CoincidentDeletion(deletedElement)
    override def leftEdit(
        result: Audit[Element],
        editedBaseElement: Element,
        editedRightElement: Element,
        editElements: IndexedSeq[Element]
    ): Audit[Element] =
      result :+ LeftEdit(editedBaseElement, editedRightElement, editElements)
    override def rightEdit(
        result: Audit[Element],
        editedBaseElement: Element,
        editedLeftElement: Element,
        editElements: IndexedSeq[Element]
    ): Audit[Element] =
      result :+ RightEdit(editedBaseElement, editedLeftElement, editElements)
    override def coincidentEdit(
        result: Audit[Element],
        editedElement: Element,
        editElements: IndexedSeq[(Element, Element)]
    ): Audit[Element] = result :+ CoincidentEdit(editedElement, editElements)
    override def conflict(
        result: Audit[Element],
        editedElements: IndexedSeq[Element],
        leftEditElements: IndexedSeq[Element],
        rightEditElements: IndexedSeq[Element]
    ): Audit[Element] =
      result :+ Conflict(editedElements, leftEditElements, rightEditElements)
  end auditingCoreMergeAlgebra
end MatchesContextTest
