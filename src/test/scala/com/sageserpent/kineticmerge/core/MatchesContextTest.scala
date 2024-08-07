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
class MatchesContextTest:
  @TestFactory
  def ourMoveDestinationInsertion: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val sourceElement: Element = 1

      val ourSideInsertedElement: Element = 2

      val baseAndOurSidePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = sourceElement,
              rightElement = ourSideInsertedElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = sourceElement,
              leftElement = ourSideInsertedElement
            )

      val matchesByElement =
        Map(
          sourceElement          -> baseAndOurSidePairwiseMatch,
          ourSideInsertedElement -> baseAndOurSidePairwiseMatch
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
            .rightInsertion(mergeAlgebra.empty, ourSideInsertedElement)
        else
          mergeAlgebra.leftInsertion(mergeAlgebra.empty, ourSideInsertedElement)

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSidePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set(ourSideInsertedElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set(ourSideInsertedElement),
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
      val sourceElement: Element = 1

      val baseElement: Element = 2

      val theirSideElement: Element = 3

      val ourSideEditElement: Element = 4

      val baseAndOurSidePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = sourceElement,
              rightElement = ourSideEditElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = sourceElement,
              leftElement = ourSideEditElement
            )

      val matchesByElement =
        Map(
          sourceElement      -> baseAndOurSidePairwiseMatch,
          ourSideEditElement -> baseAndOurSidePairwiseMatch
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
              baseElement,
              theirSideElement,
              IndexedSeq(ourSideEditElement)
            )
        else
          mergeAlgebra.leftEdit(
            mergeAlgebra.empty,
            baseElement,
            theirSideElement,
            IndexedSeq(ourSideEditElement)
          )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSidePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set(ourSideEditElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set(ourSideEditElement),
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
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val ourSideEditElement: Element  = 3
      val baseSourceElement: Element   = 4

      val baseAndOurSideOutgoingMovePairwiseMatch =
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

      val baseAndOurSideIncomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseSourceElement,
              rightElement = ourSideEditElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseSourceElement,
              leftElement = ourSideEditElement
            )

      val matchesByElement =
        Map(
          baseElement         -> baseAndOurSideOutgoingMovePairwiseMatch,
          ourSideMovedElement -> baseAndOurSideOutgoingMovePairwiseMatch,
          baseSourceElement   -> baseAndOurSideIncomingMovePairwiseMatch,
          ourSideEditElement  -> baseAndOurSideIncomingMovePairwiseMatch
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
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideMovedElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSideIncomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(baseSourceElement),
            left = Set.empty,
            right = Set(ourSideEditElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(baseSourceElement),
            left = Set(ourSideEditElement),
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
      val baseElement: Element          = 1
      val ourSideMovedElement: Element  = 2
      val ourSideEditElement: Element   = 3
      val theirSideEditElement: Element = 4
      val baseSourceElement: Element    = 5

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

      val baseAndOurSideIncomingMovePairwiseMatch =
        if mirrorImage then
          Match
            .BaseAndRight(
              baseElement = baseSourceElement,
              rightElement = ourSideEditElement
            )
        else
          Match
            .BaseAndLeft(
              baseElement = baseSourceElement,
              leftElement = ourSideEditElement
            )

      val matchesByElement =
        Map(
          baseElement         -> baseAndOurSidePairwiseMatch,
          ourSideMovedElement -> baseAndOurSidePairwiseMatch,
          baseSourceElement   -> baseAndOurSideIncomingMovePairwiseMatch,
          ourSideEditElement  -> baseAndOurSideIncomingMovePairwiseMatch
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
        ) == mergeResult.changesMigratedThroughMotion.get(ourSideMovedElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(baseAndOurSideIncomingMovePairwiseMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(baseSourceElement),
            left = Set.empty,
            right = Set(ourSideEditElement),
            coincident = Set.empty
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(baseSourceElement),
            left = Set(ourSideEditElement),
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
        mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

      assert(
        Vector(CoincidentDeletion(baseElement)) == mergeResult.coreMergeResult
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
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
          IndexedSeq(theirSideEditElement)
        ) == mergeResult.changesMigratedThroughMotion.get(ourSideMovedElement)
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
        Seq(
          Insertion(
            if mirrorImage then Side.Right else Side.Left,
            ourSideEditElement
          )
        ) == mergeResult.insertions
      )

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
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
        ) == mergeResult.changesMigratedThroughMotion.get(ourSideMovedElement)
      )
    }
  end ourEditVersusTheirEditConflictWhereBaseHasMovedAwayOnOurSide

  @TestFactory
  def coincidentEditWhereBaseHasMovedAwayOnOurSide: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val baseElement: Element                    = 1
      val ourSideMovedElement: Element            = 2
      val ourSideCoincidentEditElement: Element   = 3
      val theirSideCoincidentEditElement: Element = 4

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
            baseElement,
            IndexedSeq(
              theirSideCoincidentEditElement -> ourSideCoincidentEditElement
            )
          )
        else
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            baseElement,
            IndexedSeq(
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
              baseElement,
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
              baseElement,
              IndexedSeq(
                ourSideCoincidentEditElement -> theirSideCoincidentEditElement
              )
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        Set(IndexedSeq.empty) == mergeResult.changesMigratedThroughMotion
          .get(ourSideMovedElement)
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
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val theirSideElement: Element    = 3

      val spuriousTheirSideElement: Element = 4

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

      val spuriousAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = spuriousTheirSideElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = spuriousTheirSideElement
          )

      val spuriousMatchesByElement =
        Map(
          baseElement              -> spuriousAllSidesMatch,
          ourSideMovedElement      -> spuriousAllSidesMatch,
          spuriousTheirSideElement -> spuriousAllSidesMatch
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
            .rightDeletion(mergeAlgebra.empty, baseElement, theirSideElement)
        else
          mergeAlgebra
            .leftDeletion(mergeAlgebra.empty, baseElement, theirSideElement)

      if mirrorImage then
        assert(
          Vector(
            RightDeletion(baseElement, theirSideElement)
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            LeftDeletion(baseElement, theirSideElement)
          ) == mergeResult.coreMergeResult
        )
      end if

      resolutionOutcome match
        case ResolutionOutcome.LeftChosen =>
          assert(
            !mergeResult.changesMigratedThroughMotion
              .containsKey(ourSideMovedElement)
          )
        case ResolutionOutcome.SomethingElseChosen(offset) =>
          if mirrorImage then
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseElement),
                    theirSideElement,
                    ourSideMovedElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideMovedElement)
            )
          else
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseElement),
                    ourSideMovedElement,
                    theirSideElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideMovedElement)
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
      val baseElement: Element         = 1
      val ourSideMovedElement: Element = 2
      val ourSideEditElement: Element  = 3
      val theirSideElement: Element    = 4

      val spuriousTheirSideElement: Element = 5

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

      val spuriousAllSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = baseElement,
            leftElement = spuriousTheirSideElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = baseElement,
            leftElement = ourSideMovedElement,
            rightElement = spuriousTheirSideElement
          )

      val spuriousMatchesByElement =
        Map(
          baseElement              -> spuriousAllSidesMatch,
          ourSideMovedElement      -> spuriousAllSidesMatch,
          spuriousTheirSideElement -> spuriousAllSidesMatch
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
            baseElement,
            theirSideElement,
            IndexedSeq(ourSideEditElement)
          )
        else
          mergeAlgebra.leftEdit(
            mergeAlgebra.empty,
            baseElement,
            theirSideElement,
            IndexedSeq(ourSideEditElement)
          )

      if mirrorImage then
        assert(
          Vector(
            RightEdit(
              baseElement,
              theirSideElement,
              IndexedSeq(ourSideEditElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            LeftEdit(
              baseElement,
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
              .containsKey(ourSideMovedElement)
          )
        case ResolutionOutcome.SomethingElseChosen(offset) =>
          if mirrorImage then
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseElement),
                    theirSideElement,
                    ourSideMovedElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideMovedElement)
            )
          else
            assert(
              Set(
                IndexedSeq(
                  resolution(
                    Some(baseElement),
                    ourSideMovedElement,
                    theirSideElement
                  )
                )
              ) == mergeResult.changesMigratedThroughMotion
                .get(ourSideMovedElement)
            )
          end if
      end match
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

      given Eq[Element] = matchesByElement.equivalent

      val mergeAlgebra =
        MatchesContext(
          matchesFor(matchesByElement)
        ).MergeResultDetectingMotion.mergeAlgebra(
          auditingCoreMergeAlgebra,
          guardedCoinFlippingResolution
        )

      val mergeResult =
        mergeAlgebra.coincidentDeletion(mergeAlgebra.empty, baseElement)

      assert(
        Vector(CoincidentDeletion(baseElement)) == mergeResult.coreMergeResult
      )

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideMovedElement)
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

      if mirrorImage then
        assert(
          Vector(
            Conflict(
              IndexedSeq(baseElement),
              IndexedSeq.empty,
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
              IndexedSeq.empty
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideMovedElement)
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
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideMovedElement)
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
      mergeAlgebra.leftInsertion(mergeAlgebra.empty, ourSideInsertedElement)

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
      mergeAlgebra.rightInsertion(mergeAlgebra.empty, theirSideInsertedElement)

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
      val sourceElement: Element         = 1
      val ourSideMovedElement: Element   = 2
      val theirSideMovedElement: Element = 3

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = sourceElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = sourceElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          sourceElement         -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
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
            theirSideMovedElement,
            ourSideMovedElement
          )
        else
          mergeAlgebra.coincidentInsertion(
            mergeAlgebra.empty,
            ourSideMovedElement,
            theirSideMovedElement
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
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideMovedElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(allSidesMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(theirSideMovedElement -> ourSideMovedElement)
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(ourSideMovedElement -> theirSideMovedElement)
          )
        )
      end if
    }
  end coincidentMoveDestinationInsertion

  @TestFactory
  def coincidentMoveDestinationEdit: DynamicTests =
    Trials.api.booleans.withLimit(2).dynamicTests { mirrorImage =>
      val sourceElement: Element         = 1
      val baseElement: Element           = 2
      val ourSideMovedElement: Element   = 3
      val theirSideMovedElement: Element = 4

      val allSidesMatch =
        if mirrorImage then
          Match.AllSides(
            baseElement = sourceElement,
            leftElement = theirSideMovedElement,
            rightElement = ourSideMovedElement
          )
        else
          Match.AllSides(
            baseElement = sourceElement,
            leftElement = ourSideMovedElement,
            rightElement = theirSideMovedElement
          )

      val matchesByElement =
        Map(
          sourceElement         -> allSidesMatch,
          ourSideMovedElement   -> allSidesMatch,
          theirSideMovedElement -> allSidesMatch
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
            baseElement,
            IndexedSeq(theirSideMovedElement -> ourSideMovedElement)
          )
        else
          mergeAlgebra.coincidentEdit(
            mergeAlgebra.empty,
            baseElement,
            IndexedSeq(ourSideMovedElement -> theirSideMovedElement)
          )

      if mirrorImage then
        assert(
          Vector(
            CoincidentEdit(
              baseElement,
              IndexedSeq(theirSideMovedElement -> ourSideMovedElement)
            )
          ) == mergeResult.coreMergeResult
        )
      else
        assert(
          Vector(
            CoincidentEdit(
              baseElement,
              IndexedSeq(ourSideMovedElement -> theirSideMovedElement)
            )
          ) == mergeResult.coreMergeResult
        )
      end if

      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(ourSideMovedElement)
      )
      assert(
        !mergeResult.changesMigratedThroughMotion
          .containsKey(theirSideMovedElement)
      )

      val Seq((matches, moveDestinations)) =
        mergeResult.moveDestinationsReport.moveDestinationsByMatches.toSeq

      assert(matches == Set(allSidesMatch))

      if mirrorImage then
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(theirSideMovedElement -> ourSideMovedElement)
          )
        )
      else
        assert(
          moveDestinations == MoveDestinations(
            sources = Set(sourceElement),
            left = Set.empty,
            right = Set.empty,
            coincident = Set(ourSideMovedElement -> theirSideMovedElement)
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
