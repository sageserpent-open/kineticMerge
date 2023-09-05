package com.sageserpent.kineticmerge.core

import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.Merge.Result.*

import scala.annotation.tailrec

object Merge:
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      matchFor: Element => Option[Match[Element]]
  ): Either[Divergence.type, Result[Element]] =
    def equivalent(lhs: Element, rhs: Element) =
      matchFor(lhs) -> matchFor(rhs) match
        case (None, None)    => false
        case (None, Some(_)) => false
        case (Some(_), None) => false
        case (Some(lhsMatch), Some(rhsMatch)) =>
          lhsMatch.dominantElement == rhsMatch.dominantElement

    def addCommon(
        partialResult: Result[Element],
        commonElement: Element
    ): Result[Element] =
      partialResult match
        case FullyMerged(sections) =>
          FullyMerged(sections :+ commonElement)
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ commonElement,
            rightElements :+ commonElement
          )

    def addLeftEditConflictingWithRightDeletion(
        partialResult: Result[Element],
        leftElement: Element
    ): Result[Element] =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            leftElements = sections :+ leftElement,
            rightElements = sections
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements :+ leftElement, rightElements)

    def addRightEditConflictingWithLeftDeletion(
        partialResult: Result[Element],
        rightElement: Element
    ): Result[Element] =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            leftElements = sections,
            rightElements = sections :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements, rightElements :+ rightElement)

    def addConflictingEdits(
        partialResult: Result[Element],
        leftElement: Element,
        rightElement: Element
    ): Result[Element] =
      partialResult match
        case FullyMerged(sections) =>
          MergedWithConflicts(
            sections :+ leftElement,
            sections :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ leftElement,
            rightElements :+ rightElement
          )

    @tailrec
    def mergeBetweenRunsOfCommonElements(
        base: Seq[Contribution[Element]],
        left: Seq[Contribution[Element]],
        right: Seq[Contribution[Element]]
    )(partialResult: Result[Element]): Result[Element] =
      (base, left, right) match
        case (
              Seq(Contribution.Common(baseElement), baseTail*),
              Seq(Contribution.Common(leftElement), leftTail*),
              Seq(Contribution.Common(rightElement), rightTail*)
            ) => // Preservation.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseElement) = matchFor(baseElement): @unchecked

          assume(matchFor(leftElement).contains(matchForBaseElement))
          assume(matchFor(rightElement).contains(matchForBaseElement))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            addCommon(partialResult, matchForBaseElement.dominantElement)
          )

        case (
              Seq(Contribution.Difference(baseElement), baseTail*),
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Coincident edit.
          // Invariant - these must both belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForLeftElement) = matchFor(leftElement): @unchecked

          assume(matchFor(rightElement).contains(matchForLeftElement))

          // Invariant - the base section cannot be part of a match when it is
          // different.
          assume(matchFor(baseElement).isEmpty)

          baseTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element in the base would also be
              // coincidentally deleted, coalesce into a single coincident
              // edit.
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, matchForLeftElement.dominantElement)
              )
          end match

        case (
              _,
              Seq(
                Contribution.CommonToLeftAndRightOnly(leftElement),
                leftTail*
              ),
              Seq(
                Contribution.CommonToLeftAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Coincident insertion.
          // Invariant - these must both belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForLeftElement) = matchFor(leftElement): @unchecked

          assume(matchFor(rightElement).contains(matchForLeftElement))

          val dominantElement = matchForLeftElement.dominantElement

          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            addCommon(partialResult, dominantElement)
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(baseElement),
                baseTail*
              ),
              _,
              Seq(
                Contribution.CommonToBaseAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Left deletion.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseElement) = matchFor(baseElement): @unchecked

          assume(matchFor(rightElement).contains(matchForBaseElement))

          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult
          )

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(baseElement),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(leftElement),
                leftTail*
              ),
              _
            ) => // Right deletion.
          // Invariant - these must all belong to the same match; progress
          // through common elements is synchronized.
          val Some(matchForBaseElement) = matchFor(baseElement): @unchecked

          assume(matchFor(leftElement).contains(matchForBaseElement))

          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult
          )

        case (
              Seq(Contribution.Difference(baseElement), baseTail*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Conflict, multiple possibilities.
          baseTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element in the base would also be edited,
              // coalesce into a single coincident edit conflict.
              mergeBetweenRunsOfCommonElements(baseTail, left, right)(
                partialResult
              )

            case Seq(
                  Contribution.CommonToBaseAndLeftOnly(_),
                  _*
                ) => // Left edit / right deletion conflict with pending right edit.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
                addLeftEditConflictingWithRightDeletion(
                  partialResult,
                  leftElement
                )
              )

            case Seq(
                  Contribution.CommonToBaseAndRightOnly(_),
                  _*
                ) => // Right edit / left deletion conflict with pending left edit.
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                addRightEditConflictingWithLeftDeletion(
                  partialResult,
                  rightElement
                )
              )

            case _ =>
              // Edit conflict.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addConflictingEdits(partialResult, leftElement, rightElement)
              )
          end match

        case (
              Seq(Contribution.Difference(_), baseTail*),
              _,
              _
            ) => // Coincident deletion.
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(_), _*)
            ) => // Left insertion with pending right edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(leftElement).isEmpty)

          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftElement)
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(rightElement).isEmpty)

          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightElement)
          )

        case (
              _,
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Insertion conflict.
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            addConflictingEdits(partialResult, leftElement, rightElement)
          )

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndLeftOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndRightOnly(_),
                _*
              ) | Seq(),
              Seq(Contribution.Difference(leftElement), leftTail*),
              _
            ) => // Left insertion.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(leftElement).isEmpty)

          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftElement)
          )

        case (
              Seq(
                Contribution.Common(_) |
                Contribution.CommonToBaseAndRightOnly(_) |
                Contribution.CommonToLeftAndRightOnly(_) |
                Contribution.CommonToBaseAndLeftOnly(_),
                _*
              ) | Seq(),
              _,
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion.
          // Invariant - the left section cannot be part of a match when it is
          // different.
          assume(matchFor(rightElement).isEmpty)

          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightElement)
          )

        case (Seq(), Seq(), Seq()) => // Terminating case!
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)(equivalent _)

    val emptyResult: Result[Element] = FullyMerged(IndexedSeq.empty)

    // TODO: for now, failure is not tolerated, but obviously that will have to
    // be accommodated - both merge conflicts and later divergences.
    Right(
      mergeBetweenRunsOfCommonElements(
        longestCommonSubsequence.base,
        longestCommonSubsequence.left,
        longestCommonSubsequence.right
      )(emptyResult)
    )
  end of

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

  enum Result[Element]:
    case FullyMerged(elements: IndexedSeq[Element])
    case MergedWithConflicts(
        leftElements: IndexedSeq[Element],
        rightElements: IndexedSeq[Element]
    )
  end Result

end Merge
