package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution
import com.sageserpent.kineticmerge.core.Merge.Result.*

import scala.annotation.tailrec

object Merge:
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      equality: Eq[Element]
  ): Either[Divergence.type, Result[Element]] =
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
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
            addCommon(partialResult, leftElement)
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
                addCommon(partialResult, leftElement)
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
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            addCommon(partialResult, leftElement)
          )

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(baseElement),
                baseTail*
              ),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(
                Contribution.CommonToBaseAndRightOnly(rightSection),
                rightTail*
              )
            ) => // Left edit.
          leftTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element on the left would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                addCommon(partialResult, leftSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, leftSection)
              )
          end match

        case (
              Seq(
                Contribution.CommonToBaseAndRightOnly(baseSection),
                baseTail*
              ),
              _,
              Seq(
                Contribution.CommonToBaseAndRightOnly(rightElement),
                rightTail*
              )
            ) => // Left deletion.
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
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right edit.
          rightTail match
            case Seq(Contribution.Difference(_), _*) =>
              // If the following element on the right would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                addCommon(partialResult, rightSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                addCommon(partialResult, rightSection)
              )
          end match

        case (
              Seq(
                Contribution.CommonToBaseAndLeftOnly(baseSection),
                baseTail*
              ),
              Seq(
                Contribution.CommonToBaseAndLeftOnly(leftSection),
                leftTail*
              ),
              _
            ) => // Right deletion.
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
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              _
            ) => // Left edit / right deletion conflict.
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            addLeftEditConflictingWithRightDeletion(partialResult, leftSection)
          )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right insertion with pending left coincident edit.
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              _,
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right edit / left deletion conflict.
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            addRightEditConflictingWithLeftDeletion(partialResult, rightSection)
          )

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
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            addCommon(partialResult, leftElement)
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
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
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            addCommon(partialResult, rightElement)
          )

        case (Seq(), Seq(), Seq()) => // Terminating case!
          partialResult
      end match
    end mergeBetweenRunsOfCommonElements

    val longestCommonSubsequence =
      LongestCommonSubsequence.of(base, left, right)(equality)

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

  enum Result[Element]:
    case FullyMerged(elements: IndexedSeq[Element])
    case MergedWithConflicts(
        leftElements: IndexedSeq[Element],
        rightElements: IndexedSeq[Element]
    )
  end Result

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

end Merge
