package com.sageserpent.kineticmerge.core

import cats.Eq
import com.sageserpent.kineticmerge.core.LongestCommonSubsequence.Contribution

import scala.annotation.tailrec

object merge:
  def of[Element](
      base: IndexedSeq[Element],
      left: IndexedSeq[Element],
      right: IndexedSeq[Element]
  )(
      equality: Eq[Element]
  ): Either[Divergence.type, Result[Element]] =
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
            partialResult.addCommon(leftElement)
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
                partialResult.addCommon(leftElement)
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
            partialResult.addCommon(leftElement)
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
          baseTail -> leftTail match
            case (
                  Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                )
                // Guard against a coincident insertion prior to the right side
                // of a pending left edit or deletion; that would maroon the
                // latter, so we *would* coalesce the following element on the
                // left.
                if rightTail
                  .takeWhile {
                    case Contribution.CommonToBaseAndRightOnly(_) => false
                    case _                                        => true
                  }
                  .forall {
                    case Contribution.CommonToLeftAndRightOnly(_) => false
                    case _                                        => true
                  } =>
              // There is another pending left edit, so *don't* coalesce the
              // following element on the left; that then respects any
              // insertions that may be lurking on the right prior to the
              // pending left edit claiming the following element on the left.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommon(leftSection)
              )

            case (_, Seq(Contribution.Difference(_), _*)) =>
              // If the following element on the left would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, leftTail, right)(
                partialResult.addCommon(leftSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommon(leftSection)
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
          baseTail -> rightTail match
            case (
                  Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
                  Seq(Contribution.Difference(_), _*)
                )
                // Guard against a coincident insertion prior to the left side
                // of a pending right edit or deletion; that would maroon the
                // latter, so we *would* coalesce the following element on the
                // right.
                if leftTail
                  .takeWhile {
                    case Contribution.CommonToBaseAndLeftOnly(_) => false
                    case _                                       => true
                  }
                  .forall {
                    case Contribution.CommonToLeftAndRightOnly(_) => false
                    case _                                        => true
                  } =>
              // There is another pending right edit, so *don't* coalesce the
              // following element on the right; that then respects any
              // insertions that may be lurking on the left prior to the pending
              // right edit claiming the following element on the right.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommon(rightSection)
              )

            case (_, Seq(Contribution.Difference(_), _*)) =>
              // If the following element on the right would also be inserted,
              // coalesce into a single edit.
              mergeBetweenRunsOfCommonElements(base, left, rightTail)(
                partialResult.addCommon(rightSection)
              )

            case _ =>
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addCommon(rightSection)
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
                partialResult.addLeftEditConflictingWithRightDeletion(
                  leftElement
                )
              )

            case Seq(
                  Contribution.CommonToBaseAndRightOnly(_),
                  _*
                ) => // Right edit / left deletion conflict with pending left edit.
              mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
                partialResult.addRightEditConflictingWithLeftDeletion(
                  rightElement
                )
              )

            case _ =>
              // Edit conflict.
              mergeBetweenRunsOfCommonElements(baseTail, leftTail, rightTail)(
                partialResult.addConflictingEdits(leftElement, rightElement)
              )
          end match

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*)
            ) => // Left insertion with pending coincident edit.
          mergeBetweenRunsOfCommonElements(base, leftTail, right)(
            partialResult.addCommon(leftSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*)
            ) => // Coincident deletion with pending left edit.
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.Difference(_), baseTail*),
              Seq(Contribution.Difference(leftSection), leftTail*),
              _
            ) => // Left edit / right deletion conflict.
          mergeBetweenRunsOfCommonElements(baseTail, leftTail, right)(
            partialResult.addLeftEditConflictingWithRightDeletion(leftSection)
          )

        case (
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.CommonToLeftAndRightOnly(_), _*),
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right insertion with pending left coincident edit.
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult.addCommon(rightSection)
          )

        case (
              Seq(Contribution.Difference(_), baseTail*),
              Seq(Contribution.CommonToBaseAndLeftOnly(_), _*),
              Seq(Contribution.Difference(_), _*)
            ) => // Coincident deletion with pending left edit.
          mergeBetweenRunsOfCommonElements(baseTail, left, right)(partialResult)

        case (
              Seq(Contribution.Difference(_), baseTail*),
              _,
              Seq(Contribution.Difference(rightSection), rightTail*)
            ) => // Right edit / left deletion conflict.
          mergeBetweenRunsOfCommonElements(baseTail, left, rightTail)(
            partialResult.addRightEditConflictingWithLeftDeletion(rightSection)
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
            partialResult.addCommon(leftElement)
          )

        case (
              Seq(Contribution.CommonToBaseAndRightOnly(_), _*),
              Seq(Contribution.Difference(_), _*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Right insertion with pending left edit.
          mergeBetweenRunsOfCommonElements(base, left, rightTail)(
            partialResult.addCommon(rightElement)
          )

        case (
              _,
              Seq(Contribution.Difference(leftElement), leftTail*),
              Seq(Contribution.Difference(rightElement), rightTail*)
            ) => // Insertion conflict.
          mergeBetweenRunsOfCommonElements(base, leftTail, rightTail)(
            partialResult.addConflictingEdits(leftElement, rightElement)
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
            partialResult.addCommon(leftElement)
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
            partialResult.addCommon(rightElement)
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

  sealed trait Result[Element]:
    def addCommon(
        commonElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          FullyMerged(elements :+ commonElement)
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ commonElement,
            rightElements :+ commonElement
          )

    def addLeftEditConflictingWithRightDeletion(
        leftElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            leftElements = elements :+ leftElement,
            rightElements = elements
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements :+ leftElement, rightElements)

    def addRightEditConflictingWithLeftDeletion(
        rightElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            leftElements = elements,
            rightElements = elements :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(leftElements, rightElements :+ rightElement)

    def addConflictingEdits(
        leftElement: Element,
        rightElement: Element
    ): Result[Element] =
      this match
        case FullyMerged(elements) =>
          MergedWithConflicts(
            elements :+ leftElement,
            elements :+ rightElement
          )
        case MergedWithConflicts(leftElements, rightElements) =>
          MergedWithConflicts(
            leftElements :+ leftElement,
            rightElements :+ rightElement
          )
  end Result

  case class FullyMerged[Element](elements: IndexedSeq[Element])
      extends Result[Element]

  case class MergedWithConflicts[Element](
      leftElements: IndexedSeq[Element],
      rightElements: IndexedSeq[Element]
  ) extends Result[Element]

  // TODO: "Something went wrong!" - "What was it?"
  case object Divergence

end merge
